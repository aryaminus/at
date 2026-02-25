use std::io::{BufRead, Write};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicU8, Ordering};
use std::sync::Arc;

use serde_json::{json, Value as JsonValue};

const MAX_REQUEST_LINE_BYTES: usize = 1024 * 1024;
const SUPPORTED_PROTOCOL_VERSION: &str = "2024-11-05";
const LOG_LEVEL_DEBUG: u8 = 10;
const LOG_LEVEL_INFO: u8 = 20;
const LOG_LEVEL_NOTICE: u8 = 30;
const LOG_LEVEL_WARNING: u8 = 40;
const LOG_LEVEL_ERROR: u8 = 50;
const LOG_LEVEL_CRITICAL: u8 = 60;
const LOG_LEVEL_ALERT: u8 = 70;
const LOG_LEVEL_EMERGENCY: u8 = 80;

#[derive(Clone)]
/// `Clone` is shallow: clones share handlers and runtime state via `Rc`/`Arc`.
pub struct McpServer {
    name: String,
    version: String,
    tools: Vec<Tool>,
    context: Option<String>,
    tool_handler: Option<Rc<ToolHandler>>,
    resources: Vec<Resource>,
    prompts: Vec<Prompt>,
    client_initialized: Arc<AtomicBool>,
    shutdown_requested: Arc<AtomicBool>,
    log_level: Arc<AtomicU8>,
    completion_handler: Option<Rc<CompletionHandler>>,
    sample_handler: Option<Rc<SampleHandler>>,
}

pub type ToolHandler = dyn Fn(&str, &JsonValue) -> Result<JsonValue, String>;
pub type CompletionHandler = dyn Fn(&str, &JsonValue) -> Result<Vec<String>, String>;
pub type SampleHandler = dyn Fn(&JsonValue) -> Result<JsonValue, String>;

#[derive(Debug, Clone)]
pub struct Tool {
    pub name: String,
    pub description: String,
    pub input_schema: JsonValue,
    pub needs: Vec<String>,
    pub output_schema: Option<JsonValue>,
    pub imports: Vec<String>,
    pub import_aliases: Vec<(String, String)>,
}

#[derive(Debug, Clone)]
pub struct Resource {
    pub uri: String,
    pub name: String,
    pub description: Option<String>,
    pub mime_type: Option<String>,
    pub text: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Prompt {
    pub name: String,
    pub description: Option<String>,
    pub messages: Vec<JsonValue>,
}

#[derive(Debug, Clone)]
pub struct LogMessage {
    pub level: String,
    pub message: String,
    pub data: Option<JsonValue>,
}

impl LogMessage {
    pub fn info(message: impl Into<String>) -> Self {
        Self {
            level: "info".to_string(),
            message: message.into(),
            data: None,
        }
    }

    pub fn warn(message: impl Into<String>) -> Self {
        Self {
            level: "warning".to_string(),
            message: message.into(),
            data: None,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self {
            level: "error".to_string(),
            message: message.into(),
            data: None,
        }
    }
}

impl McpServer {
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
            tools: Vec::new(),
            context: None,
            tool_handler: None,
            resources: Vec::new(),
            prompts: Vec::new(),
            client_initialized: Arc::new(AtomicBool::new(false)),
            shutdown_requested: Arc::new(AtomicBool::new(false)),
            log_level: Arc::new(AtomicU8::new(LOG_LEVEL_INFO)),
            completion_handler: None,
            sample_handler: None,
        }
    }

    pub fn with_tools(mut self, tools: Vec<Tool>) -> Self {
        self.tools = tools;
        self
    }

    fn validate_input_size(&self, value: &JsonValue) -> Result<(), String> {
        let encoded = serde_json::to_vec(value).map_err(|err| err.to_string())?;
        if encoded.len() > MAX_REQUEST_LINE_BYTES {
            return Err("Invalid Request: request too large".to_string());
        }
        Ok(())
    }

    pub fn with_context(mut self, context: Option<String>) -> Self {
        self.context = context;
        self
    }

    pub fn with_resources(mut self, resources: Vec<Resource>) -> Self {
        self.resources = resources;
        self
    }

    pub fn with_prompts(mut self, prompts: Vec<Prompt>) -> Self {
        self.prompts = prompts;
        self
    }

    pub fn notify_log_message<W: Write>(
        &self,
        mut writer: W,
        message: LogMessage,
    ) -> std::io::Result<()> {
        if !self.should_emit_log(&message.level) {
            return Ok(());
        }
        let notification = json!({
            "jsonrpc": "2.0",
            "method": "logging/message",
            "params": {
                "level": message.level,
                "message": message.message,
                "data": message.data,
            }
        });
        write_json_line(&mut writer, &notification)
    }

    pub fn run_stdio(&self) -> std::io::Result<()> {
        let stdin = std::io::stdin();
        let stdout = std::io::stdout();
        let reader = std::io::BufReader::new(stdin.lock());
        let writer = std::io::BufWriter::new(stdout.lock());
        self.run(reader, writer)
    }

    pub fn with_tool_handler<F>(mut self, handler: F) -> Self
    where
        F: Fn(&str, &JsonValue) -> Result<JsonValue, String> + 'static,
    {
        self.tool_handler = Some(Rc::new(handler));
        self
    }

    pub fn with_completion_handler<F>(mut self, handler: F) -> Self
    where
        F: Fn(&str, &JsonValue) -> Result<Vec<String>, String> + 'static,
    {
        self.completion_handler = Some(Rc::new(handler));
        self
    }

    pub fn with_sample_handler<F>(mut self, handler: F) -> Self
    where
        F: Fn(&JsonValue) -> Result<JsonValue, String> + 'static,
    {
        self.sample_handler = Some(Rc::new(handler));
        self
    }

    pub fn run<R: BufRead, W: Write>(&self, mut reader: R, mut writer: W) -> std::io::Result<()> {
        let mut line = String::new();
        loop {
            line.clear();
            if reader.read_line(&mut line)? == 0 {
                return Ok(());
            }
            if line.len() > MAX_REQUEST_LINE_BYTES {
                let response = json!({
                    "jsonrpc": "2.0",
                    "id": null,
                    "error": {
                        "code": -32600,
                        "message": "Invalid Request: request too large"
                    }
                });
                write_json_line(&mut writer, &response)?;
                continue;
            }
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            let message: serde_json::Value = match serde_json::from_str(trimmed) {
                Ok(message) => message,
                Err(_) => {
                    let response = json!({
                        "jsonrpc": "2.0",
                        "id": null,
                        "error": {
                            "code": -32700,
                            "message": "Parse error"
                        }
                    });
                    write_json_line(&mut writer, &response)?;
                    continue;
                }
            };

            // Validate JSON-RPC version
            if message.get("jsonrpc") != Some(&serde_json::Value::String("2.0".to_string())) {
                let response = json!({
                    "jsonrpc": "2.0",
                    "id": null,
                    "error": {
                        "code": -32600,
                        "message": "Invalid Request: missing or invalid jsonrpc field"
                    }
                });
                write_json_line(&mut writer, &response)?;
                continue;
            }

            let id = message.get("id").cloned();
            let method = message
                .get("method")
                .and_then(|value| value.as_str())
                .unwrap_or("");

            if id.is_none() {
                // Handle notifications (messages without id)
                match method {
                    "notifications/initialized" => {
                        self.client_initialized.store(true, Ordering::Relaxed);
                        eprintln!("[at-mcp] client initialized");
                    }
                    "exit" => return Ok(()),
                    _ => {
                        eprintln!("[at-mcp] ignoring notification: {}", method);
                    }
                }
                continue;
            }

            let response = match method {
                "shutdown" => {
                    self.shutdown_requested.store(true, Ordering::Relaxed);
                    json!({
                        "jsonrpc": "2.0",
                        "id": id,
                        "result": {}
                    })
                }
                _ if self.shutdown_requested.load(Ordering::Relaxed) => {
                    json!({
                        "jsonrpc": "2.0",
                        "id": id,
                        "error": {
                            "code": -32000,
                            "message": "Server has been shut down"
                        }
                    })
                }
                "initialize" => {
                    if let Some(params) = message.get("params") {
                        if let Some(client_version) =
                            params.get("protocolVersion").and_then(|v| v.as_str())
                        {
                            if client_version != SUPPORTED_PROTOCOL_VERSION {
                                eprintln!(
                                    "[at-mcp] unsupported protocol version: {}, using {}",
                                    client_version, SUPPORTED_PROTOCOL_VERSION
                                );
                            }
                        }
                    }
                    let mut result = json!({
                        "jsonrpc": "2.0",
                        "id": id,
                        "result": {
                            "protocolVersion": SUPPORTED_PROTOCOL_VERSION,
                            "serverInfo": {
                                "name": self.name,
                                "version": self.version,
                            },
                            "capabilities": {
                                "tools": {},
                                "roots": {},
                                "resources": {},
                                "prompts": {},
                                "completion": {},
                                "sampling": {},
                            }
                        }
                    });

                    // Only include instructions if context is present
                    if let Some(context) = &self.context {
                        result["result"]["instructions"] = json!(context);
                    }

                    result
                }
                "resources/list" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "resources": self.resources.iter().map(|resource| {
                            let mut value = json!({
                                "uri": resource.uri,
                                "name": resource.name,
                            });
                            if let Some(description) = &resource.description {
                                value["description"] = json!(description);
                            }
                            if let Some(mime_type) = &resource.mime_type {
                                value["mimeType"] = json!(mime_type);
                            }
                            value
                        }).collect::<Vec<_>>(),
                    }
                }),
                "resources/read" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    if let Some(uri) = params.get("uri").and_then(|value| value.as_str()) {
                        let resource = self.resources.iter().find(|res| res.uri == uri);
                        if let Some(resource) = resource {
                            let content = if let Some(text) = &resource.text {
                                json!({
                                    "type": "text",
                                    "text": text,
                                })
                            } else {
                                json!({
                                    "type": "text",
                                    "text": "",
                                })
                            };
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "result": {
                                    "contents": [content],
                                }
                            })
                        } else {
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "error": { "code": -32602, "message": "Unknown resource" }
                            })
                        }
                    } else {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": { "code": -32602, "message": "Invalid params: uri must be a string" }
                        })
                    }
                }
                "prompts/list" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "prompts": self.prompts.iter().map(|prompt| {
                            let mut value = json!({
                                "name": prompt.name,
                            });
                            if let Some(description) = &prompt.description {
                                value["description"] = json!(description);
                            }
                            value
                        }).collect::<Vec<_>>(),
                    }
                }),
                "prompts/get" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    if let Some(name) = params.get("name").and_then(|value| value.as_str()) {
                        let prompt = self.prompts.iter().find(|prompt| prompt.name == name);
                        if let Some(prompt) = prompt {
                            let mut result = json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "result": {
                                    "messages": prompt.messages,
                                }
                            });
                            if let Some(description) = &prompt.description {
                                result["result"]["description"] = json!(description);
                            }
                            result
                        } else {
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "error": { "code": -32602, "message": "Unknown prompt" }
                            })
                        }
                    } else {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": { "code": -32602, "message": "Invalid params: name must be a string" }
                        })
                    }
                }
                "logging/setLevel" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    let level = params.get("level").and_then(|value| value.as_str());
                    match level.and_then(parse_log_level) {
                        Some(level) => {
                            self.log_level.store(level, Ordering::Relaxed);
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "result": {}
                            })
                        }
                        None => json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": { "code": -32602, "message": "Invalid params: level must be one of debug|info|notice|warning|error|critical|alert|emergency" }
                        }),
                    }
                }
                "tools/list" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "tools": self.tools.iter().map(|tool| {
                            let mut value = json!({
                                "name": tool.name,
                                "description": tool.description,
                                "inputSchema": {
                                    "allOf": [
                                        tool.input_schema.clone(),
                                        {
                                            "type": "object",
                                            "properties": {
                                                "x-at-needs": {
                                                    "type": "array",
                                                    "items": { "type": "string" },
                                                    "default": tool.needs,
                                                }
                                                ,
                                                "x-at-imports": {
                                                    "type": "array",
                                                    "items": { "type": "string" },
                                                    "default": tool.imports,
                                                }
                                                ,
                                                "x-at-import-aliases": {
                                                    "type": "array",
                                                    "items": {
                                                        "type": "object",
                                                        "properties": {
                                                            "alias": { "type": "string" },
                                                            "path": { "type": "string" }
                                                        },
                                                        "required": ["alias", "path"],
                                                        "additionalProperties": false
                                                    },
                                                    "default": tool
                                                        .import_aliases
                                                        .iter()
                                                        .map(|(alias, path)| {
                                                            json!({
                                                                "alias": alias,
                                                                "path": path
                                                            })
                                                        })
                                                        .collect::<Vec<_>>()
                                                }
                                            },
                                            "additionalProperties": true
                                        }
                                    ]
                                }
                            });
                            if let Some(schema) = &tool.output_schema {
                                value["outputSchema"] = schema.clone();
                            }
                            value
                        }).collect::<Vec<_>>(),
                    }
                }),
                "tools/call" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);

                    // Validate params is an object
                    if !params.is_object() {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": {
                                "code": -32602,
                                "message": "Invalid params: expected object"
                            }
                        })
                    } else {
                        let params_size_error = self.validate_input_size(&params).err();
                        if let Some(message) = params_size_error {
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "error": {
                                    "code": -32600,
                                    "message": message
                                }
                            })
                        } else {
                            let name = params.get("name").and_then(|value| value.as_str());

                            // Validate name is present
                            if name.is_none() {
                                json!({
                                    "jsonrpc": "2.0",
                                    "id": id,
                                    "error": {
                                        "code": -32602,
                                        "message": "Invalid params: name must be a string"
                                    }
                                })
                            } else {
                                let arguments = params
                                    .get("arguments")
                                    .cloned()
                                    .unwrap_or_else(|| json!({}));
                                if !arguments.is_object() {
                                    json!({
                                        "jsonrpc": "2.0",
                                        "id": id,
                                        "error": {
                                            "code": -32602,
                                            "message": "Invalid params: arguments must be an object"
                                        }
                                    })
                                } else {
                                    match (name, &self.tool_handler) {
                                        (Some(name), Some(handler)) => {
                                            match handler(name, &arguments) {
                                                Ok(result) => {
                                                    if result
                                                        .get("content")
                                                        .and_then(|v| v.as_array())
                                                        .is_some()
                                                    {
                                                        let mut payload = result.clone();
                                                        if payload.get("isError").is_none() {
                                                            payload["isError"] = json!(false);
                                                        }
                                                        json!({
                                                            "jsonrpc": "2.0",
                                                            "id": id,
                                                            "result": payload
                                                        })
                                                    } else if let Some(image) = result.get("image")
                                                    {
                                                        let data = image
                                                            .get("data")
                                                            .cloned()
                                                            .unwrap_or(JsonValue::Null);
                                                        let mime_type = image
                                                            .get("mimeType")
                                                            .cloned()
                                                            .unwrap_or_else(|| json!("image/png"));
                                                        json!({
                                                            "jsonrpc": "2.0",
                                                            "id": id,
                                                            "result": {
                                                                "content": [
                                                                    {"type": "image", "data": data, "mimeType": mime_type}
                                                                ],
                                                                "isError": false
                                                            }
                                                        })
                                                    } else {
                                                        json!({
                                                            "jsonrpc": "2.0",
                                                            "id": id,
                                                            "result": {
                                                                "content": [
                                                                    {"type": "text", "text": result.to_string()}
                                                                ],
                                                                "isError": false
                                                            }
                                                        })
                                                    }
                                                }
                                                Err(message) => json!({
                                                    "jsonrpc": "2.0",
                                                    "id": id,
                                                    "result": {
                                                        "content": [
                                                            {"type": "text", "text": message}
                                                        ],
                                                        "isError": true
                                                    }
                                                }),
                                            }
                                        }
                                        _ => json!({
                                            "jsonrpc": "2.0",
                                            "id": id,
                                            "result": {
                                                "content": [
                                                    {"type": "text", "text": "unknown tool"}
                                                ],
                                                "isError": true
                                            }
                                        }),
                                    }
                                }
                            }
                        }
                    }
                }
                "completion/complete" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    if !params.is_object() {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": {
                                "code": -32602,
                                "message": "Invalid params: expected object"
                            }
                        })
                    } else {
                        let completion_ref = params
                            .get("ref")
                            .and_then(|value| {
                                value
                                    .as_object()
                                    .and_then(|obj| {
                                        obj.get("name")
                                            .or_else(|| obj.get("id"))
                                            .or_else(|| obj.get("uri"))
                                            .and_then(|entry| entry.as_str())
                                    })
                                    .or_else(|| value.as_str())
                            })
                            .or_else(|| params.get("name").and_then(|value| value.as_str()));
                        if completion_ref.is_none() {
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "error": {
                                    "code": -32602,
                                    "message": "Invalid params: expected ref.name (or legacy name) string"
                                }
                            })
                        } else {
                            let arguments = params
                                .get("arguments")
                                .or_else(|| params.get("argument"))
                                .cloned()
                                .unwrap_or_else(|| json!({}));
                            if !arguments.is_object() {
                                json!({
                                    "jsonrpc": "2.0",
                                    "id": id,
                                    "error": {
                                        "code": -32602,
                                        "message": "Invalid params: arguments must be an object"
                                    }
                                })
                            } else {
                                match (completion_ref, &self.completion_handler) {
                                    (Some(name), Some(handler)) => {
                                        match handler(name, &arguments) {
                                            Ok(items) => json!({
                                                "jsonrpc": "2.0",
                                                "id": id,
                                                "result": {
                                                    "items": items
                                                }
                                            }),
                                            Err(message) => json!({
                                                "jsonrpc": "2.0",
                                                "id": id,
                                                "error": { "code": -32000, "message": message }
                                            }),
                                        }
                                    }
                                    _ => json!({
                                        "jsonrpc": "2.0",
                                        "id": id,
                                        "result": { "items": [] }
                                    }),
                                }
                            }
                        }
                    }
                }
                "sampling/sample" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    if !params.is_object() {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": {
                                "code": -32602,
                                "message": "Invalid params: expected object"
                            }
                        })
                    } else if let Some(handler) = &self.sample_handler {
                        match handler(&params) {
                            Ok(result) => json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "result": result
                            }),
                            Err(message) => json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "error": { "code": -32000, "message": message }
                            }),
                        }
                    } else {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "result": {
                                "content": [
                                    {"type": "text", "text": "sampling not supported"}
                                ],
                                "isError": true
                            }
                        })
                    }
                }
                "ping" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {}
                }),
                "roots/list" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "roots": []
                    }
                }),
                _ => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "error": {
                        "code": -32601,
                        "message": "Method not found"
                    }
                }),
            };

            write_json_line(&mut writer, &response)?;
        }
    }
}

fn parse_log_level(level: &str) -> Option<u8> {
    match level.to_ascii_lowercase().as_str() {
        "debug" => Some(LOG_LEVEL_DEBUG),
        "info" => Some(LOG_LEVEL_INFO),
        "notice" => Some(LOG_LEVEL_NOTICE),
        "warning" | "warn" => Some(LOG_LEVEL_WARNING),
        "error" => Some(LOG_LEVEL_ERROR),
        "critical" => Some(LOG_LEVEL_CRITICAL),
        "alert" => Some(LOG_LEVEL_ALERT),
        "emergency" => Some(LOG_LEVEL_EMERGENCY),
        _ => None,
    }
}

fn write_json_line<W: Write>(writer: &mut W, value: &JsonValue) -> std::io::Result<()> {
    let line = serde_json::to_string(value).map_err(|err| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("failed to serialize JSON-RPC message: {err}"),
        )
    })?;
    writeln!(writer, "{line}")?;
    writer.flush()?;
    Ok(())
}

impl McpServer {
    fn should_emit_log(&self, level: &str) -> bool {
        let Some(message_level) = parse_log_level(level) else {
            return true;
        };
        message_level >= self.log_level.load(Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use super::{LogMessage, McpServer, Prompt, Resource};
    use serde_json::json;

    #[test]
    fn handles_tools_call() {
        let input = format!(
            "{}\n",
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {"name": "echo", "arguments": {"value": "ok"}}
            })
        );
        let server = McpServer::new("at", "0.1.0").with_tool_handler(|name, args| {
            if name != "echo" {
                return Err("unexpected tool".to_string());
            }
            Ok(args.clone())
        });
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(input.as_bytes()), &mut output)
            .expect("run server");
        let output = String::from_utf8(output).expect("utf8");
        let line = output.lines().next().expect("response line");
        let response: serde_json::Value = serde_json::from_str(line).expect("json");
        assert_eq!(response.get("id").and_then(|v| v.as_i64()), Some(1));
        let result = response.get("result").expect("result");
        let content = result
            .get("content")
            .and_then(|v| v.as_array())
            .expect("content");
        assert_eq!(
            content[0].get("type").and_then(|v| v.as_str()),
            Some("text")
        );
    }

    #[test]
    fn initialize_capabilities_are_consistent() {
        let input = format!(
            "{}\n{}\n",
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {"protocolVersion": "2024-11-05"}
            }),
            json!({
                "jsonrpc": "2.0",
                "id": 2,
                "method": "initialize"
            })
        );
        let server = McpServer::new("at", "0.1.0");
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(input.as_bytes()), &mut output)
            .expect("run server");
        let output = String::from_utf8(output).expect("utf8");
        let mut lines = output.lines();
        let first: serde_json::Value =
            serde_json::from_str(lines.next().expect("first response")).expect("json");
        let second: serde_json::Value =
            serde_json::from_str(lines.next().expect("second response")).expect("json");
        let first_caps = first
            .get("result")
            .and_then(|value| value.get("capabilities"))
            .cloned()
            .expect("first capabilities");
        let second_caps = second
            .get("result")
            .and_then(|value| value.get("capabilities"))
            .cloned()
            .expect("second capabilities");
        assert_eq!(first_caps, second_caps);
        assert!(first_caps.get("resources").is_some());
        assert!(first_caps.get("prompts").is_some());
    }

    #[test]
    fn completion_accepts_ref_object() {
        let input = format!(
            "{}\n",
            json!({
                "jsonrpc": "2.0",
                "id": 7,
                "method": "completion/complete",
                "params": {
                    "ref": {
                        "type": "ref/tool",
                        "name": "echo"
                    },
                    "argument": {
                        "name": "value",
                        "value": "hel"
                    }
                }
            })
        );
        let server = McpServer::new("at", "0.1.0").with_completion_handler(|name, args| {
            if name != "echo" {
                return Err("unexpected completion target".to_string());
            }
            if args.get("name").and_then(|value| value.as_str()) != Some("value") {
                return Err("missing argument payload".to_string());
            }
            Ok(vec!["hello".to_string(), "help".to_string()])
        });
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(input.as_bytes()), &mut output)
            .expect("run server");
        let output = String::from_utf8(output).expect("utf8");
        let line = output.lines().next().expect("response line");
        let response: serde_json::Value = serde_json::from_str(line).expect("json");
        let items = response
            .get("result")
            .and_then(|value| value.get("items"))
            .and_then(|value| value.as_array())
            .expect("completion items");
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_str(), Some("hello"));
    }

    #[test]
    fn resources_and_prompts_methods_work() {
        let input = format!(
            "{}\n{}\n{}\n{}\n",
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "resources/list"
            }),
            json!({
                "jsonrpc": "2.0",
                "id": 2,
                "method": "resources/read",
                "params": { "uri": "at://docs/readme" }
            }),
            json!({
                "jsonrpc": "2.0",
                "id": 3,
                "method": "prompts/list"
            }),
            json!({
                "jsonrpc": "2.0",
                "id": 4,
                "method": "prompts/get",
                "params": { "name": "example" }
            }),
        );
        let server = McpServer::new("at", "0.1.0")
            .with_resources(vec![Resource {
                uri: "at://docs/readme".to_string(),
                name: "README".to_string(),
                description: Some("Project docs".to_string()),
                mime_type: Some("text/plain".to_string()),
                text: Some("hello".to_string()),
            }])
            .with_prompts(vec![Prompt {
                name: "example".to_string(),
                description: Some("Sample prompt".to_string()),
                messages: vec![json!({"role": "user", "content": "hi"})],
            }]);
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(input.as_bytes()), &mut output)
            .expect("run server");

        let output = String::from_utf8(output).expect("utf8");
        let responses: Vec<serde_json::Value> = output
            .lines()
            .map(|line| serde_json::from_str(line).expect("json"))
            .collect();
        assert_eq!(responses.len(), 4);
        assert_eq!(
            responses[0]["result"]["resources"][0]["uri"].as_str(),
            Some("at://docs/readme")
        );
        assert_eq!(
            responses[1]["result"]["contents"][0]["text"].as_str(),
            Some("hello")
        );
        assert_eq!(
            responses[2]["result"]["prompts"][0]["name"].as_str(),
            Some("example")
        );
        assert_eq!(
            responses[3]["result"]["messages"][0]["content"].as_str(),
            Some("hi")
        );
    }

    #[test]
    fn logging_set_level_filters_log_notifications() {
        let request = format!(
            "{}\n",
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "logging/setLevel",
                "params": { "level": "warning" }
            })
        );
        let server = McpServer::new("at", "0.1.0");
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(request.as_bytes()), &mut output)
            .expect("run server");
        let response: serde_json::Value = serde_json::from_str(
            String::from_utf8(output)
                .expect("utf8")
                .lines()
                .next()
                .expect("response"),
        )
        .expect("json");
        assert!(response.get("result").is_some());

        let mut info_output = Vec::new();
        server
            .notify_log_message(&mut info_output, LogMessage::info("ignored"))
            .expect("log message");
        assert!(info_output.is_empty(), "info should be filtered at warning");

        let mut error_output = Vec::new();
        server
            .notify_log_message(&mut error_output, LogMessage::error("kept"))
            .expect("log message");
        let rendered = String::from_utf8(error_output).expect("utf8");
        assert!(
            rendered.contains("kept"),
            "expected error log message to be emitted"
        );
    }

    #[test]
    fn shutdown_then_exit_stops_server() {
        let input = format!(
            "{}\n{}\n{}\n",
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "shutdown"
            }),
            json!({
                "jsonrpc": "2.0",
                "method": "exit"
            }),
            json!({
                "jsonrpc": "2.0",
                "id": 2,
                "method": "ping"
            }),
        );
        let server = McpServer::new("at", "0.1.0");
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(input.as_bytes()), &mut output)
            .expect("run server");

        let rendered = String::from_utf8(output).expect("utf8");
        let lines: Vec<&str> = rendered.lines().collect();
        assert_eq!(lines.len(), 1, "server should stop processing after exit");
        let response: serde_json::Value = serde_json::from_str(lines[0]).expect("json");
        assert!(response.get("result").is_some());
    }

    #[test]
    fn malformed_input_returns_json_error_line() {
        let input = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"\n";
        let server = McpServer::new("at", "0.1.0");
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(input.as_bytes()), &mut output)
            .expect("run server");
        let output = String::from_utf8(output).expect("utf8");
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines.len(), 1);
        assert!(!lines[0].is_empty());
        let response: serde_json::Value = serde_json::from_str(lines[0]).expect("json");
        assert_eq!(response["error"]["code"].as_i64(), Some(-32700));
    }

    #[test]
    fn ping_and_roots_list_are_supported() {
        let input = format!(
            "{}\n{}\n",
            json!({
                "jsonrpc": "2.0",
                "id": 10,
                "method": "ping"
            }),
            json!({
                "jsonrpc": "2.0",
                "id": 11,
                "method": "roots/list"
            }),
        );
        let server = McpServer::new("at", "0.1.0");
        let mut output = Vec::new();
        server
            .run(std::io::Cursor::new(input.as_bytes()), &mut output)
            .expect("run server");
        let responses: Vec<serde_json::Value> = String::from_utf8(output)
            .expect("utf8")
            .lines()
            .map(|line| serde_json::from_str(line).expect("json"))
            .collect();
        assert_eq!(responses[0]["id"].as_i64(), Some(10));
        assert!(responses[0]["result"].is_object());
        assert_eq!(responses[1]["id"].as_i64(), Some(11));
        assert_eq!(
            responses[1]["result"]["roots"]
                .as_array()
                .expect("roots array")
                .len(),
            0
        );
    }
}
