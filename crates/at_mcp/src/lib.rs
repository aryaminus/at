use std::cell::Cell;
use std::io::{BufRead, Write};
use std::rc::Rc;

use serde_json::{json, Value as JsonValue};

const MAX_REQUEST_LINE_BYTES: usize = 1024 * 1024;
const SUPPORTED_PROTOCOL_VERSION: &str = "2024-11-05";

#[derive(Clone)]
pub struct McpServer {
    name: String,
    version: String,
    tools: Vec<Tool>,
    context: Option<String>,
    tool_handler: Option<Rc<ToolHandler>>,
    resources: Vec<Resource>,
    prompts: Vec<Prompt>,
    client_initialized: Cell<bool>,
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
            client_initialized: Cell::new(false),
            completion_handler: None,
            sample_handler: None,
        }
    }

    pub fn with_tools(mut self, tools: Vec<Tool>) -> Self {
        self.tools = tools;
        self
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
        let notification = json!({
            "jsonrpc": "2.0",
            "method": "logging/message",
            "params": {
                "level": message.level,
                "message": message.message,
                "data": message.data,
            }
        });
        let response_line = serde_json::to_string(&notification).unwrap_or_default();
        writeln!(writer, "{}", response_line)?;
        writer.flush()?;
        Ok(())
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
                let response_line = serde_json::to_string(&response).unwrap_or_default();
                writeln!(writer, "{}", response_line)?;
                writer.flush()?;
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
                    let response_line = serde_json::to_string(&response).unwrap_or_default();
                    writeln!(writer, "{}", response_line)?;
                    writer.flush()?;
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
                let response_line = serde_json::to_string(&response).unwrap_or_default();
                writeln!(writer, "{}", response_line)?;
                writer.flush()?;
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
                        self.client_initialized.set(true);
                        eprintln!("[at-mcp] client initialized");
                    }
                    _ => {
                        eprintln!("[at-mcp] ignoring notification: {}", method);
                    }
                }
                continue;
            }

            let response = match method {
                "initialize" => {
                    if let Some(params) = message.get("params") {
                        if let Some(client_version) =
                            params.get("protocolVersion").and_then(|v| v.as_str())
                        {
                            if client_version != SUPPORTED_PROTOCOL_VERSION {
                                json!({
                                    "jsonrpc": "2.0",
                                    "id": id,
                                    "error": {
                                        "code": -32602,
                                        "message": format!(
                                            "Unsupported protocol version: {}",
                                            client_version
                                        ),
                                        "data": {
                                            "supportedVersions": [SUPPORTED_PROTOCOL_VERSION]
                                        }
                                    }
                                })
                            } else {
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
                        } else {
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
                    } else {
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
                }
                "resources/list" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "resources": self.resources.iter().map(|resource| json!({
                            "uri": resource.uri,
                            "name": resource.name,
                            "description": resource.description,
                            "mimeType": resource.mime_type,
                        })).collect::<Vec<_>>(),
                    }
                }),
                "resources/read" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    let uri = params.get("uri").and_then(|value| value.as_str());
                    if uri.is_none() {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": { "code": -32602, "message": "Invalid params: uri must be a string" }
                        })
                    } else {
                        let uri = uri.unwrap();
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
                    }
                }
                "prompts/list" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "prompts": self.prompts.iter().map(|prompt| json!({
                            "name": prompt.name,
                            "description": prompt.description,
                        })).collect::<Vec<_>>(),
                    }
                }),
                "prompts/get" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    let name = params.get("name").and_then(|value| value.as_str());
                    if name.is_none() {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": { "code": -32602, "message": "Invalid params: name must be a string" }
                        })
                    } else {
                        let name = name.unwrap();
                        let prompt = self.prompts.iter().find(|prompt| prompt.name == name);
                        if let Some(prompt) = prompt {
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "result": {
                                    "messages": prompt.messages,
                                }
                            })
                        } else {
                            json!({
                                "jsonrpc": "2.0",
                                "id": id,
                                "error": { "code": -32602, "message": "Unknown prompt" }
                            })
                        }
                    }
                }
                "logging/setLevel" => {
                    let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
                    let level = params.get("level").and_then(|value| value.as_str());
                    if level.is_none() {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "error": { "code": -32602, "message": "Invalid params: level must be a string" }
                        })
                    } else {
                        json!({
                            "jsonrpc": "2.0",
                            "id": id,
                            "result": {}
                        })
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
                                                } else if let Some(image) = result.get("image") {
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
                        let name = params.get("name").and_then(|value| value.as_str());
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
                                match (name, &self.completion_handler) {
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

            let response_line = serde_json::to_string(&response).unwrap_or_default();
            writeln!(writer, "{}", response_line)?;
            writer.flush()?;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::McpServer;
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
}
