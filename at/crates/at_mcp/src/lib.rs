use std::cell::Cell;
use std::io::{BufRead, Write};
use std::rc::Rc;

use serde_json::{json, Value as JsonValue};

#[derive(Clone)]
pub struct McpServer {
    name: String,
    version: String,
    tools: Vec<Tool>,
    context: Option<String>,
    tool_handler: Option<Rc<ToolHandler>>,
    client_initialized: Cell<bool>,
}

pub type ToolHandler = dyn Fn(&str, &JsonValue) -> Result<JsonValue, String>;

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

impl McpServer {
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
            tools: Vec::new(),
            context: None,
            tool_handler: None,
            client_initialized: Cell::new(false),
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

    pub fn run<R: BufRead, W: Write>(&self, mut reader: R, mut writer: W) -> std::io::Result<()> {
        let mut line = String::new();
        loop {
            line.clear();
            if reader.read_line(&mut line)? == 0 {
                return Ok(());
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
                        // Silently ignore other notifications
                    }
                }
                continue;
            }

            let response = match method {
                "initialize" => {
                    let mut result = json!({
                        "jsonrpc": "2.0",
                        "id": id,
                        "result": {
                            "protocolVersion": "2024-11-05",
                            "serverInfo": {
                                "name": self.name,
                                "version": self.version,
                            },
                            "capabilities": {
                                "tools": {},
                                "roots": {},
                            }
                        }
                    });

                    // Only include instructions if context is present
                    if let Some(context) = &self.context {
                        result["result"]["instructions"] = json!(context);
                    }

                    result
                }
                "tools/list" => json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "tools": self.tools.iter().map(|tool| json!({
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
                            },
                            "outputSchema": tool.output_schema,
                        })).collect::<Vec<_>>(),
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
                                    "message": "Invalid params: missing required field 'name'"
                                }
                            })
                        } else {
                            let arguments = params
                                .get("arguments")
                                .cloned()
                                .unwrap_or_else(|| json!({}));

                            match (name, &self.tool_handler) {
                                (Some(name), Some(handler)) => match handler(name, &arguments) {
                                    Ok(result) => json!({
                                        "jsonrpc": "2.0",
                                        "id": id,
                                        "result": {
                                            "content": [
                                                {"type": "text", "text": result.to_string()}
                                            ],
                                            "isError": false
                                        }
                                    }),
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
                                },
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
