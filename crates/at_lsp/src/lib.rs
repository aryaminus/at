use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, OnceLock};
use std::time::SystemTime;

use at_lint;
use at_parser::{parse_module, ParseError};
use at_syntax::{Module, Span, TypeRef};
use at_vm::Compiler;
use lsp_server::{Connection, Message, Notification, Response};
use lsp_types::{
    CodeAction, CodeActionParams, CompletionItem, CompletionItemKind, CompletionOptions,
    CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity, DocumentHighlight,
    DocumentHighlightKind, DocumentHighlightParams, DocumentSymbol, DocumentSymbolParams,
    DocumentSymbolResponse, FoldingRange, FoldingRangeKind, FoldingRangeParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    InitializeResult, InlayHint, InlayHintKind, InlayHintParams, Location, MarkupContent,
    MarkupKind, ParameterInformation, ParameterLabel, Position, Range, SemanticToken,
    SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensParams, ServerCapabilities, SignatureHelp,
    SignatureHelpOptions, SignatureHelpParams, SignatureInformation, SymbolKind,
    TextDocumentContentChangeEvent, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit,
    Uri, WorkspaceFileOperationsServerCapabilities, WorkspaceFoldersServerCapabilities,
    WorkspaceServerCapabilities,
};
use sha2::{Digest, Sha256};

pub fn run_stdio() -> Result<(), String> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions::default()),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),
        code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
        }),
        document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
        document_highlight_provider: Some(lsp_types::OneOf::Left(true)),
        folding_range_provider: Some(lsp_types::FoldingRangeProviderCapability::Simple(true)),
        document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
        document_range_formatting_provider: Some(lsp_types::OneOf::Left(true)),
        semantic_tokens_provider: Some(
            lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                SemanticTokensOptions {
                    legend: semantic_tokens_legend(),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
                },
            ),
        ),
        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(lsp_types::OneOf::Left(true)),
            }),
            file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                did_create: Some(lsp_types::FileOperationRegistrationOptions {
                    filters: vec![lsp_types::FileOperationFilter {
                        scheme: Some("file".to_string()),
                        pattern: lsp_types::FileOperationPattern {
                            glob: "**/*.at".to_string(),
                            matches: None,
                            options: None,
                        },
                    }],
                }),
                did_rename: Some(lsp_types::FileOperationRegistrationOptions {
                    filters: vec![lsp_types::FileOperationFilter {
                        scheme: Some("file".to_string()),
                        pattern: lsp_types::FileOperationPattern {
                            glob: "**/*.at".to_string(),
                            matches: None,
                            options: None,
                        },
                    }],
                }),
                did_delete: Some(lsp_types::FileOperationRegistrationOptions {
                    filters: vec![lsp_types::FileOperationFilter {
                        scheme: Some("file".to_string()),
                        pattern: lsp_types::FileOperationPattern {
                            glob: "**/*.at".to_string(),
                            matches: None,
                            options: None,
                        },
                    }],
                }),
                will_create: None,
                will_rename: None,
                will_delete: None,
            }),
        }),
        ..ServerCapabilities::default()
    };

    let initialize_result = InitializeResult {
        capabilities: server_capabilities,
        server_info: None,
    };

    connection
        .initialize(serde_json::to_value(initialize_result).map_err(|err| err.to_string())?)
        .map_err(|err| err.to_string())?;

    let mut docs: HashMap<Uri, String> = HashMap::new();
    let mut module_cache: HashMap<String, CachedModule> = HashMap::new();
    let mut doc_cache: HashMap<Uri, DocCacheEntry> = HashMap::new();

    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection
                    .handle_shutdown(&request)
                    .map_err(|err| err.to_string())?
                {
                    break;
                }
                if let Some(response) =
                    handle_request(&docs, &mut doc_cache, &mut module_cache, &request)?
                {
                    connection
                        .sender
                        .send(Message::Response(response))
                        .map_err(|err| err.to_string())?;
                } else {
                    let response = Response::new_err(
                        request.id.clone(),
                        lsp_server::ErrorCode::MethodNotFound as i32,
                        "unknown request".to_string(),
                    );
                    connection
                        .sender
                        .send(Message::Response(response))
                        .map_err(|err| err.to_string())?;
                }
            }
            Message::Notification(notification) => {
                if let Some((url, text)) =
                    handle_notification(&mut docs, &mut doc_cache, &notification)?
                {
                    let module = get_cached_module(&text, &url, &mut doc_cache);
                    publish_diagnostics(&connection, &url, &text, module.as_ref())?;
                }
            }
            Message::Response(_) => {}
        }
    }

    io_threads.join().map_err(|err| err.to_string())?;
    Ok(())
}

fn handle_notification(
    docs: &mut HashMap<Uri, String>,
    doc_cache: &mut HashMap<Uri, DocCacheEntry>,
    notification: &Notification,
) -> Result<Option<(Uri, String)>, String> {
    match notification.method.as_str() {
        "textDocument/didOpen" => {
            let params: lsp_types::DidOpenTextDocumentParams =
                serde_json::from_value(notification.params.clone())
                    .map_err(|err| err.to_string())?;
            let url = params.text_document.uri;
            let text = params.text_document.text;
            docs.insert(url.clone(), text.clone());
            Ok(Some((url, text)))
        }
        "textDocument/didChange" => {
            let params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(notification.params.clone())
                    .map_err(|err| err.to_string())?;
            let url = params.text_document.uri;
            let text = merge_changes(docs.get(&url), params.content_changes);
            docs.insert(url.clone(), text.clone());
            Ok(Some((url, text)))
        }
        "textDocument/didSave" => {
            let params: lsp_types::DidSaveTextDocumentParams =
                serde_json::from_value(notification.params.clone())
                    .map_err(|err| err.to_string())?;
            let url = params.text_document.uri;
            let text = if let Some(text) = params.text {
                docs.insert(url.clone(), text.clone());
                text
            } else {
                docs.get(&url).cloned().unwrap_or_default()
            };
            Ok(Some((url, text)))
        }
        "textDocument/didClose" => {
            let params: lsp_types::DidCloseTextDocumentParams =
                serde_json::from_value(notification.params.clone())
                    .map_err(|err| err.to_string())?;
            let url = params.text_document.uri;
            docs.remove(&url);
            doc_cache.remove(&url);
            Ok(None)
        }
        _ => Ok(None),
    }
}

fn handle_request(
    docs: &HashMap<Uri, String>,
    doc_cache: &mut HashMap<Uri, DocCacheEntry>,
    module_cache: &mut HashMap<String, CachedModule>,
    request: &lsp_server::Request,
) -> Result<Option<Response>, String> {
    match request.method.as_str() {
        "textDocument/hover" => {
            let params: HoverParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params
                .text_document_position_params
                .text_document
                .uri
                .clone();
            let response = if let Some(text) = docs.get(&url) {
                let module = get_cached_module(text, &url, doc_cache);
                let hover = provide_hover(text, module.as_ref(), module_cache, &params);
                Response::new_ok(request.id.clone(), serde_json::to_value(hover).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/definition" => {
            let params: GotoDefinitionParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params
                .text_document_position_params
                .text_document
                .uri
                .clone();
            let response = if let Some(text) = docs.get(&url) {
                let module = get_cached_module(text, &url, doc_cache);
                let definition =
                    provide_definition(text, module.as_ref(), module_cache, &url, &params);
                Response::new_ok(
                    request.id.clone(),
                    serde_json::to_value(definition).unwrap(),
                )
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/completion" => {
            let params: CompletionParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document_position.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let module = get_cached_module(text, &url, doc_cache);
                let completion = provide_completion(
                    text,
                    module.as_ref(),
                    &url,
                    module_cache,
                    params.text_document_position.position,
                );
                Response::new_ok(
                    request.id.clone(),
                    serde_json::to_value(completion).unwrap(),
                )
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/inlayHint" => {
            let params: InlayHintParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let module = get_cached_module(text, &url, doc_cache);
                let hints = provide_inlay_hints(text, module.as_ref());
                Response::new_ok(request.id.clone(), serde_json::to_value(hints).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/codeAction" => {
            let params: lsp_types::CodeActionParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let actions = provide_code_actions(text, &url, &params);
                Response::new_ok(request.id.clone(), serde_json::to_value(actions).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/signatureHelp" => {
            let params: SignatureHelpParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params
                .text_document_position_params
                .text_document
                .uri
                .clone();
            let response = if let Some(text) = docs.get(&url) {
                let module = get_cached_module(text, &url, doc_cache);
                let help = provide_signature_help(text, module.as_ref(), module_cache, &params);
                Response::new_ok(request.id.clone(), serde_json::to_value(help).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/documentSymbol" => {
            let params: DocumentSymbolParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let module = get_cached_module(text, &url, doc_cache);
                let symbols = provide_document_symbols(text, module.as_ref());
                Response::new_ok(request.id.clone(), serde_json::to_value(symbols).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/semanticTokens/full" => {
            let params: SemanticTokensParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let tokens = provide_semantic_tokens(text);
                Response::new_ok(request.id.clone(), serde_json::to_value(tokens).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/documentHighlight" => {
            let params: DocumentHighlightParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params
                .text_document_position_params
                .text_document
                .uri
                .clone();
            let response = if let Some(text) = docs.get(&url) {
                let highlights = provide_document_highlights(text, &params);
                Response::new_ok(
                    request.id.clone(),
                    serde_json::to_value(highlights).unwrap(),
                )
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/foldingRange" => {
            let params: FoldingRangeParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let module = get_cached_module(text, &url, doc_cache);
                let ranges = provide_folding_ranges(text, module.as_ref());
                Response::new_ok(request.id.clone(), serde_json::to_value(ranges).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/formatting" => {
            let params: lsp_types::DocumentFormattingParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let edits = provide_formatting(text, None);
                Response::new_ok(request.id.clone(), serde_json::to_value(edits).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        "textDocument/rangeFormatting" => {
            let params: lsp_types::DocumentRangeFormattingParams =
                serde_json::from_value(request.params.clone()).map_err(|err| err.to_string())?;
            let url = params.text_document.uri.clone();
            let response = if let Some(text) = docs.get(&url) {
                let edits = provide_formatting(text, Some(params.range));
                Response::new_ok(request.id.clone(), serde_json::to_value(edits).unwrap())
            } else {
                Response::new_ok(request.id.clone(), serde_json::Value::Null)
            };
            Ok(Some(response))
        }
        _ => Ok(None),
    }
}

fn provide_hover(
    text: &str,
    module: Option<&Module>,
    module_cache: &mut HashMap<String, CachedModule>,
    params: &HoverParams,
) -> Option<Hover> {
    let position = params.text_document_position_params.position;
    let offset = position_to_offset(text, position);
    let (name, qualifier) = ident_at_offset_with_qualifier(text, offset)?;
    let functions = module
        .map(collect_functions_with_inferred)
        .or_else(|| collect_functions(text))?;
    if qualifier.is_none() {
        if let Some(info) = functions.get(&name) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::PlainText,
                    value: info.signature.clone(),
                }),
                range: Some(span_to_range(text, info.span)),
            });
        }
    }

    let imports = module
        .map(collect_imports_from_module)
        .or_else(|| collect_imports(text));
    let had_qualifier = qualifier.is_some();
    if let Some(qualifier) = qualifier {
        if let Some(imports) = imports.as_ref() {
            if let Some(info) = resolve_imported_function(
                imports,
                module_cache,
                &params.text_document_position_params.text_document.uri,
                &qualifier,
                &name,
            ) {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::PlainText,
                        value: info.signature,
                    }),
                    range: None,
                });
            }
        }
    }

    // Only try local function lookup if we had a qualifier (import resolution failed)
    // When no qualifier, we already checked at line 217 above
    if had_qualifier {
        if let Some(info) = functions.get(&name) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::PlainText,
                    value: info.signature.clone(),
                }),
                range: Some(span_to_range(text, info.span)),
            });
        }
    }

    let imports = imports?;
    let info = imports.get(&name)?;
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::PlainText,
            value: format!("import \"{}\" as {}", info.path, name),
        }),
        range: Some(span_to_range(text, info.span)),
    })
}

fn provide_signature_help(
    text: &str,
    module: Option<&Module>,
    module_cache: &mut HashMap<String, CachedModule>,
    params: &SignatureHelpParams,
) -> Option<SignatureHelp> {
    let position = params.text_document_position_params.position;
    let offset = position_to_offset(text, position);
    let (func_name, qualifier, active_param) = find_call_at_offset(text, offset)?;
    let signature = if let Some(qualifier) = qualifier {
        let imports = module.map(collect_imports_from_module)?;
        resolve_imported_function(
            &imports,
            module_cache,
            &params.text_document_position_params.text_document.uri,
            &qualifier,
            &func_name,
        )
        .map(|info| info.signature)
    } else {
        let functions = module
            .map(collect_functions_with_inferred)
            .or_else(|| collect_functions(text))?;
        functions.get(&func_name).map(|info| info.signature.clone())
    }?;

    let params_list = extract_signature_params(&signature);
    let parameters: Vec<ParameterInformation> = params_list
        .iter()
        .map(|param| ParameterInformation {
            label: ParameterLabel::Simple(param.clone()),
            documentation: None,
        })
        .collect();
    let sig_info = SignatureInformation {
        label: signature,
        documentation: None,
        parameters: Some(parameters),
        active_parameter: None,
    };
    Some(SignatureHelp {
        signatures: vec![sig_info],
        active_signature: Some(0),
        active_parameter: Some(active_param as u32),
    })
}

#[allow(deprecated)]
fn provide_document_symbols(text: &str, module: Option<&Module>) -> Option<DocumentSymbolResponse> {
    let owned_module;
    let module = if let Some(module) = module {
        module
    } else {
        owned_module = parse_module(text).ok()?;
        &owned_module
    };
    let mut symbols = Vec::new();
    for func in &module.functions {
        let range = span_to_range(text, func.name.span);
        let mut name = String::new();
        name.push_str("fn ");
        name.push_str(&func.name.name);
        let symbol = DocumentSymbol {
            name,
            detail: None,
            kind: SymbolKind::FUNCTION,
            range,
            selection_range: range,
            children: None,
            tags: None,
            deprecated: None,
        };
        symbols.push(symbol);
    }

    for stmt in &module.stmts {
        match stmt {
            at_syntax::Stmt::Struct { name, .. } => {
                let range = span_to_range(text, name.span);
                symbols.push(DocumentSymbol {
                    name: name.name.clone(),
                    detail: None,
                    kind: SymbolKind::STRUCT,
                    range,
                    selection_range: range,
                    children: None,
                    tags: None,
                    deprecated: None,
                });
            }
            at_syntax::Stmt::Enum { name, .. } => {
                let range = span_to_range(text, name.span);
                symbols.push(DocumentSymbol {
                    name: name.name.clone(),
                    detail: None,
                    kind: SymbolKind::ENUM,
                    range,
                    selection_range: range,
                    children: None,
                    tags: None,
                    deprecated: None,
                });
            }
            at_syntax::Stmt::TypeAlias { name, .. } => {
                let range = span_to_range(text, name.span);
                symbols.push(DocumentSymbol {
                    name: name.name.clone(),
                    detail: None,
                    kind: SymbolKind::TYPE_PARAMETER,
                    range,
                    selection_range: range,
                    children: None,
                    tags: None,
                    deprecated: None,
                });
            }
            at_syntax::Stmt::Test { name, .. } => {
                let range = Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                };
                symbols.push(DocumentSymbol {
                    name: format!("test {}", name),
                    detail: None,
                    kind: SymbolKind::METHOD,
                    range,
                    selection_range: range,
                    children: None,
                    tags: None,
                    deprecated: None,
                });
            }
            _ => {}
        }
    }
    Some(DocumentSymbolResponse::Nested(symbols))
}

fn semantic_tokens_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::KEYWORD,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::TYPE,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
        ],
        token_modifiers: Vec::new(),
    }
}

fn provide_semantic_tokens(text: &str) -> SemanticTokens {
    let legend = semantic_tokens_legend();
    let mut data: Vec<SemanticToken> = Vec::new();
    let mut last_line = 0u32;
    let mut last_char = 0u32;

    let mut push_token = |start: Position, length: u32, token_type: SemanticTokenType| {
        let line = start.line;
        let character = start.character;
        let delta_line = line - last_line;
        let delta_start = if delta_line == 0 {
            character.saturating_sub(last_char)
        } else {
            character
        };
        let token_index = legend
            .token_types
            .iter()
            .position(|t| *t == token_type)
            .unwrap_or(0) as u32;
        data.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token_index,
            token_modifiers_bitset: 0,
        });
        last_line = line;
        last_char = character;
    };

    let mut offset = 0usize;
    while offset < text.len() {
        let ch = text[offset..].chars().next().unwrap();
        if ch.is_whitespace() {
            offset += ch.len_utf8();
            continue;
        }

        if ch == '"' {
            let start = offset_to_position(text, offset);
            offset += 1;
            while offset < text.len() {
                let next = text[offset..].chars().next().unwrap();
                offset += next.len_utf8();
                if next == '"' {
                    break;
                }
            }
            let end = offset_to_position(text, offset);
            let length = end.character.saturating_sub(start.character);
            push_token(start, length, SemanticTokenType::STRING);
            continue;
        }

        if ch.is_ascii_digit() {
            let start = offset;
            offset += ch.len_utf8();
            while offset < text.len() {
                let next = text[offset..].chars().next().unwrap();
                if !next.is_ascii_digit() && next != '.' {
                    break;
                }
                offset += next.len_utf8();
            }
            let start_pos = offset_to_position(text, start);
            let end_pos = offset_to_position(text, offset);
            let length = end_pos.character.saturating_sub(start_pos.character);
            push_token(start_pos, length, SemanticTokenType::NUMBER);
            continue;
        }

        if ch.is_ascii_alphabetic() || ch == '_' {
            let start = offset;
            offset += ch.len_utf8();
            while offset < text.len() {
                let next = text[offset..].chars().next().unwrap();
                if !is_ident_continue(next) {
                    break;
                }
                offset += next.len_utf8();
            }
            let ident = &text[start..offset];
            let token_type = match ident {
                "fn" | "let" | "set" | "using" | "if" | "else" | "match" | "while" | "for"
                | "in" | "break" | "continue" | "return" | "import" | "as" | "struct" | "enum"
                | "type" | "test" | "true" | "false" | "needs" | "tool" => {
                    SemanticTokenType::KEYWORD
                }
                _ => SemanticTokenType::VARIABLE,
            };
            let start_pos = offset_to_position(text, start);
            let end_pos = offset_to_position(text, offset);
            let length = end_pos.character.saturating_sub(start_pos.character);
            push_token(start_pos, length, token_type);
            continue;
        }

        offset += ch.len_utf8();
    }

    SemanticTokens {
        result_id: None,
        data,
    }
}

fn provide_document_highlights(
    text: &str,
    params: &DocumentHighlightParams,
) -> Option<Vec<DocumentHighlight>> {
    let position = params.text_document_position_params.position;
    let offset = position_to_offset(text, position);
    let name = ident_at_offset(text, offset)?;
    let mut highlights = Vec::new();
    let mut cursor = 0usize;
    while cursor < text.len() {
        let remaining = &text[cursor..];
        if let Some(found) = remaining.find(&name) {
            let start = cursor + found;
            let end = start + name.len();
            let before = if start == 0 {
                None
            } else {
                text[..start].chars().next_back()
            };
            let after = if end >= text.len() {
                None
            } else {
                text[end..].chars().next()
            };
            let before_ok = before.map(|ch| !is_ident_continue(ch)).unwrap_or(true);
            let after_ok = after.map(|ch| !is_ident_continue(ch)).unwrap_or(true);
            if before_ok && after_ok {
                let range = Range {
                    start: offset_to_position(text, start),
                    end: offset_to_position(text, end),
                };
                highlights.push(DocumentHighlight {
                    range,
                    kind: Some(DocumentHighlightKind::TEXT),
                });
            }
            cursor = end;
        } else {
            break;
        }
    }
    Some(highlights)
}

fn provide_folding_ranges(text: &str, module: Option<&Module>) -> Option<Vec<FoldingRange>> {
    let owned_module;
    let module = if let Some(module) = module {
        module
    } else {
        owned_module = parse_module(text).ok()?;
        &owned_module
    };
    let mut ranges = Vec::new();
    for func in &module.functions {
        let range = span_to_range(text, func.name.span);
        if range.start.line < range.end.line {
            ranges.push(FoldingRange {
                start_line: range.start.line,
                start_character: Some(range.start.character),
                end_line: range.end.line,
                end_character: Some(range.end.character),
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            });
        }
    }
    for stmt in &module.stmts {
        if let at_syntax::Stmt::Block { stmts, .. } = stmt {
            if let (Some(first), Some(last)) = (stmts.first(), stmts.last()) {
                let start = stmt_span(first);
                let end = stmt_span(last);
                let range = Range {
                    start: offset_to_position(text, start.start),
                    end: offset_to_position(text, end.end),
                };
                if range.start.line < range.end.line {
                    ranges.push(FoldingRange {
                        start_line: range.start.line,
                        start_character: Some(range.start.character),
                        end_line: range.end.line,
                        end_character: Some(range.end.character),
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    });
                }
            }
        }
    }
    Some(ranges)
}

fn provide_formatting(text: &str, range: Option<Range>) -> Vec<TextEdit> {
    let module = match parse_module(text) {
        Ok(module) => module,
        Err(_) => return Vec::new(),
    };
    let formatted = at_fmt::format_module(&module);
    match range {
        Some(range) => {
            let start = position_to_offset(text, range.start);
            let end = position_to_offset(text, range.end);
            if start <= end && end <= text.len() {
                let new_text = formatted.clone();
                vec![TextEdit { range, new_text }]
            } else {
                Vec::new()
            }
        }
        None => vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: offset_to_position(text, text.len()),
            },
            new_text: formatted,
        }],
    }
}

fn provide_definition(
    text: &str,
    module: Option<&Module>,
    module_cache: &mut HashMap<String, CachedModule>,
    url: &Uri,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let position = params.text_document_position_params.position;
    let offset = position_to_offset(text, position);
    let (name, qualifier) = ident_at_offset_with_qualifier(text, offset)?;
    let functions = module
        .map(collect_functions_with_inferred)
        .or_else(|| collect_functions(text));
    if let Some(functions) = functions {
        if let Some(info) = functions.get(&name) {
            let location = Location {
                uri: url.clone(),
                range: span_to_range(text, info.span),
            };
            return Some(GotoDefinitionResponse::Scalar(location));
        }
    }

    let imports = module
        .map(collect_imports_from_module)
        .or_else(|| collect_imports(text));
    if let Some(qualifier) = qualifier {
        if let Some(imports) = imports.as_ref() {
            if let Some(info) =
                resolve_imported_function(imports, module_cache, url, &qualifier, &name)
            {
                let location = Location {
                    uri: info.uri,
                    range: info.range,
                };
                return Some(GotoDefinitionResponse::Scalar(location));
            }
        }
    }

    if let Some(imports) = imports {
        if let Some(info) = imports.get(&name) {
            let location = Location {
                uri: url.clone(),
                range: span_to_range(text, info.span),
            };
            return Some(GotoDefinitionResponse::Scalar(location));
        }
    }

    None
}

fn provide_completion(
    text: &str,
    module: Option<&Module>,
    url: &Uri,
    module_cache: &mut HashMap<String, CachedModule>,
    position: Position,
) -> CompletionResponse {
    let prefix = completion_prefix_at(text, position);
    let mut items = Vec::new();
    let mut seen = HashSet::new();
    let functions = module
        .map(collect_functions_with_inferred)
        .or_else(|| collect_functions(text));
    if let Some(functions) = functions {
        for (name, info) in functions {
            if !prefix_matches(&prefix, &name) {
                continue;
            }
            if seen.insert(name.clone()) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(info.signature),
                    insert_text: Some(format!("{}($0)", name)),
                    insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
                    ..CompletionItem::default()
                });
            }
        }
    }
    let imports = module
        .map(collect_imports_from_module)
        .or_else(|| collect_imports(text));
    if let Some(imports) = imports {
        for (alias, info) in imports {
            if prefix_matches(&prefix, &alias) && seen.insert(alias.clone()) {
                items.push(CompletionItem {
                    label: alias.clone(),
                    kind: Some(CompletionItemKind::MODULE),
                    detail: Some(format!("import \"{}\"", info.path)),
                    ..CompletionItem::default()
                });
            }
            if let Some(module) = load_module_cached(&info.path, url, module_cache) {
                for (name, signature) in module.functions {
                    let label = format!("{alias}.{name}");
                    if !prefix_matches(&prefix, &label) {
                        continue;
                    }
                    if seen.insert(label.clone()) {
                        items.push(CompletionItem {
                            label: label.clone(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(signature),
                            insert_text: Some(format!("{label}($0)")),
                            insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
                            ..CompletionItem::default()
                        });
                    }
                }
            }
        }
    }
    for builtin in builtin_completions() {
        if !prefix_matches(&prefix, &builtin.label) {
            continue;
        }
        if seen.insert(builtin.label.clone()) {
            items.push(builtin);
        }
    }
    CompletionResponse::Array(items)
}

fn provide_inlay_hints(text: &str, module: Option<&Module>) -> Vec<InlayHint> {
    let module = match module {
        Some(module) => module.clone(),
        None => match parse_module(text) {
            Ok(module) => module,
            Err(_) => return Vec::new(),
        },
    };
    let inferred_returns = at_check::infer_function_returns(&module);
    let mut hints = Vec::new();
    for func in module.functions {
        if func.return_ty.is_none() {
            if let Some(inferred) = inferred_returns.get(&func.name.name) {
                let label = format!("-> {}", inferred);
                let position = offset_to_position(text, func.name.span.end);
                hints.push(InlayHint {
                    position,
                    label: lsp_types::InlayHintLabel::String(label),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: Some(false),
                    data: None,
                });
            }
        }
    }
    hints
}

fn provide_code_actions(text: &str, url: &Uri, params: &CodeActionParams) -> Vec<CodeAction> {
    let module = match parse_module(text) {
        Ok(module) => module,
        Err(_) => return Vec::new(),
    };

    let mut actions = Vec::new();

    if let Err(lint_errors) = at_lint::lint_module(&module) {
        let start = params.range.start;
        let end = params.range.end;

        for error in lint_errors {
            if let Some(fix) = error.fix {
                let fix_start = offset_to_position(text, fix.span.start);
                let fix_end = offset_to_position(text, fix.span.end);

                if fix_start.line >= start.line
                    && fix_start.line <= end.line
                    && fix_end.line >= start.line
                    && fix_end.line <= end.line
                {
                    actions.push(CodeAction {
                        title: fix.description.clone(),
                        kind: Some(lsp_types::CodeActionKind::QUICKFIX),
                        diagnostics: None,
                        edit: Some(lsp_types::WorkspaceEdit {
                            changes: Some({
                                let mut map = HashMap::new();
                                map.insert(
                                    url.clone(),
                                    vec![lsp_types::TextEdit {
                                        range: Range {
                                            start: fix_start,
                                            end: fix_end,
                                        },
                                        new_text: fix.replacement,
                                    }],
                                );
                                map
                            }),
                            document_changes: None,
                            change_annotations: None,
                        }),
                        command: None,
                        is_preferred: None,
                        disabled: None,
                        data: None,
                    });
                }
            }
        }
    }

    actions
}

struct ImportInfo {
    path: String,
    span: Span,
}

struct ImportedFunctionInfo {
    signature: String,
    uri: Uri,
    range: Range,
}

#[derive(Clone)]
struct CachedModule {
    mtime: Option<SystemTime>,
    source: String,
    functions: HashMap<String, String>,
    spans: HashMap<String, Span>,
}

#[derive(Clone)]
struct DocCacheEntry {
    hash: u64,
    module: Module,
}

fn collect_imports(text: &str) -> Option<HashMap<String, ImportInfo>> {
    let module = parse_module(text).ok()?;
    Some(collect_imports_from_module(&module))
}

fn collect_imports_from_module(module: &Module) -> HashMap<String, ImportInfo> {
    let mut imports = HashMap::new();
    for stmt in &module.stmts {
        if let at_syntax::Stmt::Import { path, alias, .. } = stmt {
            imports.insert(
                alias.name.clone(),
                ImportInfo {
                    path: path.clone(),
                    span: alias.span,
                },
            );
        }
    }
    imports
}

fn resolve_imported_function(
    imports: &HashMap<String, ImportInfo>,
    module_cache: &mut HashMap<String, CachedModule>,
    base_uri: &Uri,
    alias: &str,
    name: &str,
) -> Option<ImportedFunctionInfo> {
    let info = imports.get(alias)?;
    let module = load_module_cached(&info.path, base_uri, module_cache)?;
    let signature = module.functions.get(name)?.clone();
    let span = module.spans.get(name).cloned()?;
    let resolved = resolve_import_path(base_uri, &info.path)?;
    Some(ImportedFunctionInfo {
        signature,
        uri: path_to_uri(&resolved)?,
        range: span_to_range(&module.source, span),
    })
}

fn load_module_cached(
    path: &str,
    base_uri: &Uri,
    cache: &mut HashMap<String, CachedModule>,
) -> Option<CachedModule> {
    let resolved = resolve_import_path(base_uri, path)?;
    let canonical = std::path::Path::new(&resolved).canonicalize().ok()?;
    let key = canonical.to_string_lossy().to_string();
    let metadata = std::fs::metadata(&canonical).ok();
    let mtime = metadata.and_then(|data| data.modified().ok());

    if let Some(entry) = cache.get(&key) {
        if entry.mtime == mtime {
            return Some(entry.clone());
        }
    }

    let source = std::fs::read_to_string(&canonical).ok()?;
    let module = parse_module(&source).ok()?;
    let info = build_cached_module(&module, source.clone(), mtime);
    cache.insert(key, info.clone());
    Some(info)
}

fn build_cached_module(module: &Module, source: String, mtime: Option<SystemTime>) -> CachedModule {
    let mut functions = HashMap::new();
    let mut spans = HashMap::new();
    let inferred = at_check::infer_function_returns(module);
    for func in &module.functions {
        let signature =
            format_function_signature_with_inferred(func, inferred.get(&func.name.name));
        functions.insert(func.name.name.clone(), signature);
        spans.insert(func.name.name.clone(), func.name.span);
    }
    CachedModule {
        mtime,
        source,
        functions,
        spans,
    }
}

fn get_cached_module(
    text: &str,
    uri: &Uri,
    cache: &mut HashMap<Uri, DocCacheEntry>,
) -> Option<Module> {
    let hash = hash_text(text);
    if let Some(entry) = cache.get(uri) {
        if entry.hash == hash {
            return Some(entry.module.clone());
        }
    }
    let module = parse_module(text).ok()?;
    cache.insert(
        uri.clone(),
        DocCacheEntry {
            hash,
            module: Module {
                id: module.id,
                functions: module.functions.clone(),
                stmts: module.stmts.clone(),
                comments: Vec::new(),
            },
        },
    );
    Some(module)
}

fn hash_text(text: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    text.hash(&mut hasher);
    hasher.finish()
}

fn resolve_import_path(base_uri: &Uri, path: &str) -> Option<String> {
    if path.starts_with("http://") || path.starts_with("https://") {
        return resolve_remote_import(path);
    }

    if path == "std" || path == "std.at" {
        let root = std::env::current_dir().ok()?;
        return Some(
            root.join("stdlib")
                .join("std.at")
                .to_string_lossy()
                .to_string(),
        );
    }

    let base_path = uri_to_path(base_uri)?;
    let base_dir = std::path::Path::new(&base_path).parent()?.to_path_buf();
    let candidate = std::path::Path::new(path);
    let resolved = if candidate.is_absolute() {
        candidate.to_path_buf()
    } else {
        base_dir.join(candidate)
    };
    Some(resolved.to_string_lossy().to_string())
}

fn uri_to_path(uri: &Uri) -> Option<String> {
    let value = uri.to_string();
    if let Some(stripped) = value.strip_prefix("file://") {
        // Percent-decode the path
        let mut result = String::new();
        let mut chars = stripped.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '%' {
                let hex1 = chars.next()?;
                let hex2 = chars.next()?;
                let hex_str = format!("{}{}", hex1, hex2);
                if let Ok(byte) = u8::from_str_radix(&hex_str, 16) {
                    result.push(byte as char);
                } else {
                    return None;
                }
            } else {
                result.push(ch);
            }
        }
        return Some(result);
    }
    None
}

fn resolve_remote_import(url: &str) -> Option<String> {
    let root = std::env::current_dir().ok()?;
    let lock_path = root.join(".at").join("lock");
    if !lock_path.exists() {
        if let Some(cached) = resolve_legacy_cached_path(&root, url) {
            return Some(cached);
        }
        return fetch_and_cache_remote(&root, url);
    }

    let contents = std::fs::read_to_string(lock_path).ok()?;
    for line in contents.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line.starts_with('#') {
            continue;
        }
        let mut parts = line.split_whitespace();
        let entry_url = parts.next()?;
        if entry_url == "version" {
            continue;
        }
        let hash = parts.next()?;
        if entry_url == url {
            let path = root.join(".at").join("cache").join(format!("{hash}.at"));
            if path.exists() {
                return Some(path.to_string_lossy().to_string());
            }
        }
    }
    if let Some(cached) = resolve_legacy_cached_path(&root, url) {
        return Some(cached);
    }
    enqueue_remote_fetch(root, url.to_string());
    None
}

fn resolve_legacy_cached_path(root: &std::path::Path, url: &str) -> Option<String> {
    let legacy = url
        .replace("https://", "")
        .replace("http://", "")
        .replace('/', "_")
        .replace(':', "_");
    let path = root.join(".at").join("cache").join(legacy);
    if path.exists() {
        return Some(path.to_string_lossy().to_string());
    }
    None
}

fn fetch_and_cache_remote(root: &std::path::Path, url: &str) -> Option<String> {
    let response = ureq::get(url).call().ok()?;
    let contents = response.into_string().ok()?;
    let hash = hash_contents(&contents);
    let cache_dir = root.join(".at").join("cache");
    std::fs::create_dir_all(&cache_dir).ok()?;
    let path = cache_dir.join(format!("{hash}.at"));
    std::fs::write(&path, contents).ok()?;
    Some(path.to_string_lossy().to_string())
}

fn enqueue_remote_fetch(root: std::path::PathBuf, url: String) {
    let inflight = remote_fetch_inflight();
    {
        let mut guard = inflight.lock().unwrap();
        if guard.contains(&url) {
            return;
        }
        guard.insert(url.clone());
    }
    std::thread::spawn(move || {
        let _ = fetch_and_cache_remote(&root, &url);
        let inflight = remote_fetch_inflight();
        let mut guard = inflight.lock().unwrap();
        guard.remove(&url);
    });
}

fn remote_fetch_inflight() -> &'static Mutex<HashSet<String>> {
    static INFLIGHT: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
    INFLIGHT.get_or_init(|| Mutex::new(HashSet::new()))
}

fn hash_contents(contents: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(contents.as_bytes());
    hex::encode(hasher.finalize())
}

fn path_to_uri(path: &str) -> Option<Uri> {
    let path = std::path::Path::new(path).canonicalize().ok()?;
    let path_str = path.to_string_lossy();

    // Percent-encode special characters
    let mut encoded = String::new();
    for ch in path_str.chars() {
        match ch {
            ' ' => encoded.push_str("%20"),
            '%' => encoded.push_str("%25"),
            '?' => encoded.push_str("%3F"),
            '#' => encoded.push_str("%23"),
            '[' => encoded.push_str("%5B"),
            ']' => encoded.push_str("%5D"),
            '@' => encoded.push_str("%40"),
            '!' => encoded.push_str("%21"),
            '$' => encoded.push_str("%24"),
            '&' => encoded.push_str("%26"),
            '\'' => encoded.push_str("%27"),
            '(' => encoded.push_str("%28"),
            ')' => encoded.push_str("%29"),
            '*' => encoded.push_str("%2A"),
            '+' => encoded.push_str("%2B"),
            ',' => encoded.push_str("%2C"),
            ';' => encoded.push_str("%3B"),
            '=' => encoded.push_str("%3D"),
            ch => encoded.push(ch),
        }
    }

    let url = format!("file://{}", encoded);
    url.parse().ok()
}

fn builtin_completions() -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for name in [
        "assert",
        "assert_eq",
        "some",
        "none",
        "ok",
        "err",
        "is_some",
        "is_none",
        "is_ok",
        "is_err",
    ] {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin".to_string()),
            insert_text: Some(format!("{}($0)", name)),
            insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
            ..CompletionItem::default()
        });
    }
    for name in ["time", "rng"] {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some("effect".to_string()),
            ..CompletionItem::default()
        });
    }
    items
}

fn completion_prefix_at(text: &str, position: Position) -> String {
    let mut offset = position_to_offset(text, position);
    if offset > text.len() {
        offset = text.len();
    }
    let mut start = offset;
    while start > 0 {
        let prev = text[..start].chars().next_back().unwrap();
        if !is_ident_continue(prev) {
            break;
        }
        start -= prev.len_utf8();
    }
    text[start..offset].to_string()
}

fn prefix_matches(prefix: &str, candidate: &str) -> bool {
    if prefix.is_empty() {
        return true;
    }
    candidate.starts_with(prefix)
}

fn merge_changes(
    existing: Option<&String>,
    changes: Vec<TextDocumentContentChangeEvent>,
) -> String {
    if let Some(change) = changes.into_iter().last() {
        return change.text;
    }
    existing.cloned().unwrap_or_default()
}

fn publish_diagnostics(
    connection: &Connection,
    url: &Uri,
    text: &str,
    module: Option<&Module>,
) -> Result<(), String> {
    let mut diagnostics = Vec::new();
    match module.cloned().or_else(|| parse_module(text).ok()) {
        Some(module) => {
            if let Err(errors) = at_check::typecheck_module(&module) {
                for error in errors {
                    diagnostics.push(type_error_to_diagnostic(text, error));
                }
            }
            if let Err(errors) = at_lint::lint_module(&module) {
                for error in errors {
                    diagnostics.push(lint_error_to_diagnostic(text, error));
                }
            }
            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile_module(&module) {
                diagnostics.extend(compile_error_to_diagnostic(text, err));
            }
        }
        None => {
            if let Err(err) = parse_module(text) {
                diagnostics.push(parse_error_to_diagnostic(text, err));
            }
        }
    };

    let params = lsp_types::PublishDiagnosticsParams {
        uri: url.clone(),
        diagnostics,
        version: None,
    };

    let notification = Notification::new("textDocument/publishDiagnostics".to_string(), params);
    connection
        .sender
        .send(Message::Notification(notification))
        .map_err(|err| err.to_string())?;
    Ok(())
}

fn parse_error_to_diagnostic(text: &str, err: ParseError) -> Diagnostic {
    let (message, span, code) = match err {
        ParseError::UnexpectedToken {
            expected,
            found,
            span,
        } => (
            format!("expected {expected}, found {found:?}"),
            span,
            "E001",
        ),
        ParseError::UnterminatedString { span } => {
            ("unterminated string".to_string(), span, "E002")
        }
        ParseError::UnterminatedBlockComment { span } => {
            ("unterminated block comment".to_string(), span, "E003")
        }
        ParseError::InvalidNumber { span } => ("invalid number".to_string(), span, "E004"),
    };

    let range = span_to_range(text, span);
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(lsp_types::NumberOrString::String(code.to_string())),
        code_description: None,
        source: Some("at".to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

fn type_error_to_diagnostic(text: &str, error: at_check::TypeError) -> Diagnostic {
    let range = error
        .span
        .map(|span| span_to_range(text, span))
        .unwrap_or(Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        });
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(lsp_types::NumberOrString::String("T001".to_string())),
        code_description: None,
        source: Some("at".to_string()),
        message: error.message,
        related_information: None,
        tags: None,
        data: None,
    }
}

fn lint_error_to_diagnostic(text: &str, error: at_lint::LintError) -> Diagnostic {
    let range = error
        .span
        .map(|span| span_to_range(text, span))
        .unwrap_or(Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        });
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::WARNING),
        code: Some(lsp_types::NumberOrString::String("L001".to_string())),
        code_description: None,
        source: Some("at".to_string()),
        message: error.message,
        related_information: None,
        tags: None,
        data: None,
    }
}

fn compile_error_to_diagnostic(text: &str, err: at_vm::VmError) -> Vec<Diagnostic> {
    match err {
        at_vm::VmError::Compile { message, span } => {
            let range = span.map(|span| span_to_range(text, span)).unwrap_or(Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            });
            vec![Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("at".to_string()),
                message,
                related_information: None,
                tags: None,
                data: None,
            }]
        }
        _ => Vec::new(),
    }
}

fn span_to_range(text: &str, span: Span) -> Range {
    let start = offset_to_position(text, span.start);
    let end = offset_to_position(text, span.end);
    Range { start, end }
}

fn expr_span(expr: &at_syntax::Expr) -> Option<Span> {
    match expr {
        at_syntax::Expr::Int(_, span, _)
        | at_syntax::Expr::Float(_, span, _)
        | at_syntax::Expr::String(_, span, _)
        | at_syntax::Expr::Bool(_, span, _) => Some(*span),
        at_syntax::Expr::Ident(ident) => Some(ident.span),
        at_syntax::Expr::Unary { expr, .. } => expr_span(expr),
        at_syntax::Expr::Binary { left, right, .. } => {
            let start = expr_span(left)?.start;
            let end = expr_span(right)?.end;
            Some(Span::new(start, end))
        }
        at_syntax::Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let start = expr_span(condition)?.start;
            let end = if let Some(else_branch) = else_branch {
                expr_span(else_branch)?.end
            } else {
                expr_span(then_branch)?.end
            };
            Some(Span::new(start, end))
        }
        at_syntax::Expr::Member { base, name, .. } => {
            let start = expr_span(base)?.start;
            let end = name.span.end;
            Some(Span::new(start, end))
        }
        at_syntax::Expr::Call { callee, args, .. } => {
            let start = expr_span(callee)?.start;
            let end = if let Some(last) = args.last() {
                expr_span(last)?.end
            } else {
                start
            };
            Some(Span::new(start, end))
        }
        at_syntax::Expr::Try(expr, _) => expr_span(expr),
        at_syntax::Expr::TryCatch { try_span, .. } => Some(*try_span),
        at_syntax::Expr::Match { match_span, .. } => Some(*match_span),
        at_syntax::Expr::Block { block_span, .. } => Some(*block_span),
        at_syntax::Expr::Array { array_span, .. } => Some(*array_span),
        at_syntax::Expr::Index { index_span, .. } => Some(*index_span),
        at_syntax::Expr::Tuple { tuple_span, .. } => Some(*tuple_span),
        at_syntax::Expr::Range { range_span, .. } => Some(*range_span),
        at_syntax::Expr::InterpolatedString { span, .. } => Some(*span),
        at_syntax::Expr::Closure { span, .. } => Some(*span),
        at_syntax::Expr::StructLiteral { span, .. } => Some(*span),
        at_syntax::Expr::EnumLiteral { span, .. } => Some(*span),
        at_syntax::Expr::MapLiteral { span, .. } => Some(*span),
        at_syntax::Expr::As { span, .. } => Some(*span),
        at_syntax::Expr::Is { span, .. } => Some(*span),
        at_syntax::Expr::Group { span, .. } => Some(*span),
    }
}

fn stmt_span(stmt: &at_syntax::Stmt) -> Span {
    match stmt {
        at_syntax::Stmt::Import { alias, .. } => alias.span,
        at_syntax::Stmt::TypeAlias { name, .. } => name.span,
        at_syntax::Stmt::Enum { name, .. } => name.span,
        at_syntax::Stmt::Struct { name, .. } => name.span,
        at_syntax::Stmt::Let { name, .. } => name.span,
        at_syntax::Stmt::Using { name, .. } => name.span,
        at_syntax::Stmt::Set { name, .. } => name.span,
        at_syntax::Stmt::SetMember { field, .. } => field.span,
        at_syntax::Stmt::SetIndex { base, .. } => expr_span(base).unwrap_or(Span::new(0, 0)),
        at_syntax::Stmt::While { while_span, .. } => *while_span,
        at_syntax::Stmt::For { for_span, .. } => *for_span,
        at_syntax::Stmt::Break { break_span, .. } => *break_span,
        at_syntax::Stmt::Continue { continue_span, .. } => *continue_span,
        at_syntax::Stmt::Expr { expr, .. } => expr_span(expr).unwrap_or(Span::new(0, 0)),
        at_syntax::Stmt::Return { expr, .. } => {
            expr.as_ref().and_then(expr_span).unwrap_or(Span::new(0, 0))
        }
        at_syntax::Stmt::Block { stmts, .. } => {
            if let (Some(first), Some(last)) = (stmts.first(), stmts.last()) {
                let start = stmt_span(first).start;
                let end = stmt_span(last).end;
                Span::new(start, end)
            } else {
                Span::new(0, 0)
            }
        }
        at_syntax::Stmt::Test { .. } => Span::new(0, 0),
    }
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;
    let mut count = 0usize;
    for ch in text.chars() {
        if count >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            // Count UTF-16 code units (surrogate pairs count as 2)
            character += ch.len_utf16() as u32;
        }
        count += ch.len_utf8();
    }
    Position { line, character }
}

fn position_to_offset(text: &str, position: Position) -> usize {
    let mut line = 0u32;
    let mut character = 0u32;
    let mut count = 0usize;
    for ch in text.chars() {
        if line == position.line && character == position.character {
            break;
        }
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += ch.len_utf16() as u32;
        }
        count += ch.len_utf8();
    }
    count
}

fn ident_at_offset(text: &str, offset: usize) -> Option<String> {
    if offset > text.len() {
        return None;
    }
    let mut start = offset;
    let mut end = offset;

    while start > 0 {
        let prev = text[..start].chars().next_back()?;
        if !is_ident_continue(prev) {
            break;
        }
        start -= prev.len_utf8();
    }

    while end < text.len() {
        let next = text[end..].chars().next()?;
        if !is_ident_continue(next) {
            break;
        }
        end += next.len_utf8();
    }

    if start == end {
        return None;
    }
    Some(text[start..end].to_string())
}

fn ident_at_offset_with_qualifier(text: &str, offset: usize) -> Option<(String, Option<String>)> {
    if offset > text.len() {
        return None;
    }
    let name = ident_at_offset(text, offset)?;
    if let Some((qualifier, _)) = split_qualified_ident(text, offset) {
        return Some((name, Some(qualifier)));
    }
    Some((name, None))
}

fn find_call_at_offset(text: &str, offset: usize) -> Option<(String, Option<String>, usize)> {
    let mut cursor = offset;
    let mut depth = 0i32;
    let mut saw_paren = false;
    let mut arg_index = 0usize;
    while cursor > 0 {
        let prev = text[..cursor].chars().next_back()?;
        if prev == ')' {
            depth += 1;
        } else if prev == '(' {
            if depth == 0 {
                saw_paren = true;
                break;
            }
            depth -= 1;
        } else if prev == ',' && depth == 0 {
            arg_index += 1;
        }
        cursor -= prev.len_utf8();
    }

    if !saw_paren {
        return None;
    }

    let mut name_end = cursor;
    while name_end > 0 {
        let prev = text[..name_end].chars().next_back()?;
        if prev.is_whitespace() {
            name_end -= prev.len_utf8();
            continue;
        }
        break;
    }

    let (name, qualifier) = ident_at_offset_with_qualifier(text, name_end)?;
    Some((name, qualifier, arg_index))
}

fn extract_signature_params(signature: &str) -> Vec<String> {
    let start = signature.find('(').map(|idx| idx + 1).unwrap_or(0);
    let end = signature.rfind(')').unwrap_or(signature.len());
    let inner = if start <= end {
        &signature[start..end]
    } else {
        ""
    };
    if inner.trim().is_empty() {
        return Vec::new();
    }
    inner
        .split(',')
        .map(|part| part.trim().to_string())
        .collect()
}

fn split_qualified_ident(text: &str, offset: usize) -> Option<(String, String)> {
    if offset == 0 {
        return None;
    }
    let mut cursor = offset;
    while cursor > 0 {
        let prev = text[..cursor].chars().next_back()?;
        if prev == '.' {
            break;
        }
        if !is_ident_continue(prev) {
            return None;
        }
        cursor -= prev.len_utf8();
    }
    if cursor == 0 {
        return None;
    }
    let before_dot = cursor - 1;
    let mut start = before_dot;
    while start > 0 {
        let prev = text[..start].chars().next_back()?;
        if !is_ident_continue(prev) {
            break;
        }
        start -= prev.len_utf8();
    }
    let qualifier = text[start..before_dot].to_string();
    let name = ident_at_offset(text, offset)?;
    Some((qualifier, name))
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

struct FunctionInfo {
    span: Span,
    signature: String,
}

fn collect_functions(text: &str) -> Option<HashMap<String, FunctionInfo>> {
    let module = parse_module(text).ok()?;
    Some(collect_functions_with_inferred(&module))
}

fn collect_functions_with_inferred(module: &Module) -> HashMap<String, FunctionInfo> {
    let inferred = at_check::infer_function_returns(module);
    let mut info = HashMap::new();
    for func in &module.functions {
        let signature =
            format_function_signature_with_inferred(func, inferred.get(&func.name.name));
        info.insert(
            func.name.name.clone(),
            FunctionInfo {
                span: func.name.span,
                signature,
            },
        );
    }
    info
}

fn format_function_signature_with_inferred(
    func: &at_syntax::Function,
    inferred_return: Option<&String>,
) -> String {
    let mut out = String::new();
    if func.is_tool {
        out.push_str("tool ");
    }
    out.push_str("fn ");
    out.push_str(&func.name.name);
    out.push('(');
    for (idx, param) in func.params.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&param.name.name);
        if let Some(ty) = &param.ty {
            out.push_str(": ");
            format_type_ref(ty, &mut out);
        }
    }
    out.push(')');
    if let Some(ty) = &func.return_ty {
        out.push_str(" -> ");
        format_type_ref(ty, &mut out);
    } else if let Some(inferred) = inferred_return {
        out.push_str(" -> ");
        out.push_str(inferred);
        out.push_str(" (inferred)");
    }
    out
}

fn format_type_ref(ty: &TypeRef, out: &mut String) {
    match ty {
        TypeRef::Named { name, args } => {
            out.push_str(&name.name);
            if !args.is_empty() {
                out.push('<');
                for (idx, arg) in args.iter().enumerate() {
                    if idx > 0 {
                        out.push_str(", ");
                    }
                    format_type_ref(arg, out);
                }
                out.push('>');
            }
        }
        TypeRef::Union { types } => {
            for (idx, ty) in types.iter().enumerate() {
                if idx > 0 {
                    out.push_str(" | ");
                }
                format_type_ref(ty, out);
            }
        }
        TypeRef::Intersection { types } => {
            for (idx, ty) in types.iter().enumerate() {
                if idx > 0 {
                    out.push_str(" & ");
                }
                format_type_ref(ty, out);
            }
        }
        TypeRef::Tuple { items, .. } => {
            out.push('(');
            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                format_type_ref(item, out);
            }
            out.push(')');
        }
        TypeRef::Function {
            params, return_ty, ..
        } => {
            out.push_str("fn(");
            for (idx, param) in params.iter().enumerate() {
                if idx > 0 {
                    out.push_str(", ");
                }
                format_type_ref(param, out);
            }
            out.push_str(") -> ");
            format_type_ref(return_ty, out);
        }
    }
}
