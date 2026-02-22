use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use at_fmt::format_module;
use at_mcp::{McpServer, Tool};
use at_parser::parse_module;
use at_syntax::{Module, Stmt, TypeRef};
use at_vm::{format_value, Chunk, Compiler, Op, Program, Value, Vm};
use serde_json::{json, Value as JsonValue};
use sha2::{Digest, Sha256};

fn print_usage() {
    eprintln!("at - An agent-native programming language");
    eprintln!();
    eprintln!("Usage:");
    eprintln!("  at <file.at>                    Run a file");
    eprintln!("  at run <file.at>                Run a file");
    eprintln!("  at repl                         Start interactive REPL");
    eprintln!("  at fmt [--write] <file.at>      Format a file");
    eprintln!("  at test <file.at>               Run tests in a file");
    eprintln!("  at check <file.at>              Type-check a file");
    eprintln!("  at lint <file.at>               Lint a file");
    eprintln!("  at bench <file.at>              Benchmark a file");
    eprintln!("  at deps <file.at>               Show dependencies");
    eprintln!("  at lsp                          Start LSP server");
    eprintln!("  at mcp-server <file.at>         Start MCP server");
    eprintln!("  at cache <cmd>                  Cache management");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -h, --help     Show this help message");
    eprintln!("  -V, --version  Show version information");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // Handle --help and --version flags
    if args.len() > 1 && (args[1] == "--help" || args[1] == "-h") {
        print_usage();
        std::process::exit(0);
    }
    if args.len() > 1 && (args[1] == "--version" || args[1] == "-V" || args[1] == "-v") {
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        std::process::exit(0);
    }

    if args.len() < 2 {
        print_usage();
        std::process::exit(1);
    }

    let command = &args[1];
    if command == "--help" || command == "-h" {
        print_usage();
        std::process::exit(0);
    }
    if command == "--version" || command == "-V" || command == "-v" {
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        std::process::exit(0);
    }

    if args[1] == "mcp-server" {
        if args.len() > 2 {
            let path = &args[2];
            let loaded = match load_module(path) {
                Ok(module) => module,
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(1);
                }
            };
            let module = loaded.module;
            let imports = loaded.imports;
            let import_aliases = loaded.import_aliases;
            let program = match Compiler::new().compile_module(&module) {
                Ok(program) => program,
                Err(err) => {
                    eprintln!("{}", format_compile_error(&err, None, Some(path)));
                    std::process::exit(1);
                }
            };

            let mut tool_execs = HashMap::new();
            for (id, func) in module.functions.iter().enumerate() {
                if !func.is_tool {
                    continue;
                }
                let params = func
                    .params
                    .iter()
                    .map(|param| ToolParam {
                        name: param.name.name.clone(),
                        ty: param.ty.as_ref().map(format_type_ref),
                    })
                    .collect::<Vec<_>>();
                tool_execs.insert(
                    func.name.name.clone(),
                    ToolExec {
                        func_id: id,
                        params,
                        needs: func.needs.iter().map(|ident| ident.name.clone()).collect(),
                    },
                );
            }

            let program = std::sync::Arc::new(program);
            let tool_execs = std::sync::Arc::new(tool_execs);

            let tool_handler = move |name: &str, arguments: &JsonValue| {
                let exec = tool_execs
                    .get(name)
                    .ok_or_else(|| format!("unknown tool: {name}"))?;
                let args_obj = arguments
                    .as_object()
                    .ok_or_else(|| "tools/call expects arguments to be an object".to_string())?;
                let mut args = Vec::new();
                for param in &exec.params {
                    let value = args_obj
                        .get(&param.name)
                        .ok_or_else(|| format!("missing tool argument: {}", param.name))?;
                    args.push(json_to_value(value, param.ty.as_deref())?);
                }

                let mut chunk = Chunk::default();
                for need in &exec.needs {
                    chunk.code.push(Op::GrantCapability(need.clone()));
                    chunk.spans.push(None);
                }
                for value in args {
                    let index = chunk.constants.len();
                    chunk.constants.push(value);
                    chunk.code.push(Op::Const(index));
                    chunk.spans.push(None);
                }
                chunk.code.push(Op::Call(exec.func_id, exec.params.len()));
                chunk.spans.push(None);
                chunk.code.push(Op::Halt);
                chunk.spans.push(None);

                let call_program = Program {
                    functions: program.functions.clone(),
                    main: chunk,
                    main_locals: 0,
                };
                let mut vm = Vm::new();
                let value = vm
                    .run(&call_program)
                    .map_err(|err| format!("tool runtime error: {err:?}"))?;
                Ok(value.map(value_to_json).unwrap_or(JsonValue::Null))
            };

            let tools = module
                .functions
                .iter()
                .filter(|func| func.is_tool)
                .map(|func| {
                    let needs = if func.needs.is_empty() {
                        String::new()
                    } else {
                        format!(
                            "needs: {}",
                            func.needs
                                .iter()
                                .map(|ident| ident.name.as_str())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    };
                    let output_schema = func.return_ty.as_ref().map(type_ref_to_schema);
                    Tool {
                        name: func.name.name.clone(),
                        description: needs,
                        input_schema: json!({
                            "type": "object",
                            "properties": func
                                .params
                                .iter()
                                .map(|param| {
                                    let schema = param
                                        .ty
                                        .as_ref()
                                        .map(type_ref_to_schema)
                                        .unwrap_or_else(|| json!({ "type": "string" }));
                                    (param.name.name.clone(), schema)
                                })
                                .collect::<serde_json::Map<_, _>>(),
                            "required": func
                                .params
                                .iter()
                                .map(|param| param.name.name.clone())
                                .collect::<Vec<_>>(),
                            "additionalProperties": false,
                        }),
                        needs: func.needs.iter().map(|ident| ident.name.clone()).collect(),
                        output_schema,
                        imports: imports.clone(),
                        import_aliases: import_aliases.clone(),
                    }
                })
                .collect::<Vec<_>>();

            let context = {
                let base = std::path::Path::new(&args[2]);
                let dir = base.parent().unwrap_or_else(|| std::path::Path::new("."));
                let context_path = dir.join("AT.md");
                fs::read_to_string(context_path).ok()
            };

            let server = McpServer::new("at", env!("CARGO_PKG_VERSION"))
                .with_tools(tools)
                .with_context(context)
                .with_tool_handler(tool_handler);
            if let Err(err) = server.run_stdio() {
                eprintln!("mcp server error: {err}");
                std::process::exit(1);
            }
            return;
        } else {
            let context = None;
            let server = McpServer::new("at", env!("CARGO_PKG_VERSION"))
                .with_tools(Vec::new())
                .with_context(context);
            if let Err(err) = server.run_stdio() {
                eprintln!("mcp server error: {err}");
                std::process::exit(1);
            }
            return;
        }
    }

    if args[1] == "lsp" {
        if let Err(err) = at_lsp::run_stdio() {
            eprintln!("lsp server error: {err}");
            std::process::exit(1);
        }
        return;
    }

    if args[1] == "run" {
        if args.len() < 3 {
            eprintln!("usage: at run <file.at>");
            std::process::exit(1);
        }
        run_file(&args[2], &args[3..]);
        return;
    }

    if args[1] == "fmt" {
        if args.len() < 3 {
            eprintln!("usage: at fmt [--write] <file.at>");
            std::process::exit(1);
        }
        let mut path_index = 2;
        let mut write = false;
        if args.get(2).map(|arg| arg.as_str()) == Some("--write") {
            write = true;
            path_index = 3;
        }
        if args.len() <= path_index {
            eprintln!("usage: at fmt [--write] <file.at>");
            std::process::exit(1);
        }
        let path = &args[path_index];
        let module = match load_module(path) {
            Ok(module) => module.module,
            Err(err) => {
                eprintln!("{err}");
                std::process::exit(1);
            }
        };
        let formatted = format_module(&module);
        if write {
            if let Err(err) = std::fs::write(path, formatted) {
                eprintln!("failed to write {path}: {err}");
                std::process::exit(1);
            }
        } else {
            print!("{}", formatted);
        }
        return;
    }

    if args[1] == "test" {
        if args.len() < 3 {
            eprintln!("usage: at test <file.at>");
            std::process::exit(1);
        }
        let path = &args[2];
        let module = match load_module(path) {
            Ok(module) => module.module,
            Err(err) => {
                eprintln!("{err}");
                std::process::exit(1);
            }
        };

        let mut compiler = Compiler::new();
        let program = match compiler.compile_module(&module) {
            Ok(program) => program,
            Err(err) => {
                eprintln!("{}", format_compile_error(&err, None, Some(path)));
                std::process::exit(1);
            }
        };

        let mut failures = 0;
        for stmt in &module.stmts {
            if let at_syntax::Stmt::Test { name, body } = stmt {
                let chunk = match compiler.compile_test_body(body) {
                    Ok(chunk) => chunk,
                    Err(err) => {
                        eprintln!(
                            "fail - {name}: {}",
                            format_compile_error(&err, None, Some(path))
                        );
                        failures += 1;
                        continue;
                    }
                };
                let test_program = at_vm::Program {
                    functions: program.functions.clone(),
                    main: chunk,
                    main_locals: 0,
                };
                let mut vm = Vm::new();
                match vm.run(&test_program) {
                    Ok(_) => println!("ok - {name}"),
                    Err(err) => {
                        eprintln!("fail - {name}: {err:?}");
                        failures += 1;
                    }
                }
            }
        }

        if failures > 0 {
            std::process::exit(1);
        }
        return;
    }

    if args[1] == "deps" {
        if args.len() < 3 {
            eprintln!("usage: at deps <file.at> [--tree]");
            std::process::exit(1);
        }
        let path = &args[2];
        if args.len() > 3 && args[3] == "--tree" {
            let mut visited = HashSet::new();
            if let Err(err) = print_deps_tree(Path::new(path), 0, &mut visited) {
                eprintln!("{err}");
                std::process::exit(1);
            }
        } else {
            let loaded = match load_module(path) {
                Ok(module) => module,
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(1);
                }
            };
            let mut imports = loaded.imports;
            imports.sort();
            imports.dedup();
            for import in imports {
                println!("{import}");
            }
        }
        return;
    }

    if args[1] == "lint" {
        let fix_mode = args.iter().any(|arg| arg == "--fix" || arg == "-f");
        let path = if fix_mode {
            args.iter()
                .position(|arg| arg == "--fix" || arg == "-f")
                .and_then(|pos| args.get(pos + 1))
        } else {
            args.get(2)
        };

        let path = match path {
            Some(p) => p,
            None => {
                if fix_mode {
                    eprintln!("usage: at lint --fix <file.at>");
                } else {
                    eprintln!("usage: at lint <file.at>");
                }
                std::process::exit(1);
            }
        };

        let source = match fs::read_to_string(path) {
            Ok(source) => source,
            Err(err) => {
                eprintln!("error reading {path}: {err}");
                std::process::exit(1);
            }
        };
        let module = match parse_module(&source) {
            Ok(module) => module,
            Err(err) => {
                eprintln!("{}", format_parse_error(&err, &source, Some(path)));
                std::process::exit(1);
            }
        };

        let config_source = load_lint_config(path);
        match at_lint::lint_module_with_config(&module, config_source.as_deref()) {
            Ok(()) => {
                println!("no issues found");
            }
            Err(errors) => {
                let fixable = at_lint::count_fixable(&errors);
                let (errors, warnings, infos) = split_lint_by_severity(errors);
                let total = errors.len() + warnings.len() + infos.len();

                if fix_mode && fixable > 0 {
                    // Apply fixes
                    let fixed_source = at_lint::apply_fixes(&source, &errors);

                    // Write back to file
                    if let Err(err) = fs::write(path, &fixed_source) {
                        eprintln!("error writing {path}: {err}");
                        std::process::exit(1);
                    }

                    println!("fixed {} issue(s)", fixable);

                    // Re-lint to check remaining issues
                    let fixed_module = match parse_module(&fixed_source) {
                        Ok(m) => m,
                        Err(err) => {
                            eprintln!("{}", format_parse_error(&err, &fixed_source, Some(path)));
                            std::process::exit(1);
                        }
                    };

                    if let Err(remaining) =
                        at_lint::lint_module_with_config(&fixed_module, config_source.as_deref())
                    {
                        let remaining_fixable = at_lint::count_fixable(&remaining);
                        let (errors, warnings, infos) = split_lint_by_severity(remaining);
                        let total = errors.len() + warnings.len() + infos.len();
                        if remaining_fixable > 0 {
                            println!(
                                "{} remaining issue(s) can be auto-fixed (run --fix again)",
                                remaining_fixable
                            );
                        }
                        print_lint_bucket("error", errors, &fixed_source, Some(path));
                        print_lint_bucket("warn", warnings, &fixed_source, Some(path));
                        print_lint_bucket("info", infos, &fixed_source, Some(path));
                        if total > 0 {
                            println!("{} issue(s) require manual fix", total - remaining_fixable);
                        }
                    }
                } else {
                    print_lint_bucket("error", errors, &source, Some(path));
                    print_lint_bucket("warn", warnings, &source, Some(path));
                    print_lint_bucket("info", infos, &source, Some(path));
                    if fixable > 0 {
                        eprintln!("\n{} issue(s) can be auto-fixed with --fix", fixable);
                    }
                    if total > 0 {
                        std::process::exit(1);
                    }
                }
            }
        }
        return;
    }

    if args[1] == "check" {
        if args.len() < 3 {
            eprintln!("usage: at check <file.at>");
            std::process::exit(1);
        }
        let path = &args[2];
        let source = match fs::read_to_string(path) {
            Ok(source) => source,
            Err(err) => {
                eprintln!("error reading {path}: {err}");
                std::process::exit(1);
            }
        };
        let module = match parse_module(&source) {
            Ok(module) => module,
            Err(err) => {
                eprintln!("{}", format_parse_error(&err, &source, Some(path)));
                std::process::exit(1);
            }
        };
        let config_source = load_lint_config(path);
        if let Err(errors) = at_lint::lint_module_with_config(&module, config_source.as_deref()) {
            let (errors, warnings, infos) = split_lint_by_severity(errors);
            print_lint_bucket("error", errors, &source, Some(path));
            print_lint_bucket("warn", warnings, &source, Some(path));
            print_lint_bucket("info", infos, &source, Some(path));
            std::process::exit(1);
        }
        if let Err(errors) = at_check::typecheck_module(&module) {
            for error in errors {
                eprintln!(
                    "{}",
                    format_diagnostic(&error.message, error.span, &source, Some(path))
                );
            }
            std::process::exit(1);
        }
        let module = match load_module(path) {
            Ok(module) => module.module,
            Err(err) => {
                eprintln!("{err}");
                std::process::exit(1);
            }
        };
        let mut compiler = Compiler::new();
        if let Err(err) = compiler.compile_module(&module) {
            eprintln!("{}", format_compile_error(&err, Some(&source), Some(path)));
            std::process::exit(1);
        }
        return;
    }

    if args[1] == "bench" {
        if args.len() < 3 {
            eprintln!("usage: at bench <file.at> [runs]");
            std::process::exit(1);
        }
        let runs = parse_runs(&args[3..]).unwrap_or_else(|| {
            eprintln!("usage: at bench <file.at> [runs]");
            std::process::exit(1);
        });
        let path = Path::new(&args[2]);
        let source = fs::read_to_string(path)
            .map_err(|err| format!("error reading {}: {err}", path.display()))
            .unwrap_or_else(|err| {
                eprintln!("{err}");
                std::process::exit(1);
            });
        let parse_start = std::time::Instant::now();
        let module = parse_module(&source).unwrap_or_else(|err| {
            eprintln!("{}", format_parse_error(&err, &source, path.to_str()));
            std::process::exit(1);
        });
        let parse_time = parse_start.elapsed();

        let compile_start = std::time::Instant::now();
        let mut compiler = Compiler::new();
        let program = compiler.compile_module(&module).unwrap_or_else(|err| {
            eprintln!(
                "{}",
                format_compile_error(&err, Some(&source), path.to_str())
            );
            std::process::exit(1);
        });
        let compile_time = compile_start.elapsed();

        let mut run_time = std::time::Duration::from_secs(0);
        for _ in 0..runs {
            let run_start = std::time::Instant::now();
            let mut vm = Vm::new();
            let _ = vm.run(&program).unwrap_or_else(|err| {
                eprintln!("{}", format_runtime_error(&err, Some(&source)));
                std::process::exit(1);
            });
            run_time += run_start.elapsed();
        }
        let avg_run_time = run_time / runs;

        println!("parse:   {:?}", parse_time);
        println!("compile: {:?}", compile_time);
        if runs > 1 {
            println!("run:     {:?} (avg over {})", avg_run_time, runs);
        } else {
            println!("run:     {:?}", run_time);
        }
        println!("total:   {:?}", parse_time + compile_time + run_time);
        if args.iter().any(|arg| arg == "--json") {
            let avg_run = if runs > 1 { avg_run_time } else { run_time };
            println!(
                "{{\"parse_ns\":{},\"compile_ns\":{},\"run_ns\":{},\"total_ns\":{},\"runs\":{}}}",
                parse_time.as_nanos(),
                compile_time.as_nanos(),
                avg_run.as_nanos(),
                (parse_time + compile_time + run_time).as_nanos(),
                runs
            );
        }
        return;
    }

    if args[1] == "cache" {
        if args.len() < 3 {
            eprintln!("usage: at cache <add|list|show|remove|clear|prune>");
            std::process::exit(1);
        }
        match args[2].as_str() {
            "add" => {
                if args.len() < 5 {
                    eprintln!("usage: at cache add <url> <path>");
                    std::process::exit(1);
                }
                let url = &args[3];
                let source_path = &args[4];
                let contents = fs::read_to_string(source_path)
                    .map_err(|err| format!("error reading {source_path}: {err}"))
                    .unwrap_or_else(|err| {
                        eprintln!("{err}");
                        std::process::exit(1);
                    });
                let cache_path = store_remote_contents(url, &contents, Path::new("."))
                    .map_err(|err| format!("error caching {url}: {err}"))
                    .unwrap_or_else(|err| {
                        eprintln!("{err}");
                        std::process::exit(1);
                    });
                println!("cached {}", cache_path.display());
            }
            "show" => {
                if args.len() < 4 {
                    eprintln!("usage: at cache show <url>");
                    std::process::exit(1);
                }
                let url = &args[3];
                let cache_path = resolve_cached_path(Path::new("."), url).unwrap_or_else(|err| {
                    eprintln!("{err}");
                    std::process::exit(1);
                });
                let contents = fs::read_to_string(&cache_path)
                    .map_err(|err| format!("error reading {}: {err}", cache_path.display()))
                    .unwrap_or_else(|err| {
                        eprintln!("{err}");
                        std::process::exit(1);
                    });
                print!("{contents}");
            }
            "list" => {
                let cache_dir = Path::new(".at").join("cache");
                if !cache_dir.exists() {
                    return;
                }
                let entries = fs::read_dir(&cache_dir)
                    .map_err(|err| format!("error reading cache directory: {err}"))
                    .unwrap_or_else(|err| {
                        eprintln!("{err}");
                        std::process::exit(1);
                    });
                for entry in entries.flatten() {
                    println!("{}", entry.path().display());
                }
            }
            "remove" => {
                if args.len() < 4 {
                    eprintln!("usage: at cache remove <url>");
                    std::process::exit(1);
                }
                let url = &args[3];
                let cache_path = resolve_cached_path(Path::new("."), url)
                    .unwrap_or_else(|_| Path::new(".at").join("cache").join(cache_file_name(url)));
                if cache_path.exists() {
                    fs::remove_file(&cache_path)
                        .map_err(|err| format!("error removing {}: {err}", cache_path.display()))
                        .unwrap_or_else(|err| {
                            eprintln!("{err}");
                            std::process::exit(1);
                        });
                }
            }
            "clear" => {
                let cache_dir = Path::new(".at").join("cache");
                if cache_dir.exists() {
                    fs::remove_dir_all(&cache_dir)
                        .map_err(|err| format!("error clearing cache: {err}"))
                        .unwrap_or_else(|err| {
                            eprintln!("{err}");
                            std::process::exit(1);
                        });
                }
            }
            "prune" => {
                let options = parse_prune_options(&args[3..]).unwrap_or_else(|err| {
                    eprintln!("{err}");
                    eprintln!("usage: at cache prune [--max <files>] [--max-files <files>] [--max-mb <mb>]");
                    std::process::exit(1);
                });
                prune_cache(Path::new("."), &options)
                    .map_err(|err| format!("error pruning cache: {err}"))
                    .unwrap_or_else(|err| {
                        eprintln!("{err}");
                        std::process::exit(1);
                    });
            }
            _ => {
                eprintln!("usage: at cache <add|list|show|remove|clear|prune>");
                std::process::exit(1);
            }
        }
        return;
    }

    if args[1] == "repl" {
        run_repl();
        return;
    }

    run_file(&args[1], &args[2..]);
}

fn parse_runs(args: &[String]) -> Option<u32> {
    let mut runs: Option<u32> = None;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--runs" => {
                let value = args.get(index + 1)?;
                let parsed = value.parse::<u32>().ok()?;
                if parsed == 0 {
                    return None;
                }
                runs = Some(parsed);
                index += 2;
            }
            "--json" => {
                index += 1;
            }
            value => {
                if runs.is_none() {
                    let parsed = value.parse::<u32>().ok()?;
                    if parsed == 0 {
                        return None;
                    }
                    runs = Some(parsed);
                    index += 1;
                } else {
                    return None;
                }
            }
        }
    }
    Some(runs.unwrap_or(1))
}

fn run_repl() {
    let mut buffer = String::new();
    let mut rl = match rustyline::DefaultEditor::new() {
        Ok(editor) => editor,
        Err(_) => {
            eprintln!("failed to start line editor");
            return;
        }
    };
    let history_path = std::env::var("HOME")
        .ok()
        .map(|home| std::path::PathBuf::from(home).join(".at_history"));
    if let Some(path) = history_path.as_ref() {
        let _ = rl.load_history(path);
    }

    loop {
        let prompt = if buffer.is_empty() { "> " } else { "... " };
        let line = match rl.readline(prompt) {
            Ok(line) => line,
            Err(rustyline::error::ReadlineError::Interrupted)
            | Err(rustyline::error::ReadlineError::Eof) => break,
            Err(_) => break,
        };

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if trimmed == "exit" || trimmed == ".exit" {
            break;
        }
        if trimmed == "reset" || trimmed == ".reset" {
            buffer.clear();
            println!("ok");
            continue;
        }
        let _ = rl.add_history_entry(trimmed);
        buffer.push_str(trimmed);
        buffer.push('\n');

        if !is_input_complete(&buffer) {
            continue;
        }

        match parse_module(&buffer) {
            Ok(module) => {
                let mut compiler = Compiler::new();
                let program = match compiler.compile_module(&module) {
                    Ok(program) => program,
                    Err(err) => {
                        eprintln!("{}", format_compile_error(&err, Some(&buffer), None));
                        buffer.clear();
                        continue;
                    }
                };
                let mut vm = Vm::new();
                match vm.run(&program) {
                    Ok(Some(value)) => println!("{}", format_value(&value)),
                    Ok(None) => {}
                    Err(err) => eprintln!("{}", format_runtime_error(&err, Some(&buffer))),
                }
            }
            Err(err) => {
                eprintln!("{}", format_parse_error(&err, &buffer, None));
            }
        }

        buffer.clear();
    }

    if let Some(path) = history_path.as_ref() {
        let _ = rl.append_history(path);
    }
}

fn run_file(path: &str, extra_args: &[String]) {
    let source = fs::read_to_string(path).ok();
    let module = match load_module(path) {
        Ok(module) => module.module,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    let mut compiler = Compiler::new();
    let program = match compiler.compile_module(&module) {
        Ok(program) => program,
        Err(err) => {
            eprintln!(
                "{}",
                format_compile_error(&err, source.as_deref(), Some(path))
            );
            std::process::exit(1);
        }
    };

    let mut capabilities = std::collections::HashSet::new();
    let mut i = 0;
    while i < extra_args.len() {
        if extra_args[i] == "-c" || extra_args[i] == "--capability" {
            if i + 1 < extra_args.len() {
                capabilities.insert(extra_args[i + 1].clone());
                i += 2;
            } else {
                eprintln!("error: --capability requires a value");
                std::process::exit(1);
            }
        } else {
            i += 1;
        }
    }

    let vm = if capabilities.is_empty() {
        Vm::new()
    } else {
        Vm::with_capabilities(capabilities)
    };
    let mut vm = vm;
    match vm.run(&program) {
        Ok(Some(value)) => println!("{}", format_value(&value)),
        Ok(None) => {}
        Err(err) => {
            eprintln!("{}", format_runtime_error(&err, source.as_deref()));
            std::process::exit(1);
        }
    }
}

fn format_runtime_error(err: &at_vm::VmError, source: Option<&str>) -> String {
    match err {
        at_vm::VmError::Runtime {
            message,
            span,
            stack,
        } => {
            let mut output = if let (Some(span), Some(source)) = (span, source) {
                if let Some((line, column)) = offset_to_line_col(source, span.start) {
                    format!("runtime error: {message} at {line}:{column}")
                } else {
                    format!("runtime error: {message}")
                }
            } else {
                format!("runtime error: {message}")
            };

            if let Some(stack) = stack {
                if !stack.is_empty() {
                    output.push_str("\nstack trace:");
                    for frame in stack {
                        let mut line = format!("\n  at {}", frame.name);
                        if let (Some(span), Some(source)) = (frame.span, source) {
                            if let Some((line_no, col)) = offset_to_line_col(source, span.start) {
                                line.push_str(&format!(" ({line_no}:{col})"));
                            }
                        }
                        output.push_str(&line);
                    }
                }
            }

            output
        }
        other => format!("runtime error: {other:?}"),
    }
}

fn format_parse_error(err: &at_parser::ParseError, source: &str, path: Option<&str>) -> String {
    let (message, span) = match err {
        at_parser::ParseError::UnexpectedToken {
            expected,
            found,
            span,
        } => (format!("expected {expected}, found {found:?}"), Some(*span)),
        at_parser::ParseError::UnterminatedString { span } => {
            ("unterminated string".to_string(), Some(*span))
        }
        at_parser::ParseError::UnterminatedBlockComment { span } => {
            ("unterminated block comment".to_string(), Some(*span))
        }
        at_parser::ParseError::InvalidNumber { span } => {
            ("invalid number".to_string(), Some(*span))
        }
    };

    let prefix = match path {
        Some(path) => format!("parse error in {path}"),
        None => "parse error".to_string(),
    };

    if let Some(span) = span {
        if let Some((line, col)) = offset_to_line_col(source, span.start) {
            return format!("{prefix} at {line}:{col}: {message}");
        }
    }

    format!("{prefix}: {message}")
}

fn load_lint_config(path: &str) -> Option<String> {
    let file_path = Path::new(path);
    let mut dir = file_path.parent()?;
    loop {
        let candidate = dir.join(".at-lint.toml");
        if candidate.exists() {
            return fs::read_to_string(candidate).ok();
        }
        match dir.parent() {
            Some(parent) => dir = parent,
            None => break,
        }
    }
    None
}

fn split_lint_by_severity(
    mut errors: Vec<at_lint::LintError>,
) -> (
    Vec<at_lint::LintError>,
    Vec<at_lint::LintError>,
    Vec<at_lint::LintError>,
) {
    let mut error_list = Vec::new();
    let mut warn_list = Vec::new();
    let mut info_list = Vec::new();
    for error in errors.drain(..) {
        match error.severity {
            at_lint::LintSeverity::Error => error_list.push(error),
            at_lint::LintSeverity::Warn => warn_list.push(error),
            at_lint::LintSeverity::Info => info_list.push(error),
        }
    }
    (error_list, warn_list, info_list)
}

fn print_lint_bucket(
    label: &str,
    errors: Vec<at_lint::LintError>,
    source: &str,
    path: Option<&str>,
) {
    if errors.is_empty() {
        return;
    }
    for error in errors {
        eprintln!(
            "{}",
            format_diagnostic(
                &format!("[{label}] {}", error.message),
                error.span,
                source,
                path
            )
        );
    }
}

fn format_compile_error(err: &at_vm::VmError, source: Option<&str>, path: Option<&str>) -> String {
    match err {
        at_vm::VmError::Compile { message, span } => {
            if let (Some(span), Some(source)) = (span, source) {
                if let Some((line, col)) = offset_to_line_col(source, span.start) {
                    return match path {
                        Some(path) => format!("compile error in {path} at {line}:{col}: {message}"),
                        None => format!("compile error at {line}:{col}: {message}"),
                    };
                }
            }
            match path {
                Some(path) => format!("compile error in {path}: {message}"),
                None => format!("compile error: {message}"),
            }
        }
        other => match path {
            Some(path) => format!("compile error in {path}: {other:?}"),
            None => format!("compile error: {other:?}"),
        },
    }
}

fn format_diagnostic(
    message: &str,
    span: Option<at_syntax::Span>,
    source: &str,
    path: Option<&str>,
) -> String {
    let prefix = match path {
        Some(path) => path.to_string(),
        None => "<input>".to_string(),
    };
    if let Some(span) = span {
        if let Some((line, col)) = offset_to_line_col(source, span.start) {
            return format!("{prefix}:{line}:{col}: {message}");
        }
    }
    format!("{prefix}: {message}")
}

fn offset_to_line_col(source: &str, offset: usize) -> Option<(usize, usize)> {
    let mut line = 1usize;
    let mut col = 1usize;
    let mut count = 0usize;
    for ch in source.chars() {
        if count >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
        count += ch.len_utf8();
    }
    Some((line, col))
}

fn is_input_complete(input: &str) -> bool {
    let mut brace_depth: i32 = 0;
    let mut paren_depth: i32 = 0;
    let mut in_string = false;
    let mut escaped = false;

    for ch in input.chars() {
        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }

        match ch {
            '"' => in_string = true,
            '{' => brace_depth += 1,
            '}' => brace_depth = brace_depth.saturating_sub(1),
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            _ => {}
        }
    }

    !in_string && brace_depth == 0 && paren_depth == 0
}

struct LoadedModule {
    module: Module,
    imports: Vec<String>,
    import_aliases: Vec<(String, String)>,
}

fn load_module(path: &str) -> Result<LoadedModule, String> {
    let mut visited = HashSet::new();
    load_module_inner(Path::new(path), &mut visited)
}

fn load_module_inner(path: &Path, visited: &mut HashSet<PathBuf>) -> Result<LoadedModule, String> {
    let normalized = path
        .canonicalize()
        .map_err(|err| format!("error reading {}: {err}", path.display()))?;
    if !visited.insert(normalized.clone()) {
        return Ok(LoadedModule {
            module: Module {
                functions: Vec::new(),
                stmts: Vec::new(),
                comments: Vec::new(),
            },
            imports: Vec::new(),
            import_aliases: Vec::new(),
        });
    }

    let source = fs::read_to_string(&normalized)
        .map_err(|err| format!("error reading {}: {err}", normalized.display()))?;
    let mut module = parse_module(&source).map_err(|err| {
        format_parse_error(&err, &source, Some(&normalized.display().to_string()))
    })?;

    let mut merged_functions = module.functions.clone();
    let mut merged_stmts = Vec::new();
    let mut merged_imports = Vec::new();
    let mut merged_aliases = Vec::new();
    let mut seen_aliases = HashSet::new();
    let base_dir = normalized
        .parent()
        .ok_or_else(|| format!("invalid path: {}", normalized.display()))?;

    for stmt in module.stmts.drain(..) {
        match stmt {
            Stmt::Import { path, alias } => {
                if !seen_aliases.insert(alias.name.clone()) {
                    return Err(format!("duplicate import alias: {}", alias.name));
                }
                let alias_name = alias.name.clone();
                if path == "std" || path == "std.at" {
                    let import_path = Path::new("stdlib").join("std.at");
                    let mut imported = load_module_inner(&import_path, visited)?;
                    prefix_module(&mut imported.module, &alias.name);
                    merged_imports.push(import_path.display().to_string());
                    merged_imports.extend(imported.imports);
                    merged_aliases.push((alias_name, import_path.display().to_string()));
                    merged_aliases.extend(imported.import_aliases);
                    merged_functions.extend(imported.module.functions);
                    continue;
                }
                if path.starts_with("http://") || path.starts_with("https://") {
                    let import_path = fetch_remote(&path, base_dir)?;
                    let mut imported = load_module_inner(&import_path, visited)?;
                    prefix_module(&mut imported.module, &alias.name);
                    merged_imports.push(import_path.display().to_string());
                    merged_imports.extend(imported.imports);
                    merged_aliases.push((alias_name, import_path.display().to_string()));
                    merged_aliases.extend(imported.import_aliases);
                    merged_functions.extend(imported.module.functions);
                    continue;
                }
                let import_path = base_dir.join(path);
                let mut imported = load_module_inner(&import_path, visited)?;
                prefix_module(&mut imported.module, &alias.name);
                merged_imports.push(import_path.display().to_string());
                merged_imports.extend(imported.imports);
                merged_aliases.push((alias_name, import_path.display().to_string()));
                merged_aliases.extend(imported.import_aliases);
                merged_functions.extend(imported.module.functions);
            }
            other => merged_stmts.push(other),
        }
    }

    Ok(LoadedModule {
        module: Module {
            functions: merged_functions,
            stmts: merged_stmts,
            comments: Vec::new(),
        },
        imports: merged_imports,
        import_aliases: merged_aliases,
    })
}

fn prefix_module(module: &mut Module, alias: &str) {
    for func in &mut module.functions {
        func.name.name = format!("{}.{}", alias, func.name.name);
    }
}

fn fetch_remote(url: &str, base_dir: &Path) -> Result<PathBuf, String> {
    if let Ok(path) = resolve_cached_path(base_dir, url) {
        if path.exists() {
            return Ok(path);
        }
    }

    let response = ureq::get(url)
        .call()
        .map_err(|err| format!("error fetching {url}: {err}"))?;
    let contents = response
        .into_string()
        .map_err(|err| format!("error reading response from {url}: {err}"))?;
    store_remote_contents(url, &contents, base_dir)
}

fn store_remote_contents(url: &str, contents: &str, base_dir: &Path) -> Result<PathBuf, String> {
    let cache_dir = cache_dir(base_dir);
    fs::create_dir_all(&cache_dir)
        .map_err(|err| format!("error creating cache directory: {err}"))?;

    let hash = hash_contents(contents);
    let filename = format!("{hash}.at");
    let cache_path = cache_dir.join(filename);
    fs::write(&cache_path, contents)
        .map_err(|err| format!("error writing {}: {err}", cache_path.display()))?;

    let mut lockfile = load_lockfile(base_dir)?;
    lockfile.entries.insert(url.to_string(), hash);
    save_lockfile(base_dir, &lockfile)?;
    Ok(cache_path)
}

fn resolve_cached_path(base_dir: &Path, url: &str) -> Result<PathBuf, String> {
    let lockfile = load_lockfile(base_dir)?;
    if let Some(hash) = lockfile.entries.get(url) {
        return Ok(cache_dir(base_dir).join(format!("{hash}.at")));
    }
    Ok(cache_dir(base_dir).join(cache_file_name(url)))
}

fn cache_dir(base_dir: &Path) -> PathBuf {
    base_dir.join(".at").join("cache")
}

fn lockfile_path(base_dir: &Path) -> PathBuf {
    base_dir.join(".at").join("lock")
}

fn load_lockfile(base_dir: &Path) -> Result<Lockfile, String> {
    let path = lockfile_path(base_dir);
    if !path.exists() {
        return Ok(Lockfile {
            version: LOCKFILE_VERSION,
            entries: HashMap::new(),
        });
    }
    let contents = fs::read_to_string(&path)
        .map_err(|err| format!("error reading {}: {err}", path.display()))?;
    let mut entries = HashMap::new();
    let mut version = 0;
    for line in contents.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line.starts_with('#') {
            continue;
        }
        let mut parts = line.split_whitespace();
        let url = match parts.next() {
            Some(value) => value.to_string(),
            None => continue,
        };
        if url == "version" {
            let value = match parts.next() {
                Some(value) => value,
                None => continue,
            };
            version = value
                .parse::<u32>()
                .map_err(|_| format!("invalid lockfile version: {value}"))?;
            continue;
        }
        let hash = match parts.next() {
            Some(value) => value.to_string(),
            None => continue,
        };
        entries.insert(url, hash);
    }
    if version > LOCKFILE_VERSION {
        return Err(format!("unsupported lockfile version: {version}"));
    }
    let version = if version == 0 {
        LOCKFILE_VERSION
    } else {
        version
    };
    Ok(Lockfile { version, entries })
}

fn save_lockfile(base_dir: &Path, lockfile: &Lockfile) -> Result<(), String> {
    let path = lockfile_path(base_dir);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("error creating lockfile directory: {err}"))?;
    }
    let mut lines: Vec<String> = lockfile
        .entries
        .iter()
        .map(|(url, hash)| format!("{url} {hash}"))
        .collect();
    lines.sort();
    lines.insert(0, format!("version {}", lockfile.version));
    fs::write(&path, lines.join("\n"))
        .map_err(|err| format!("error writing {}: {err}", path.display()))?;
    Ok(())
}

fn hash_contents(contents: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(contents.as_bytes());
    hex::encode(hasher.finalize())
}

fn cache_file_name(url: &str) -> String {
    url.replace("https://", "")
        .replace("http://", "")
        .replace('/', "_")
        .replace(':', "_")
}

fn parse_prune_options(args: &[String]) -> Result<PruneOptions, String> {
    let mut options = PruneOptions::default();
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--max" | "--max-files" => {
                let value = args
                    .get(index + 1)
                    .ok_or_else(|| "missing value for --max-files".to_string())?;
                let parsed = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid max files: {value}"))?;
                options.max_files = Some(parsed);
                index += 2;
            }
            "--max-mb" => {
                let value = args
                    .get(index + 1)
                    .ok_or_else(|| "missing value for --max-mb".to_string())?;
                let parsed = value
                    .parse::<u64>()
                    .map_err(|_| format!("invalid max mb: {value}"))?;
                options.max_bytes = Some(parsed.saturating_mul(1024 * 1024));
                index += 2;
            }
            value => {
                return Err(format!("unknown prune option: {value}"));
            }
        }
    }
    Ok(options)
}

#[cfg(test)]
mod tests {
    use super::{
        parse_prune_options, prune_cache, prune_cache_with_report, save_lockfile, Lockfile,
        PruneOptions,
    };
    use std::collections::HashMap;
    use std::fs;
    use std::path::PathBuf;

    fn at_binary() -> PathBuf {
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR");
        let profile = if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        };
        PathBuf::from(manifest_dir)
            .join("../../target")
            .join(profile)
            .join("at")
    }

    #[test]
    fn parse_prune_options_defaults() {
        let options = parse_prune_options(&[]).expect("options");
        assert!(options.max_files.is_none());
        assert!(options.max_bytes.is_none());
    }

    #[test]
    fn parse_prune_options_max_files() {
        let options = parse_prune_options(&["--max".into(), "10".into()]).expect("options");
        assert_eq!(options.max_files, Some(10));
    }

    #[test]
    fn parse_prune_options_max_files_alias() {
        let options = parse_prune_options(&["--max-files".into(), "3".into()]).expect("options");
        assert_eq!(options.max_files, Some(3));
    }

    #[test]
    fn parse_prune_options_max_mb() {
        let options = parse_prune_options(&["--max-mb".into(), "2".into()]).expect("options");
        assert_eq!(options.max_bytes, Some(2 * 1024 * 1024));
    }

    #[test]
    fn parse_prune_options_rejects_unknown() {
        let err = parse_prune_options(&["--nope".into()]).expect_err("error");
        assert!(err.contains("unknown prune option"));
    }

    #[test]
    fn parse_prune_options_missing_value() {
        let err = parse_prune_options(&["--max".into()]).expect_err("error");
        assert!(err.contains("missing value"));
    }

    #[test]
    fn parse_runs_with_json_and_flag() {
        let args = vec!["--runs".into(), "5".into(), "--json".into()];
        let runs = super::parse_runs(&args).expect("runs");
        assert_eq!(runs, 5);
    }

    fn temp_base_dir(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        path.push(format!("at_cache_prune_{name}_{nanos}"));
        path
    }

    fn write_cache_file(base: &PathBuf, hash: &str, size: usize) -> PathBuf {
        let cache_dir = base.join(".at").join("cache");
        fs::create_dir_all(&cache_dir).expect("create cache dir");
        let path = cache_dir.join(format!("{hash}.at"));
        let contents = vec![b'x'; size];
        fs::write(&path, contents).expect("write cache file");
        path
    }

    #[test]
    fn prune_removes_unreferenced_when_no_limits() {
        let base = temp_base_dir("no_limits");
        fs::create_dir_all(base.join(".at")).expect("create base");
        let mut entries = HashMap::new();
        entries.insert("https://example.com/a.at".to_string(), "aaaa".to_string());
        save_lockfile(
            &base,
            &Lockfile {
                version: super::LOCKFILE_VERSION,
                entries,
            },
        )
        .expect("save lockfile");

        let referenced = write_cache_file(&base, "aaaa", 10);
        let unreferenced = write_cache_file(&base, "bbbb", 10);

        prune_cache(&base, &PruneOptions::default()).expect("prune cache");
        assert!(referenced.exists());
        assert!(!unreferenced.exists());
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn prune_respects_max_files_for_unreferenced() {
        let base = temp_base_dir("max_files");
        fs::create_dir_all(base.join(".at")).expect("create base");
        let mut entries = HashMap::new();
        entries.insert("https://example.com/a.at".to_string(), "aaaa".to_string());
        entries.insert("https://example.com/b.at".to_string(), "bbbb".to_string());
        save_lockfile(
            &base,
            &Lockfile {
                version: super::LOCKFILE_VERSION,
                entries,
            },
        )
        .expect("save lockfile");

        let referenced_a = write_cache_file(&base, "aaaa", 10);
        let referenced_b = write_cache_file(&base, "bbbb", 10);
        let old_unreferenced = write_cache_file(&base, "cccc", 10);
        std::thread::sleep(std::time::Duration::from_millis(5));
        let new_unreferenced = write_cache_file(&base, "dddd", 10);

        let options = PruneOptions {
            max_files: Some(3),
            max_bytes: None,
        };
        prune_cache(&base, &options).expect("prune cache");

        assert!(referenced_a.exists());
        assert!(referenced_b.exists());
        assert!(!old_unreferenced.exists());
        assert!(new_unreferenced.exists());
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn prune_respects_max_bytes_for_unreferenced() {
        let base = temp_base_dir("max_bytes");
        fs::create_dir_all(base.join(".at")).expect("create base");
        let mut entries = HashMap::new();
        entries.insert("https://example.com/a.at".to_string(), "aaaa".to_string());
        save_lockfile(
            &base,
            &Lockfile {
                version: super::LOCKFILE_VERSION,
                entries,
            },
        )
        .expect("save lockfile");

        let referenced = write_cache_file(&base, "aaaa", 1024);
        let old_unreferenced = write_cache_file(&base, "bbbb", 2048);
        std::thread::sleep(std::time::Duration::from_millis(5));
        let new_unreferenced = write_cache_file(&base, "cccc", 512);

        let options = PruneOptions {
            max_files: None,
            max_bytes: Some(2048),
        };
        prune_cache(&base, &options).expect("prune cache");

        assert!(referenced.exists());
        assert!(!old_unreferenced.exists());
        assert!(new_unreferenced.exists());
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn prune_keeps_referenced_when_over_limits() {
        let base = temp_base_dir("referenced_over_limits");
        fs::create_dir_all(base.join(".at")).expect("create base");
        let mut entries = HashMap::new();
        entries.insert("https://example.com/a.at".to_string(), "aaaa".to_string());
        entries.insert("https://example.com/b.at".to_string(), "bbbb".to_string());
        save_lockfile(
            &base,
            &Lockfile {
                version: super::LOCKFILE_VERSION,
                entries,
            },
        )
        .expect("save lockfile");

        let referenced_a = write_cache_file(&base, "aaaa", 1024);
        let referenced_b = write_cache_file(&base, "bbbb", 1024);

        let options = PruneOptions {
            max_files: Some(1),
            max_bytes: Some(512),
        };
        prune_cache(&base, &options).expect("prune cache");

        assert!(referenced_a.exists());
        assert!(referenced_b.exists());
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn prune_reports_referenced_over_limits() {
        let base = temp_base_dir("referenced_over_limits_report");
        fs::create_dir_all(base.join(".at")).expect("create base");
        let mut entries = HashMap::new();
        entries.insert("https://example.com/a.at".to_string(), "aaaa".to_string());
        entries.insert("https://example.com/b.at".to_string(), "bbbb".to_string());
        save_lockfile(
            &base,
            &Lockfile {
                version: super::LOCKFILE_VERSION,
                entries,
            },
        )
        .expect("save lockfile");

        let referenced_a = write_cache_file(&base, "aaaa", 1024);
        let referenced_b = write_cache_file(&base, "bbbb", 1024);

        let options = PruneOptions {
            max_files: Some(1),
            max_bytes: Some(512),
        };
        let report = prune_cache_with_report(&base, &options).expect("prune cache");

        assert!(referenced_a.exists());
        assert!(referenced_b.exists());
        assert!(report.referenced_over_limits);
        fs::remove_dir_all(&base).ok();
    }

    #[test]
    fn integration_run_simple() {
        let temp_dir = std::env::temp_dir().join("at_test_run_simple");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "print(\"hello\");").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("run")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "run failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("hello"),
            "expected 'hello' in output, got: {}",
            stdout
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_run_arithmetic() {
        let temp_dir = std::env::temp_dir().join("at_test_run_arith");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "print(2 + 3);").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("run")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "run failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("5"),
            "expected '5' in output, got: {}",
            stdout
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_run_set_nested() {
        let temp_dir = std::env::temp_dir().join("at_test_run_set_nested");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(
            &file,
            "struct Root { items: array<array<int>> }\nlet store = Root { items: [[1]] };\nset store.items[0][0] = 2;\nprint(store.items[0][0]);",
        )
        .expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("run")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "run failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("2"),
            "expected '2' in output, got: {}",
            stdout
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_run_syntax_error() {
        let temp_dir = std::env::temp_dir().join("at_test_run_syntax_error");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "print(").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("run")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            !output.status.success(),
            "expected failure for syntax error"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("error") || stderr.contains("expected"),
            "expected error message, got: {}",
            stderr
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_fmt_simple() {
        let temp_dir = std::env::temp_dir().join("at_test_fmt");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "print(1+2);").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("fmt")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "fmt failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("print"),
            "expected formatted output, got: {}",
            stdout
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_fmt_preserves_semantics() {
        let temp_dir = std::env::temp_dir().join("at_test_fmt_semantics");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "print(1+2*3);").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("fmt")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "fmt failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("1 + 2 * 3") || stdout.contains("1+2*3"),
            "unexpected formatting: {}",
            stdout
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_test_passing() {
        let temp_dir = std::env::temp_dir().join("at_test_pass");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "test \"basic\" { assert(1 + 1 == 2); }").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("test")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "test failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("ok - basic"),
            "expected 'ok - basic', got: {}",
            stdout
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_test_failing() {
        let temp_dir = std::env::temp_dir().join("at_test_fail");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "test \"should fail\" { assert(1 == 2); }").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("test")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(!output.status.success(), "expected test failure");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("fail - should fail"),
            "expected failure message, got: {}",
            stderr
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_check_valid() {
        let temp_dir = std::env::temp_dir().join("at_test_check_valid");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "let x = 1;\nprint(x);").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("check")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "check failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_check_type_error() {
        let temp_dir = std::env::temp_dir().join("at_test_check_error");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        let source = "let x: Int = \"string\";";
        fs::write(&file, source).expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("check")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(!output.status.success(), "expected type error");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("error") || stderr.contains("mismatch") || stderr.contains("string"),
            "expected type error, got: {}",
            stderr
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_lint_clean() {
        let temp_dir = std::env::temp_dir().join("at_test_lint_clean");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "let x = 1;\nprint(x);").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("lint")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "lint failed: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("no issues"),
            "expected clean lint, got: {}",
            stdout
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_lint_unused() {
        let temp_dir = std::env::temp_dir().join("at_test_lint_unused");
        fs::create_dir_all(&temp_dir).ok();
        let file = temp_dir.join("test.at");
        fs::write(&file, "fn unused() { }\nprint(1);").expect("write test file");

        let output = std::process::Command::new(at_binary())
            .arg("lint")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(!output.status.success(), "expected lint warning");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("unused") || stderr.contains("never called"),
            "expected unused warning, got: {}",
            stderr
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_version_flag() {
        let output = std::process::Command::new(at_binary())
            .arg("--version")
            .output()
            .expect("run at binary");

        assert!(output.status.success(), "--version failed");
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("at "),
            "expected version output, got: {}",
            stdout
        );
    }

    #[test]
    fn integration_examples_run() {
        let examples_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples");

        let simple_files = ["sum.at", "arrays.at"];
        for name in &simple_files {
            let file = examples_dir.join(name);
            if file.exists() {
                let output = std::process::Command::new(at_binary())
                    .arg("run")
                    .arg(&file)
                    .output()
                    .expect("run at binary");

                assert!(
                    output.status.success(),
                    "run {} failed: {:?}",
                    name,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        }
    }

    #[test]
    fn integration_cycle_detection() {
        let temp_dir = std::env::temp_dir().join("at_test_cycle");
        fs::create_dir_all(&temp_dir).ok();

        let file_a = temp_dir.join("a.at");
        let file_b = temp_dir.join("b.at");

        fs::write(&file_a, "import \"./b.at\" as b;\nprint(\"a\");").expect("write a.at");
        fs::write(&file_b, "import \"./a.at\" as a;\nprint(\"b\");").expect("write b.at");

        let output = std::process::Command::new(at_binary())
            .arg("run")
            .arg(&file_a)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "cycle should be handled gracefully: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn integration_self_import() {
        let temp_dir = std::env::temp_dir().join("at_test_self");
        fs::create_dir_all(&temp_dir).ok();

        let file = temp_dir.join("self.at");
        fs::write(&file, "import \"./self.at\" as self;").expect("write self.at");

        let output = std::process::Command::new(at_binary())
            .arg("run")
            .arg(&file)
            .output()
            .expect("run at binary");

        assert!(
            output.status.success(),
            "self-import should be handled gracefully: {:?}",
            String::from_utf8_lossy(&output.stderr)
        );

        fs::remove_dir_all(&temp_dir).ok();
    }
}

fn prune_cache(base_dir: &Path, options: &PruneOptions) -> Result<(), String> {
    let report = prune_cache_with_report(base_dir, options)?;
    if report.referenced_over_limits {
        println!("cache prune: limits exceeded by referenced files");
    }
    if report.removed > 0 {
        println!(
            "pruned {} cache files ({} bytes)",
            report.removed, report.removed_bytes
        );
    }
    Ok(())
}

fn prune_cache_with_report(base_dir: &Path, options: &PruneOptions) -> Result<PruneReport, String> {
    let lockfile = load_lockfile(base_dir)?;
    let cache_dir = cache_dir(base_dir);
    if !cache_dir.exists() {
        return Ok(PruneReport::default());
    }

    let allowed: HashSet<String> = lockfile.entries.values().cloned().collect();
    let entries =
        fs::read_dir(&cache_dir).map_err(|err| format!("error reading cache directory: {err}"))?;

    let mut referenced = Vec::new();
    let mut unreferenced = Vec::new();
    let mut total_size: u64 = 0;
    let mut total_files: usize = 0;

    for entry in entries {
        let entry = entry.map_err(|err| format!("error reading cache entry: {err}"))?;
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let file_name = match path.file_name().and_then(|name| name.to_str()) {
            Some(name) => name,
            None => continue,
        };
        let hash = match file_name.strip_suffix(".at") {
            Some(hash) => hash.to_string(),
            None => continue,
        };
        let metadata = path
            .metadata()
            .map_err(|err| format!("error reading {}: {err}", path.display()))?;
        let size = metadata.len();
        let modified = metadata
            .modified()
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH);
        total_size += size;
        total_files += 1;
        let entry = CacheEntry {
            path,
            hash,
            size,
            modified,
        };
        if allowed.contains(&entry.hash) {
            referenced.push(entry);
        } else {
            unreferenced.push(entry);
        }
    }

    let mut removed = 0usize;
    let mut removed_bytes = 0u64;
    let mut referenced_over_limits = false;

    if options.max_files.is_none() && options.max_bytes.is_none() {
        for entry in unreferenced {
            fs::remove_file(&entry.path)
                .map_err(|err| format!("error removing {}: {err}", entry.path.display()))?;
            removed += 1;
            removed_bytes += entry.size;
        }
    } else {
        unreferenced.sort_by_key(|entry| entry.modified);
        let mut unreferenced: VecDeque<CacheEntry> = unreferenced.into();
        loop {
            let over_files = options
                .max_files
                .map(|max| total_files > max)
                .unwrap_or(false);
            let over_bytes = options
                .max_bytes
                .map(|max| total_size > max)
                .unwrap_or(false);
            if !(over_files || over_bytes) {
                break;
            }
            let entry = match unreferenced.pop_front() {
                Some(entry) => entry,
                None => break,
            };
            fs::remove_file(&entry.path)
                .map_err(|err| format!("error removing {}: {err}", entry.path.display()))?;
            total_files = total_files.saturating_sub(1);
            total_size = total_size.saturating_sub(entry.size);
            removed += 1;
            removed_bytes += entry.size;
        }

        let still_over_files = options
            .max_files
            .map(|max| total_files > max)
            .unwrap_or(false);
        let still_over_bytes = options
            .max_bytes
            .map(|max| total_size > max)
            .unwrap_or(false);
        if (still_over_files || still_over_bytes) && !referenced.is_empty() {
            referenced_over_limits = true;
        }
    }

    Ok(PruneReport {
        removed,
        removed_bytes,
        referenced_over_limits,
    })
}

struct Lockfile {
    version: u32,
    entries: HashMap<String, String>,
}

const LOCKFILE_VERSION: u32 = 1;

struct CacheEntry {
    path: PathBuf,
    hash: String,
    size: u64,
    modified: std::time::SystemTime,
}

#[derive(Debug, Clone)]
struct ToolExec {
    func_id: usize,
    params: Vec<ToolParam>,
    needs: Vec<String>,
}

#[derive(Debug, Clone)]
struct ToolParam {
    name: String,
    ty: Option<String>,
}

#[derive(Debug, Default)]
struct PruneOptions {
    max_files: Option<usize>,
    max_bytes: Option<u64>,
}

#[derive(Debug, Default)]
struct PruneReport {
    removed: usize,
    removed_bytes: u64,
    referenced_over_limits: bool,
}

fn type_ref_to_schema(ty: &TypeRef) -> serde_json::Value {
    match ty {
        TypeRef::Named { name, args } => {
            let mut schema = match name.name.as_str() {
                "array" => {
                    let mut base = type_name_to_schema("array");
                    if let Some(value_schema) = args.get(0).map(type_ref_to_schema) {
                        if let Some(obj) = base.as_object_mut() {
                            obj.insert("items".to_string(), value_schema);
                        }
                    }
                    base
                }
                "option" => {
                    let mut base = type_name_to_schema("option");
                    if let Some(value_schema) = args.get(0).map(type_ref_to_schema) {
                        if let Some(obj) = base.as_object_mut() {
                            if let Some(props) = obj.get_mut("properties") {
                                if let Some(props) = props.as_object_mut() {
                                    props.insert("value".to_string(), value_schema);
                                }
                            }
                        }
                    }
                    base
                }
                "result" => {
                    let mut base = type_name_to_schema("result");
                    if args.len() == 2 {
                        let ok_schema = type_ref_to_schema(&args[0]);
                        let err_schema = type_ref_to_schema(&args[1]);
                        if let Some(obj) = base.as_object_mut() {
                            if let Some(props) = obj.get_mut("properties") {
                                if let Some(props) = props.as_object_mut() {
                                    props.insert(
                                        "value".to_string(),
                                        json!({ "oneOf": [ok_schema, err_schema] }),
                                    );
                                }
                            }
                        }
                    }
                    base
                }
                other => type_name_to_schema(other),
            };

            if let Some(obj) = schema.as_object_mut() {
                if !args.is_empty() {
                    obj.insert(
                        "x-at-type-args".to_string(),
                        json!(args.iter().map(format_type_ref).collect::<Vec<_>>()),
                    );
                }
            }

            schema
        }
        TypeRef::Tuple { items, .. } => json!({
            "type": "string",
            "x-at-type": format_type_ref(ty),
            "x-at-type-items": items.iter().map(format_type_ref).collect::<Vec<_>>(),
        }),
        TypeRef::Function {
            params, return_ty, ..
        } => json!({
            "type": "string",
            "x-at-type": format_type_ref(ty),
            "x-at-type-params": params.iter().map(format_type_ref).collect::<Vec<_>>(),
            "x-at-type-return": format_type_ref(return_ty),
        }),
    }
}

fn type_name_to_schema(name: &str) -> serde_json::Value {
    match name {
        "int" => json!({ "type": "integer" }),
        "bool" => json!({ "type": "boolean" }),
        "string" => json!({ "type": "string" }),
        "array" => json!({ "type": "array" }),
        "option" => json!({
            "type": "object",
            "properties": {
                "kind": { "type": "string", "enum": ["some", "none"] },
                "value": {}
            },
            "required": ["kind"],
            "additionalProperties": true
        }),
        "result" => json!({
            "type": "object",
            "properties": {
                "kind": { "type": "string", "enum": ["ok", "err"] },
                "value": {}
            },
            "required": ["kind"],
            "additionalProperties": true
        }),
        _ => json!({ "type": "string", "x-at-type": name }),
    }
}

fn format_type_ref(ty: &TypeRef) -> String {
    match ty {
        TypeRef::Named { name, args } => {
            if args.is_empty() {
                return name.name.clone();
            }
            let args = args
                .iter()
                .map(format_type_ref)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", name.name, args)
        }
        TypeRef::Function {
            params, return_ty, ..
        } => {
            let params = params
                .iter()
                .map(format_type_ref)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({params}) -> {}", format_type_ref(return_ty))
        }
        TypeRef::Tuple { items, .. } => {
            let inner = items
                .iter()
                .map(format_type_ref)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({inner})")
        }
    }
}

fn json_to_value(value: &JsonValue, ty: Option<&str>) -> Result<Value, String> {
    let base_ty = ty.and_then(|ty| ty.split('<').next());
    match base_ty {
        Some("int") => value
            .as_i64()
            .map(Value::Int)
            .ok_or_else(|| "expected integer".to_string()),
        Some("float") => value
            .as_f64()
            .map(Value::Float)
            .ok_or_else(|| "expected float".to_string()),
        Some("bool") => value
            .as_bool()
            .map(Value::Bool)
            .ok_or_else(|| "expected boolean".to_string()),
        Some("string") => value
            .as_str()
            .map(|text| Value::String(Rc::new(text.to_string())))
            .ok_or_else(|| "expected string".to_string()),
        Some("array") => {
            let items = value
                .as_array()
                .ok_or_else(|| "expected array".to_string())?;
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(json_to_value(item, None)?);
            }
            Ok(Value::Array(Rc::new(values)))
        }
        Some("option") => {
            if value.is_null() {
                Ok(Value::Option(None))
            } else {
                let inner = json_to_value(value, None)?;
                Ok(Value::Option(Some(Rc::new(inner))))
            }
        }
        Some("result") => {
            let obj = value
                .as_object()
                .ok_or_else(|| "expected result object".to_string())?;
            let kind = obj
                .get("kind")
                .and_then(|value| value.as_str())
                .ok_or_else(|| "result kind missing".to_string())?;
            let inner = obj.get("value").unwrap_or(&JsonValue::Null);
            let inner_value = json_to_value(inner, None)?;
            match kind {
                "ok" => Ok(Value::Result(Ok(Rc::new(inner_value)))),
                "err" => Ok(Value::Result(Err(Rc::new(inner_value)))),
                _ => Err("invalid result kind".to_string()),
            }
        }
        _ => match value {
            JsonValue::Bool(value) => Ok(Value::Bool(*value)),
            JsonValue::String(value) => Ok(Value::String(Rc::new(value.clone()))),
            JsonValue::Number(value) => {
                if let Some(int_val) = value.as_i64() {
                    Ok(Value::Int(int_val))
                } else if let Some(float_val) = value.as_f64() {
                    Ok(Value::Float(float_val))
                } else {
                    Err("expected number".to_string())
                }
            }
            JsonValue::Array(items) => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(json_to_value(item, None)?);
                }
                Ok(Value::Array(Rc::new(values)))
            }
            JsonValue::Null => Ok(Value::Unit),
            _ => Err("unsupported argument type".to_string()),
        },
    }
}

fn value_to_json(value: Value) -> JsonValue {
    match value {
        Value::Int(value) => JsonValue::from(value),
        Value::Float(value) => JsonValue::from(value),
        Value::Bool(value) => JsonValue::from(value),
        Value::String(value) => JsonValue::from(value.as_ref().clone()),
        Value::Unit => JsonValue::Null,
        Value::Array(items) => {
            JsonValue::Array(items.iter().cloned().map(value_to_json).collect::<Vec<_>>())
        }
        Value::Struct(fields) => {
            let mut obj = serde_json::Map::new();
            for (key, value) in fields.iter() {
                obj.insert(key.clone(), value_to_json(value.clone()));
            }
            JsonValue::Object(obj)
        }
        Value::Enum {
            name,
            variant,
            payload,
        } => {
            let mut obj = serde_json::Map::new();
            obj.insert("enum".to_string(), JsonValue::String(name.clone()));
            obj.insert("variant".to_string(), JsonValue::String(variant.clone()));
            if let Some(payload) = payload {
                obj.insert("value".to_string(), value_to_json((*payload).clone()));
            }
            JsonValue::Object(obj)
        }
        Value::Tuple(items) => {
            JsonValue::Array(items.iter().cloned().map(value_to_json).collect::<Vec<_>>())
        }
        Value::Closure(_) => JsonValue::String("<closure>".to_string()),
        Value::Option(Some(inner)) => value_to_json((*inner).clone()),
        Value::Option(None) => JsonValue::Null,
        Value::Result(Ok(inner)) => json!({
            "kind": "ok",
            "value": value_to_json((*inner).clone())
        }),
        Value::Result(Err(inner)) => json!({
            "kind": "err",
            "value": value_to_json((*inner).clone())
        }),
    }
}

fn print_deps_tree(
    path: &Path,
    indent: usize,
    visited: &mut HashSet<PathBuf>,
) -> Result<(), String> {
    let normalized = path
        .canonicalize()
        .map_err(|err| format!("error reading {}: {err}", path.display()))?;
    let prefix = " ".repeat(indent);
    if !visited.insert(normalized.clone()) {
        println!("{}{} (cycle)", prefix, normalized.display());
        return Ok(());
    }

    println!("{}{}", prefix, normalized.display());
    let source = fs::read_to_string(&normalized)
        .map_err(|err| format!("error reading {}: {err}", normalized.display()))?;
    let normalized_display = normalized.display().to_string();
    let module = parse_module(&source)
        .map_err(|err| format_parse_error(&err, &source, Some(&normalized_display)))?;
    let base_dir = normalized
        .parent()
        .ok_or_else(|| format!("invalid path: {}", normalized.display()))?;

    for stmt in module.stmts {
        if let Stmt::Import { path, .. } = stmt {
            let import_path = if path == "std" || path == "std.at" {
                Path::new("stdlib").join("std.at")
            } else if path.starts_with("http://") || path.starts_with("https://") {
                fetch_remote(&path, base_dir)?
            } else {
                base_dir.join(path)
            };
            print_deps_tree(&import_path, indent + 2, visited)?;
        }
    }

    Ok(())
}
