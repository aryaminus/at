use at_parser::parse_module;
use at_vm::{format_value, Compiler, Vm, VmError};
use console_error_panic_hook::set_once;
use wasm_bindgen::prelude::*;

/// Initialize the WASM module.
/// Call this once at startup for better error messages.
#[wasm_bindgen(start)]
pub fn init() {
    set_once();
}

/// Run AT source code and return structured JSON result.
///
/// The returned JSON has the following structure:
/// ```json
/// {
///   "status": "ok" | "parse_error" | "compile_error" | "runtime_error" | "execution_limit",
///   "value": "...",        // Only present when status is "ok"
///   "output": ["..."],     // Captured print output (array of strings)
///   "error": "..."         // Only present on error
/// }
/// ```
#[wasm_bindgen]
pub fn run(source: &str, max_instructions: Option<usize>) -> String {
    run_inner(source, max_instructions)
}

fn run_inner(source: &str, max_instructions: Option<usize>) -> String {
    let module = match parse_module(source) {
        Ok(module) => module,
        Err(err) => {
            return serde_json::json!({
                "status": "parse_error",
                "error": format!("{err:?}"),
                "output": []
            })
            .to_string();
        }
    };

    let mut compiler = Compiler::new();
    let program = match compiler.compile_module(&module) {
        Ok(program) => program,
        Err(err) => {
            return serde_json::json!({
                "status": "compile_error",
                "error": format!("{err:?}"),
                "output": []
            })
            .to_string();
        }
    };

    // Create VM with output capture
    let mut vm = if let Some(max) = max_instructions {
        Vm::with_execution_limit_and_output(max)
    } else {
        Vm::with_output_capture()
    };

    match vm.run(&program) {
        Ok(Some(value)) => {
            let output = vm.get_output().unwrap_or_default();
            serde_json::json!({
                "status": "ok",
                "value": format_value(&value),
                "output": output
            })
            .to_string()
        }
        Ok(None) => {
            let output = vm.get_output().unwrap_or_default();
            serde_json::json!({
                "status": "ok",
                "value": null,
                "output": output
            })
            .to_string()
        }
        Err(VmError::ExecutionLimit { message }) => {
            let output = vm.get_output().unwrap_or_default();
            serde_json::json!({
                "status": "execution_limit",
                "error": message,
                "output": output
            })
            .to_string()
        }
        Err(err) => {
            let output = vm.get_output().unwrap_or_default();
            serde_json::json!({
                "status": "runtime_error",
                "error": format!("{err:?}"),
                "output": output
            })
            .to_string()
        }
    }
}

#[wasm_bindgen(typescript_custom_section)]
const TS_TYPES: &str = r#"
export interface AtResult {
  status: 'ok' | 'parse_error' | 'compile_error' | 'runtime_error' | 'execution_limit';
  value?: string | null;
  output: string[];
  error?: string;
}

export function run(source: string, maxInstructions?: number): string;
"#;
