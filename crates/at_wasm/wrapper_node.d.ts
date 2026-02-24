export interface AtRunOptions {
  maxInstructions?: number;
}

export interface AtResult {
  status: "ok" | "parse_error" | "compile_error" | "runtime_error" | "execution_limit";
  value?: string | null;
  output: string[];
  error?: string;
}

export function run(source: string, options?: AtRunOptions): AtResult;
