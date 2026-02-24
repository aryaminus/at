import initWasm, { run as runRaw } from "./pkg/at_wasm.js";

let initPromise = null;

async function ensureInit(wasmModule) {
  if (!initPromise) {
    initPromise = wasmModule ? initWasm(wasmModule) : initWasm();
  }
  await initPromise;
}

export async function run(source, options = {}) {
  await ensureInit(options.wasmModule);
  const maxInstructions =
    typeof options.maxInstructions === "number"
      ? options.maxInstructions
      : undefined;
  const raw = runRaw(source, maxInstructions);
  return JSON.parse(raw);
}
