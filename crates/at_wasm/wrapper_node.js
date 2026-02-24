const { run: runRaw } = require("./pkg/at_wasm.js");

function run(source, options = {}) {
  const maxInstructions =
    typeof options.maxInstructions === "number"
      ? options.maxInstructions
      : undefined;
  const raw = runRaw(source, maxInstructions);
  return JSON.parse(raw);
}

module.exports = { run };
