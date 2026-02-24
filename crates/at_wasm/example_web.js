import { run } from "./wrapper_web.js";

function parseMaxInstructions(rawValue) {
  const trimmed = rawValue.trim();
  if (!trimmed) {
    return undefined;
  }
  const parsed = Number.parseInt(trimmed, 10);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error("max instructions must be a positive integer");
  }
  return parsed;
}

async function execute() {
  const sourceInput = document.getElementById("source");
  const maxInput = document.getElementById("max-instructions");
  const output = document.getElementById("output");
  if (!sourceInput || !maxInput || !output) {
    return;
  }

  output.textContent = "running...";
  try {
    const maxInstructions = parseMaxInstructions(maxInput.value);
    const options = {};
    if (maxInstructions !== undefined) {
      options.maxInstructions = maxInstructions;
    }
    const result = await run(sourceInput.value, options);
    output.textContent = JSON.stringify(result, null, 2);
  } catch (err) {
    output.textContent = JSON.stringify(
      {
        status: "error",
        error: err instanceof Error ? err.message : String(err),
      },
      null,
      2
    );
  }
}

function setup() {
  const runButton = document.getElementById("run-btn");
  if (runButton) {
    runButton.addEventListener("click", () => {
      void execute();
    });
  }
  void execute();
}

setup();
