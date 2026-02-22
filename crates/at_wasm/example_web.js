import init, { run } from "./pkg/at_wasm.js";

async function main() {
  await init();
  const source = `
fn add(a: int, b: int) -> int {
    return a + b;
}

add(40, 2);
`;
  const output = run(source);
  const target = document.getElementById("output");
  if (target) {
    target.textContent = output;
  }
}

main();
