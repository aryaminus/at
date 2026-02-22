const { run } = require("./pkg/at_wasm.js");

const source = `
fn add(a: int, b: int) -> int {
    return a + b;
}

add(40, 2);
`;

console.log(run(source));
