const { run } = require("./wrapper_node.js");

const source = `
fn add(a: int, b: int) -> int {
    return a + b;
}

add(40, 2);
`;

const result = run(source, { maxInstructions: 10000 });
console.log(result);
