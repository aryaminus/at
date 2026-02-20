#!/bin/sh
set -e

echo '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"now","arguments":{}}}' |
  at mcp-server examples/mcp_tools.at
