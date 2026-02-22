#!/usr/bin/env python3
import json
import re
import sys
from pathlib import Path


def bump_version(version: str, level: str) -> str:
    major, minor, patch = [int(part) for part in version.split(".")]
    if level == "major":
        major += 1
        minor = 0
        patch = 0
    elif level == "minor":
        minor += 1
        patch = 0
    else:
        patch += 1
    return f"{major}.{minor}.{patch}"


def main() -> int:
    level = sys.argv[1] if len(sys.argv) > 1 else "patch"
    root = Path(__file__).resolve().parents[1]

    cargo_path = root / "Cargo.toml"
    cargo_text = cargo_path.read_text()
    match = re.search(
        r"(\[workspace\.package\][\s\S]*?version\s*=\s*\")"
        r"(\d+\.\d+\.\d+)(\")",
        cargo_text,
    )
    if not match:
        raise SystemExit("workspace.package version not found")

    current = match.group(2)
    new_version = bump_version(current, level)
    updated = cargo_text[: match.start(2)] + new_version + cargo_text[match.end(2) :]
    cargo_path.write_text(updated)

    package_path = root / "package.json"
    if package_path.exists():
        data = json.loads(package_path.read_text())
        data["version"] = new_version
        package_path.write_text(json.dumps(data, indent=2) + "\n")

    print(new_version)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
