# Package manager

The `at` CLI uses URL imports with a content-hash cache.

Remote fetches are hardened with request timeouts, bounded retries, and a
response-size limit.

## Cache directory

Remote modules are cached under `.at/cache` as `<sha256>.at`.

## Lockfile format

The lockfile is stored at `.at/lock` and is a simple, line-based format.

```
version 1
https://example.com/foo.at 0123abc...
https://example.com/bar.at 9def456...
```

Rules:
- The first line is the lockfile version (currently `1`).
- Each following line is `URL HASH` separated by whitespace.
- Lines starting with `#` are ignored.

The loader accepts legacy lockfiles without a version header and treats them
as version 1.

## Cache eviction

Unreferenced cache files are removed by `at cache prune`. When limits are
specified, the command removes unreferenced files first, oldest to newest.
If referenced files alone exceed limits, the command keeps them and prints a
warning.

Examples:

```
at cache prune
at cache prune --max 100
at cache prune --max-files 100 --max-mb 200
at cache prune --max-files 100 --max-mb 200 --max-age-days 30
```
