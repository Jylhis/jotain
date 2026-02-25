#!/usr/bin/env bash
# SessionStart (compact) hook: re-inject Nix/project context after compaction.
# Runs in both web and desktop environments.
set -euo pipefail

cat <<'CTX'
Nix is installed. Key project commands:
- `just test-smoke`  — sub-second smoke tests
- `just test-fast`   — fast unit tests (< 5s)
- `just test`        — full ERT suite via Nix
- `just format`      — format all files (nixfmt, yamlfmt, etc.)
- `just compile`     — byte-compile all .el files

All Emacs packages are managed via Nix — never MELPA/ELPA at runtime.
When adding use-package forms, check nix/lib/dependencies.nix for name mappings.
Runtime tools (LSP servers, fonts, CLI, tree-sitter grammars) go in nix/lib/runtime-deps.nix.
Do not edit flake.lock manually — use `nix flake update`.
CTX
