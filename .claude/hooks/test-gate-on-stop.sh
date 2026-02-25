#!/usr/bin/env bash
# Stop hook — desktop only: verify fast tests pass before Claude finishes.
set -euo pipefail

# Skip in web environments — test infra requires full dev shell
if [ "${CLAUDE_CODE_REMOTE:-}" = "true" ]; then
  exit 0
fi

INPUT=$(cat)

# Avoid infinite loop: if this hook already triggered a continuation, let Claude stop
if [ "$(echo "$INPUT" | jq -r '.stop_hook_active')" = "true" ]; then
  exit 0
fi

cd "${CLAUDE_PROJECT_DIR:-$PWD}"

# Only run if just is available
if ! command -v just &>/dev/null; then
  exit 0
fi

# Run fast tests; block stop if they fail
if ! just test-fast 2>&1 | tail -20; then
  echo "Fast tests failed. Fix the failures before finishing." >&2
  exit 2
fi

exit 0
