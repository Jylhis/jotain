#!/usr/bin/env bash
# PostToolUse (Edit|Write) hook — desktop only: run smoke tests after .el file edits.
set -euo pipefail

# Skip in web environments — test infrastructure requires full dev shell
if [ "${CLAUDE_CODE_REMOTE:-}" = "true" ]; then
  exit 0
fi

INPUT=$(cat)
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

if [ -z "$FILE" ]; then
  exit 0
fi

if [[ $FILE == *.el ]]; then
  cd "${CLAUDE_PROJECT_DIR:-$PWD}"
  if command -v just &>/dev/null; then
    just test-smoke 2>&1 | tail -5
  fi
fi

exit 0
