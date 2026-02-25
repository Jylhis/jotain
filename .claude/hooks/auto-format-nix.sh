#!/usr/bin/env bash
# PostToolUse (Edit|Write) hook — desktop only: auto-format .nix files after edits.
set -euo pipefail

# Skip in web environments — nixfmt may not be available without dev shell
if [ "${CLAUDE_CODE_REMOTE:-}" = "true" ]; then
  exit 0
fi

INPUT=$(cat)
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

if [ -z "$FILE" ]; then
  exit 0
fi

if [[ $FILE == *.nix ]]; then
  if command -v nixfmt &>/dev/null; then
    nixfmt "$FILE" 2>/dev/null || true
  fi
fi

exit 0
