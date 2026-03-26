#!/usr/bin/env bash
# PreToolUse (Bash) hook — web only: block commands that destroy the Nix store.
set -euo pipefail

# Only enforce in web environments
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

if echo "$COMMAND" | grep -qE 'nix-collect-garbage|nix store gc|nix.store.gc|rm -rf /nix'; then
  echo "Blocked: destroying the Nix store in a web environment requires a full reinstall (~5 min). Use targeted commands instead." >&2
  exit 2
fi

exit 0
