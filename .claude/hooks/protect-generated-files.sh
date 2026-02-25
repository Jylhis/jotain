#!/usr/bin/env bash
# PreToolUse (Edit|Write) hook: block edits to generated/lockfiles.
set -euo pipefail

INPUT=$(cat)
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

if [ -z "$FILE" ]; then
  exit 0
fi

case "$FILE" in
*/flake.lock)
  echo "Blocked: flake.lock is generated — use 'nix flake update' instead." >&2
  exit 2
  ;;
*/result | */result/*)
  echo "Blocked: result/ is a Nix build output symlink — use 'just build' instead." >&2
  exit 2
  ;;
*.elc)
  echo "Blocked: .elc files are byte-compiled output — use 'just compile' instead." >&2
  exit 2
  ;;
esac

exit 0
