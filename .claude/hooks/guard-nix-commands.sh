#!/usr/bin/env bash
# PreToolUse (Bash) hook — web only: verify Nix is available before nix/just commands.
set -euo pipefail

# Only enforce in web environments
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

# Check if this is a nix or just command
if echo "$COMMAND" | grep -qE '^\s*(nix |just )'; then
  if ! command -v nix &>/dev/null; then
    # Try sourcing the profile
    if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then
      # shellcheck disable=SC1091
      . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    fi

    if ! command -v nix &>/dev/null; then
      echo "Nix is not available. Run the setup script first: ${CLAUDE_PROJECT_DIR}/scripts/setup-nix.sh" >&2
      exit 2
    fi
  fi
fi

exit 0
