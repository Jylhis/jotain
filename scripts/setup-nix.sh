#!/usr/bin/env bash
# SessionStart hook: install Nix and configure the dev environment
# for Claude Code on the web. Only runs in remote environments.
set -euo pipefail

# ── Guard: only run in Claude Code remote (web) environments ───────────
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

log() { echo "[setup-nix] $*"; }

# ── Install Nix if missing ─────────────────────────────────────────────
if ! command -v nix &>/dev/null; then
  log "Installing Nix via Determinate Systems installer..."
  curl --proto '=https' --tlsv1.2 -sSf -L \
    https://install.determinate.systems/nix | sh -s -- install --no-confirm

  # Source the Nix daemon profile so this script can use nix immediately
  if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then
    # shellcheck disable=SC1091
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  fi
else
  log "Nix already installed, skipping installation."
fi

# Verify nix is available
if ! command -v nix &>/dev/null; then
  log "ERROR: Nix not found after installation attempt."
  exit 2
fi

log "Nix version: $(nix --version)"

# ── Ensure flakes are enabled ──────────────────────────────────────────
mkdir -p ~/.config/nix
if ! grep -q 'experimental-features' ~/.config/nix/nix.conf 2>/dev/null; then
  echo "experimental-features = nix-command flakes" >>~/.config/nix/nix.conf
  log "Enabled flakes in ~/.config/nix/nix.conf"
fi

# ── Persist Nix environment for subsequent Bash commands ───────────────
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  log "Persisting Nix environment to CLAUDE_ENV_FILE..."

  # Source Nix profile to capture all env changes
  ENV_BEFORE=$(export -p | sort)

  if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then
    # shellcheck disable=SC1091
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  fi

  ENV_AFTER=$(export -p | sort)
  comm -13 <(echo "$ENV_BEFORE") <(echo "$ENV_AFTER") >>"$CLAUDE_ENV_FILE"

  # Ensure PATH includes nix (single quotes intentional: write literal $PATH to env file)
  # shellcheck disable=SC2016
  echo 'export PATH="/nix/var/nix/profiles/default/bin:$PATH"' >>"$CLAUDE_ENV_FILE"
fi

# ── Quick flake evaluation check ──────────────────────────────────────
if [ -f "${CLAUDE_PROJECT_DIR:-$PWD}/flake.nix" ]; then
  log "Verifying flake evaluation..."
  if nix flake metadata "${CLAUDE_PROJECT_DIR:-$PWD}" &>/dev/null; then
    log "Flake evaluation OK."
  else
    log "WARNING: Flake evaluation failed. Check network access (Full internet required for Nix binary cache)."
  fi
fi

log "Nix setup complete."
exit 0
