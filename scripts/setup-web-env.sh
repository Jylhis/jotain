#!/usr/bin/env bash
# setup-web-env.sh — Bootstrap Nix + just for Claude Code Web containers
# Idempotent: safe to re-run. Skips steps already completed.
#
# This script replaces devenv for the Claude Code web environment where
# the Determinate Systems installer is network-blocked.
set -euo pipefail

# Force single-user Nix (no daemon) since the container lacks systemd
export NIX_REMOTE=""

# --- Section 1: Install nix-bin and just from apt ---
NEED_APT=false
if ! command -v nix-instantiate &>/dev/null; then
  NEED_APT=true
fi
if ! command -v just &>/dev/null; then
  NEED_APT=true
fi

if [ "$NEED_APT" = true ]; then
  echo "[setup] Installing packages from apt..."
  apt-get update -qq
  if ! command -v nix-instantiate &>/dev/null; then
    apt-get install -y -qq nix-bin
  fi
  if ! command -v just &>/dev/null; then
    apt-get install -y -qq just
  fi
else
  echo "[setup] nix-bin and just already installed."
fi

# --- Section 2: Initialize /nix/store ---
if [ ! -d /nix/store ] || [ -z "$(ls -A /nix/store 2>/dev/null)" ]; then
  echo "[setup] Initializing Nix store..."
  nix-store --init
else
  echo "[setup] Nix store already initialized."
fi

# --- Section 3: Configure Nix for single-user mode ---
NIX_CONF="/etc/nix/nix.conf"
if [ ! -f "$NIX_CONF" ] || ! grep -q "sandbox = false" "$NIX_CONF"; then
  echo "[setup] Configuring Nix (single-user, no sandbox)..."
  mkdir -p /etc/nix
  cat > "$NIX_CONF" <<'NIXCONF'
sandbox = false
max-jobs = auto
substituters = https://cache.nixos.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
experimental-features = nix-command
NIXCONF
else
  echo "[setup] Nix config already present."
fi

# --- Section 4: Set up nixpkgs channel ---
# Use channels.nixos.org (not nixos.org) since the bare domain is blocked
if ! NIX_REMOTE="" nix-instantiate --eval -E '<nixpkgs>' &>/dev/null; then
  echo "[setup] Adding nixpkgs channel..."
  nix-channel --add https://channels.nixos.org/nixpkgs-unstable nixpkgs
  nix-channel --update
else
  echo "[setup] nixpkgs channel already available."
fi

# --- Section 5: Export NIX_REMOTE for the session ---
# Persist NIX_REMOTE="" so all subsequent nix commands use single-user mode
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  if ! grep -q 'NIX_REMOTE' "$CLAUDE_ENV_FILE" 2>/dev/null; then
    echo 'export NIX_REMOTE=""' >> "$CLAUDE_ENV_FILE"
  fi
fi

# --- Section 6: Verify setup ---
echo "[setup] Verifying environment..."
echo "  nix: $(nix-instantiate --version)"
echo "  just: $(just --version)"

# --- Section 7: Validate project Nix expressions ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

if [ -f "$PROJECT_DIR/default.nix" ]; then
  echo "[setup] Evaluating Nix expressions..."
  if NIX_REMOTE="" nix-instantiate "$PROJECT_DIR/default.nix" &>/dev/null; then
    echo "  default.nix: OK"
  else
    echo "  default.nix: FAILED (non-fatal, may need channel update)"
  fi

  if NIX_REMOTE="" nix-instantiate "$PROJECT_DIR/emacs.nix" &>/dev/null; then
    echo "  emacs.nix: OK"
  else
    echo "  emacs.nix: FAILED (non-fatal)"
  fi
fi

echo "[setup] Environment ready."
