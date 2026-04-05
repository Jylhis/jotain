#!/usr/bin/env bash
# setup-web-env.sh — Bootstrap Nix + devenv + just for Claude Code Web containers
# Idempotent: safe to re-run. Skips steps already completed.
#
# The Claude Code web environment (Ubuntu 24.04) has no Nix pre-installed and
# the Determinate Systems installer is network-blocked. This script bootstraps
# Nix via apt (single-user mode), then installs devenv and just so the full
# devenv-based workflow works.
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
  export DEBIAN_FRONTEND=noninteractive
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
experimental-features = nix-command

# Binary caches — cache.nixos.org is always available; cachix.org caches
# are included for environments where they are reachable (local dev) but
# may be blocked by the Claude Code web egress proxy.
substituters = https://cache.nixos.org https://nix-community.cachix.org https://devenv.cachix.org https://jylhis.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw= jylhis.cachix.org-1:H3THGMN5xOFJiFGuB2o3Su/aOcmgKMPBJ+OP/ECQHIM=

# Fall back gracefully when a substituter is unreachable
connect-timeout = 5
fallback = true
NIXCONF
else
  echo "[setup] Nix config already present."
fi

# --- Section 4: Set up nixpkgs-unstable channel ---
# Use channels.nixos.org (not nixos.org) — the bare domain is blocked by the
# Claude Code web egress proxy, but *.nixos.org subdomains are allowed.
# nixpkgs-unstable is required for latest packages (e.g. emacs30, devenv).
if ! NIX_REMOTE="" nix-instantiate --eval -E '<nixpkgs>' &>/dev/null; then
  echo "[setup] Adding nixpkgs-unstable channel..."
  nix-channel --add https://channels.nixos.org/nixpkgs-unstable nixpkgs
  nix-channel --update
else
  echo "[setup] nixpkgs channel already available."
fi

# --- Section 5: Install devenv from nixpkgs ---
if ! command -v devenv &>/dev/null; then
  echo "[setup] Installing devenv from nixpkgs..."
  nix-env -iA nixpkgs.devenv 2>&1 | tail -1
  export PATH="/root/.nix-profile/bin:$PATH"
else
  echo "[setup] devenv already installed."
fi

# --- Section 6: Export NIX_REMOTE and PATH for the session ---
# Persist NIX_REMOTE and PATH so all subsequent commands use single-user Nix
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  if ! grep -q 'NIX_REMOTE' "$CLAUDE_ENV_FILE" 2>/dev/null; then
    echo 'export NIX_REMOTE=""' >> "$CLAUDE_ENV_FILE"
  fi
  if ! grep -q 'nix-profile' "$CLAUDE_ENV_FILE" 2>/dev/null; then
    echo 'export PATH="/root/.nix-profile/bin:$PATH"' >> "$CLAUDE_ENV_FILE"
  fi
fi

# --- Section 7: Verify setup ---
echo "[setup] Verifying environment..."
echo "  nix: $(nix-instantiate --version)"
echo "  just: $(just --version)"
echo "  devenv: $(devenv version 2>/dev/null || echo 'not found')"

# --- Section 8: Validate project Nix expressions ---
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
