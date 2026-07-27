#!/usr/bin/env bash
# Bootstrap Nix in a bare container (AI agent sessions, throwaway VMs).
#
# Fresh Claude Code cloud containers have no nix, just, devenv, or
# emacs — every Justfile recipe and flake check is unrunnable until
# Nix exists. This script gets a working toolchain from a stock
# Ubuntu/Debian image:
#
#   1. apt install nix-bin (distro Nix, old but enough to bootstrap)
#   2. /etc/nix/nix.conf: flakes + the caches this repo builds against
#   3. use that Nix to install current nix + just from nixpkgs
#
# Idempotent: exits immediately when nix is already on PATH, so it is
# safe as a SessionStart hook (hooks re-run on every session resume).
# Wired up in .claude/settings.json; run it manually anywhere else.
set -euo pipefail

PROFILE_BIN="$HOME/.nix-profile/bin"

persist_path() {
    # Claude Code exports $CLAUDE_ENV_FILE for hooks; KEY=value lines
    # written there persist into subsequent shell commands.
    if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
        echo "PATH=$PROFILE_BIN:$PATH" >>"$CLAUDE_ENV_FILE"
    fi
}

if command -v nix >/dev/null 2>&1; then
    # Already bootstrapped (or a real Nix machine) — nothing to do.
    [ -x "$PROFILE_BIN/nix" ] && persist_path
    echo "bootstrap: nix already present ($(command -v nix)), skipping"
    exit 0
fi

if [ "$(id -u)" -ne 0 ]; then
    echo "bootstrap: no nix and not root — install Nix manually:" >&2
    echo "  https://nixos.org/download (multi-user installer)" >&2
    exit 1
fi

if ! command -v apt-get >/dev/null 2>&1; then
    echo "bootstrap: no nix and no apt-get — install Nix manually:" >&2
    echo "  https://nixos.org/download" >&2
    exit 1
fi

echo "bootstrap: installing distro nix-bin via apt…"
apt-get update -qq
DEBIAN_FRONTEND=noninteractive apt-get install -y -qq nix-bin

# The caches this repo builds against (see CLAUDE.md "Nix build layer"):
# nix-community carries the emacs-overlay variants (emacs-unstable, the
# distribution default base); jylhis carries the repo's own artifacts.
echo "bootstrap: writing /etc/nix/nix.conf…"
mkdir -p /etc/nix
cat >/etc/nix/nix.conf <<'EOF'
experimental-features = nix-command flakes
substituters = https://cache.nixos.org https://nix-community.cachix.org https://jylhis.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= jylhis.cachix.org-1:SIAw5iWjXRhLAmejqPy0PGuqH6bjCHIFVF9CiHmHRpE=
max-jobs = auto
EOF

# Distro nix (2.18-era) is only the seed: use it to install current
# nix plus the task runner from nixpkgs. devenv is intentionally not
# installed here — pull it in with `nix profile install nixpkgs#devenv`
# when a task actually needs `devenv test` or `devenv update`.
echo "bootstrap: installing current nix + just from nixpkgs…"
nix profile install nixpkgs#nix nixpkgs#just

export PATH="$PROFILE_BIN:$PATH"
persist_path

echo "bootstrap: done — $(nix --version), just $(just --version)"
echo "bootstrap: cheap verification targets:"
echo "  nix build --no-link .#checks.x86_64-linux.{formatting,statix,deadnix,module-eval}"
