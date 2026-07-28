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
#   4. substitute the flake.lock input source trees from the binary
#      caches, so flake-CLI builds work even when GitHub tarball
#      downloads are blocked (see prefetch_flake_sources below)
#
# Idempotent: when nix is already on PATH it only re-runs the cheap
# source prefetch, so it is safe as a SessionStart hook (hooks re-run
# on every session resume). Wired up in .claude/settings.json; run it
# manually anywhere else.
set -euo pipefail

PROFILE_BIN="$HOME/.nix-profile/bin"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

persist_path() {
    # Claude Code exports $CLAUDE_ENV_FILE for hooks; KEY=value lines
    # written there persist into subsequent shell commands.
    if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
        echo "PATH=$PROFILE_BIN:$PATH" >>"$CLAUDE_ENV_FILE"
    fi
}

prefetch_flake_sources() {
    # Agent-session egress proxies commonly 403 the github.com tarball
    # downloads every eval-time fetch here resolves to, while the Nix
    # binary caches stay reachable. The locked source *trees* themselves
    # are substitutable by narHash: nixpkgs from cache.nixos.org,
    # emacs-overlay and flake-compat from nix-community.cachix.org. With
    # those store paths pre-seeded, the flake CLI resolves locked inputs
    # from the store and never touches GitHub — `nix build .#default -o
    # result` and the per-check `nix build .#checks.<system>.<name>`
    # builds all work offline.
    #
    # Only the flake CLI, though: `nix-build` via default.nix goes
    # through flake-compat's builtins.fetchTree, which re-downloads by
    # URL even when the narHash store path exists — so `just build` /
    # `just screenshot` / `just run-built` still need the flake-CLI
    # equivalents in such sessions (`nix build .#default -o result`,
    # then the recipe's remaining steps against ./result).
    #
    # Best-effort by design: a MISS (input absent from every cache) only
    # matters if the build actually forces that input, and a network
    # failure here should never fail the SessionStart hook.
    local lock="$REPO_ROOT/flake.lock"
    [ -f "$lock" ] || return 0
    command -v python3 >/dev/null 2>&1 || {
        echo "bootstrap: python3 not found, skipping source prefetch"
        return 0
    }
    echo "bootstrap: prefetching flake.lock sources from binary caches…"
    python3 - "$lock" <<'EOF' |
import json, sys
lock = json.load(open(sys.argv[1]))
for name, node in lock["nodes"].items():
    if name == "root":
        continue
    nar = (node.get("locked") or {}).get("narHash")
    if nar:
        print(name, nar)
EOF
    while read -r name nar; do
        h32=$(nix hash convert --hash-algo sha256 --to nix32 "$nar" 2>/dev/null ||
            nix hash to-base32 --type sha256 "$nar" 2>/dev/null) || {
            echo "  skip $name (cannot convert $nar)"
            continue
        }
        path=$(nix-store --print-fixed-path --recursive sha256 "$h32" source)
        if nix-store -r "$path" >/dev/null 2>&1; then
            echo "  ok   $name"
        else
            echo "  miss $name (not in any cache; needed only if a build forces it)"
        fi
    done || echo "bootstrap: source prefetch failed (non-fatal)"
}

if command -v nix >/dev/null 2>&1; then
    # Already bootstrapped (or a real Nix machine) — only refresh the
    # source prefetch (cheap no-op when the paths are already valid).
    [ -x "$PROFILE_BIN/nix" ] && persist_path
    echo "bootstrap: nix already present ($(command -v nix))"
    prefetch_flake_sources
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

prefetch_flake_sources

echo "bootstrap: done — $(nix --version), just $(just --version)"
echo "bootstrap: cheap verification targets:"
echo "  nix build --no-link .#checks.x86_64-linux.{formatting,statix,deadnix,module-eval}"
echo "bootstrap: if GitHub tarball downloads are blocked (agent proxy 403),"
echo "  use the flake CLI, which resolves the prefetched sources offline:"
echo "  nix build .#default -o result   # instead of just build / nix-build"
