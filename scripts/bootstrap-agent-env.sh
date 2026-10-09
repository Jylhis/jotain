#!/usr/bin/env bash
# Bootstrap Nix in a bare container (AI agent sessions, throwaway VMs).
#
# From a stock Ubuntu/Debian image (run as root):
#
#   1. apt install nix-bin (distro Nix, old but enough to bootstrap)
#   2. /etc/nix/nix.conf: flakes + the caches this repo builds against
#   3. use that Nix to install current nix + just from nixpkgs
#   4. substitute the flake.lock input source trees from the binary
#      caches, so flake-CLI builds work even when GitHub tarball
#      downloads are blocked (see prefetch_flake_sources below)
#
# Idempotent: with nix already on PATH it only re-runs the cheap
# prefetch, so it is safe as the remote-session SessionStart hook in
# .claude/settings.json (hooks re-run on every resume).
set -euo pipefail

PROFILE_BIN="$HOME/.nix-profile/bin"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

persist_path() {
    # KEY=value lines in Claude Code's $CLAUDE_ENV_FILE persist into
    # later shell commands.
    if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
        echo "PATH=$PROFILE_BIN:$PATH" >>"$CLAUDE_ENV_FILE"
    fi
}

prefetch_flake_sources() {
    # Agent egress proxies often 403 github.com tarball downloads while
    # the binary caches stay reachable. The locked source trees are
    # substitutable by narHash (nixpkgs from cache.nixos.org,
    # emacs-overlay and flake-compat from nix-community.cachix.org), so
    # once pre-seeded the flake CLI (`nix build .#default -o result`,
    # `nix build .#checks.<system>.<name>`) never touches GitHub.
    #
    # `nix-build` via default.nix does not benefit: flake-compat's
    # builtins.fetchTree re-downloads by URL anyway, so `just build`,
    # `just screenshot` and `just run-built` need the flake-CLI build and
    # then the recipe's remaining steps against ./result.
    #
    # Best-effort: a miss matters only if a build forces that input, and
    # a network failure must never fail the SessionStart hook.
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

# nix-community carries the emacs-overlay builds (emacs-unstable, the
# default base); jylhis carries the repo's own artifacts.
echo "bootstrap: writing /etc/nix/nix.conf…"
mkdir -p /etc/nix
cat >/etc/nix/nix.conf <<'EOF'
experimental-features = nix-command flakes
substituters = https://cache.nixos.org https://nix-community.cachix.org https://jylhis.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= jylhis.cachix.org-1:SIAw5iWjXRhLAmejqPy0PGuqH6bjCHIFVF9CiHmHRpE=
max-jobs = auto
EOF

# Distro nix only seeds current nix + just. devenv is left out: install
# it with `nix profile install nixpkgs#devenv` when a task needs it.
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
