#!/usr/bin/env bash
# Assert flake.lock and devenv.lock agree on every shared input's rev.
#
# flake.nix and devenv.yaml both pin nixpkgs, treefmt-nix and
# emacs-overlay. They must land on the same revs or the dev shell and
# the built Emacs resolve different sources and miss the binary caches.
# Dependabot's "nix" ecosystem bumps flake.lock alone and knows nothing
# about devenv.yaml/devenv.lock, so the two drift apart silently without
# a gate.
#
# Nodes are resolved through each lock's own root input map rather than
# by attribute name, because the two files name the same input
# differently once transitive dependencies collide.
#
# Two consumers, one implementation: `just verify` (fast, no nix build)
# and the `locks-in-sync` flake check in nix/checks.nix (which is what
# covers pushes to main/next, since nix flake check never reads
# devenv.lock otherwise).
#
# Usage: verify-locks.sh [repo-root]      # default: current directory
#        SHARED_INPUTS="a b" verify-locks.sh
set -euo pipefail

root="${1:-.}"
shared_inputs="${SHARED_INPUTS:-nixpkgs treefmt-nix emacs-overlay}"

flake_lock="$root/flake.lock"
devenv_lock="$root/devenv.lock"

for f in "$flake_lock" "$devenv_lock"; do
    if [ ! -f "$f" ]; then
        echo "ERROR: $f not found" >&2
        exit 1
    fi
done

fail=0
for input in $shared_inputs; do
    flake_node=$(jq -r ".nodes.root.inputs.\"$input\" // empty" "$flake_lock")
    devenv_node=$(jq -r ".nodes.root.inputs.\"$input\" // empty" "$devenv_lock")
    if [ -z "$flake_node" ] || [ -z "$devenv_node" ]; then
        echo "FAIL: $input missing from a lock file's root inputs"
        echo "  flake node:  ${flake_node:-<missing>}"
        echo "  devenv node: ${devenv_node:-<missing>}"
        fail=1
        continue
    fi
    flake_rev=$(jq -r ".nodes.\"$flake_node\".locked.rev" "$flake_lock")
    devenv_rev=$(jq -r ".nodes.\"$devenv_node\".locked.rev" "$devenv_lock")
    if [ "$flake_rev" != "$devenv_rev" ]; then
        echo "FAIL: $input revs diverged"
        echo "  flake:  $flake_rev"
        echo "  devenv: $devenv_rev"
        fail=1
    else
        echo "OK: $input -> $flake_rev"
    fi
done

exit $fail
