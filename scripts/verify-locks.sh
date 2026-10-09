#!/usr/bin/env bash
# Assert flake.lock and devenv.lock agree on every shared input's rev.
#
# Diverged revs make the dev shell and the built Emacs resolve different
# sources and miss the binary caches; Dependabot bumps flake.lock alone.
#
# Nodes are resolved through each lock's root input map, not by name: the
# two files name the same input differently once transitive dependencies
# collide.
#
# Used by `just verify` and the `locks-in-sync` flake check.
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
