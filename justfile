default:
    @just --list --justfile {{justfile()}}

import '../../share/just/project.just'

# The elisp gates (elisp-lint, elisp-compile, scanner-fidelity,
# packages-doc-in-sync, config-startup, elisp-test) via the tree's checks.
[group('test')]
test:
    #!/usr/bin/env bash
    set -euo pipefail
    system=$(nix eval --impure --raw --expr builtins.currentSystem)
    cd "{{ repo }}"
    refs=()
    while read -r n; do refs+=("checks.${system}.${n}"); done < <(
        nix eval --json -f . "checks.${system}" --apply builtins.attrNames \
        | jq -r '.[] | select(startswith("jotain-"))')
    [ "${#refs[@]}" -gt 0 ] || { echo "FAIL: no jotain-* checks registered yet"; exit 1; }
    nix build --no-link -f . "${refs[@]}"

# Build the full distribution for this system (flakeless shim entry).
[group('build')]
build:
    nix-build {{ justfile_directory() }} --out-link result

# Launch the built Emacs with this configuration.
[group('build')]
run-built *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    ./result/bin/emacs --init-directory="{{ justfile_directory() }}" {{ ARGS }}

# lib/jotain is excluded from root treefmt; formatting is enforced by its
# own treefmt config (nix/treefmt.nix, run via devenv).
[group('lint')]
lint:
    @echo "skip: lib/jotain formats via its own treefmt (nix/treefmt.nix; see devenv)"

[group('fmt')]
fmt:
    devenv shell -- treefmt .

# Remove build artifacts (compiled elisp, results).
[group('clean')]
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{ justfile_directory() }}" -name '*.elc' -type f -delete 2>/dev/null || true
    rm -rf "{{ justfile_directory() }}/var/eln-cache" 2>/dev/null || true
    rm -f  "{{ justfile_directory() }}/result" 2>/dev/null || true
    echo "Cleaned compiled artifacts."
