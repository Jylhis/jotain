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

# Regenerate docs/configuration/package-reference.mdx from the ;;; @doc
# blocks in lisp/ (the fix for a failing packages-doc-in-sync gate).
[group('docs')]
docs-refresh-packages:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    out=$(nix build --no-link --print-out-paths --impure --expr '
      (import ./nix/inputs.nix { }).jotain.packages.${builtins.currentSystem}.packages-doc')
    cp "$out/package-reference.mdx" lib/emacs/docs/configuration/package-reference.mdx
    echo "Refreshed lib/emacs/docs/configuration/package-reference.mdx"

# Re-vendor website/public/ds from the pinned jylhis/design rev
# (the fix for a failing jotain-ds-in-sync gate).
[group('pins')]
ds-sync:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    out=$(nix-build --no-out-link -E '
      let jotain = (import ./nix/inputs.nix { }).jotain;
      in jotain.packages.${builtins.currentSystem}.ds-assets')
    # Wipe first so files dropped upstream do not linger.
    rm -rf lib/emacs/website/public/ds
    mkdir -p lib/emacs/website/public/ds
    cp -r "$out/." lib/emacs/website/public/ds/
    chmod -R u+w lib/emacs/website/public/ds
    echo "Re-vendored website/public/ds from $(nix eval --raw --file lib/emacs/nix/design-pin.nix rev)"

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

# lib/emacs is excluded from root treefmt; formatting is enforced by its
# own treefmt config (nix/treefmt.nix, run via devenv).
[group('lint')]
lint:
    @echo "skip: lib/emacs formats via its own treefmt (nix/treefmt.nix; see devenv)"

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
