default:
    @just --list --justfile {{justfile()}}

import '../../../platform/just/project.just'

# The ERT suite of test/*-test.el under the emacs on PATH, with no Nix build.
# The hermetic gates are `just nix-check`.
[group('test')]
test:
    #!/usr/bin/env bash
    set -euo pipefail
    if ! command -v emacs >/dev/null; then
        echo "emacs: no emacs on PATH: skipping (try: just nix-check)"
        exit 0
    fi
    cd "{{ justfile_directory() }}"
    emacs --batch -L lisp -L test \
        --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
        -l ert -f ert-run-tests-batch-and-exit

# The elisp gates (elisp-lint, elisp-compile, scanner-fidelity,
# packages-doc-in-sync, config-startup, elisp-test) via the tree's checks.
[group('test')]
nix-check:
    #!/usr/bin/env bash
    set -euo pipefail
    system=$(nix eval --impure --raw --expr builtins.currentSystem)
    cd "{{ repo }}"
    refs=()
    while read -r n; do refs+=("checks.${system}.${n}"); done < <(
        nix eval --json -f . "checks.${system}" --apply builtins.attrNames \
        | jq -r '.[] | select(startswith("emacs-"))')
    [ "${#refs[@]}" -gt 0 ] || { echo "FAIL: no emacs-* checks registered yet"; exit 1; }
    platform/kernel/build-lock.sh nix build --no-link -f . "${refs[@]}"

# Regenerate docs/configuration/package-reference.mdx from the ;;; @doc
# blocks in lisp/ (the fix for a failing packages-doc-in-sync gate).
[group('docs')]
docs-refresh-packages:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ repo }}"
    out=$(nix build --no-link --print-out-paths --impure --expr '
      (import ./nix/inputs.nix { }).emacs-config.packages.${builtins.currentSystem}.packages-doc')
    cp "$out/package-reference.mdx" projects/distros/emacs/docs/configuration/package-reference.mdx
    echo "Refreshed projects/distros/emacs/docs/configuration/package-reference.mdx"

# Regenerate docs/reference/language-support.mdx from
# etc/lang-eval/my-lang-registry.el (the fix for a failing
# lang-eval-doc-in-sync gate).
[group('docs')]
docs-refresh-lang-matrix:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ justfile_directory() }}"
    EMACS_CONFIG_LANG_DOC_OUT="$PWD/docs/reference/language-support.mdx" \
        emacs -Q --batch -L etc/lang-eval -l my-lang-doc.el
    echo "Refreshed docs/reference/language-support.mdx"

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

# projects/distros/emacs is excluded from root treefmt; formatting is enforced by its
# own treefmt config (nix/treefmt.nix, run from shell.nix).
[group('lint')]
lint:
    @echo "skip: projects/distros/emacs formats via its own treefmt (nix/treefmt.nix; see shell.nix)"

[group('fmt')]
fmt:
    @just _with_nix treefmt .

# Assert the bare Emacs builds still equal their prebuilt cache siblings
# (every value must be true; see "Nix build layer" in AGENTS.md).
[group('test')]
cache-parity:
    nix-instantiate --eval --strict {{ justfile_directory() }}/nix/cache-parity.nix

# Remove build artifacts (compiled elisp, results).
[group('clean')]
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{ justfile_directory() }}" -name '*.elc' -type f -delete 2>/dev/null || true
    rm -rf "{{ justfile_directory() }}/var/eln-cache" 2>/dev/null || true
    rm -f  "{{ justfile_directory() }}/result" 2>/dev/null || true
    echo "Cleaned compiled artifacts."
