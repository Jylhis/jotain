# Jotain Emacs configuration — task runner.
#
# Recipes assume the dev shell (`nix develop`), or prefix with
# `nix develop -c` (e.g. `nix develop -c just check`).

config_dir := justfile_directory()

system := "x86_64-linux"

# List available recipes.
default:
    @just --list --justfile "{{justfile()}}"


# ── Check / compile ─────────────────────────────────────────────────

# Run every flake check (heavier than PR CI).
[group('check')]
check:
    nix flake check

# Assert each bare Emacs build equals its prebuilt sibling.
[group('check')]
cache-parity:
    nix build .#checks.{{system}}.cache-parity --no-link --print-build-logs

# Run the ERT tests under test/ via the elisp-test flake check.
[group('check')]
test:
    nix build .#checks.{{system}}.elisp-test --no-link --print-build-logs


# Measures interpreted startup (the run-built baseline). Needs a display:
# prefix `xvfb-run` when headless.
# Benchmark startup of the Nix-built Emacs with the bench/ harness.
[group('check')]
bench-built output="var/bench/startup.txt":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    just build
    out="{{output}}"; case "$out" in /*) ;; *) out="{{config_dir}}/$out" ;; esac
    mkdir -p "$(dirname "$out")"
    echo "Benchmarking → $out"
    JOTAIN_BENCH_OUTPUT="$out" \
        ./result/bin/emacs --init-directory="{{config_dir}}/bench"
    echo
    cat "$out"


# ── Build (nix) ─────────────────────────────────────────────────────

# Build the full distribution (Emacs + every grammar).
[group('build')]
build:
    nix-build

# Build a bare Emacs (no grammars), the pgtk GUI.
[group('build')]
build-bare:
    nix-build --argstr system {{system}} emacs.nix

# Build from git master (the revision pinned by emacs-overlay, binary-cached).
[group('build')]
build-git:
    nix-build --arg variant '"git"' --argstr system {{system}} emacs.nix

# Build the IGC (Memory Pool System GC) branch.
[group('build')]
build-igc:
    nix-build --arg variant '"igc"' --argstr system {{system}} emacs.nix

# igc itself is a cache hit, so ccache only pays off for a custom rev. Needs
# the sandbox setup described at useCcache in emacs.nix.
# Build IGC with ccache, for cheaper repeat local rebuilds.
[group('build')]
build-igc-ccache:
    nix-build --arg variant '"igc"' --arg useCcache true --argstr system {{system}} emacs.nix

# Off every binary cache by design; see cpuTune in emacs.nix.
# Build a CPU-tuned Emacs (-O3 -march/-mtune=icelake-client).
[group('build')]
build-perf:
    nix-build --arg cpuTune '"icelake-client"' --argstr system {{system}} emacs.nix

# Build, then launch Emacs with this configuration.
[group('build')]
run-built *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    echo "Build output: $(readlink result)"
    echo "Launching Emacs from result/bin/emacs..."
    ./result/bin/emacs --init-directory="{{config_dir}}" {{ARGS}}

# var/fast-home symlinks the entry files and lisp/ into .#config-compiled,
# so realpath hits the store .eln (as in the HM daemon); var/, elpa/ and
# templates/ point back at the repo. Reflects the last build: use plain
# run-built while editing.
# Launch the AOT-compiled config (.elc + store .eln), the fastest start.
[group('build')]
run-built-fast *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    just build
    echo "Emacs:           $(readlink result)"
    store=$(nix build --no-link --print-out-paths .#config-compiled)
    echo "Compiled config: $store"
    home="{{config_dir}}/var/fast-home"
    mkdir -p "$home" "{{config_dir}}/var"
    for f in early-init.el early-init.elc init.el init.elc lisp; do
        ln -sfn "$store/$f" "$home/$f"
    done
    # Writable state shared with the repo.
    ln -sfn "{{config_dir}}/var" "$home/var"
    ln -sfn "{{config_dir}}/templates" "$home/templates"
    [ -e "{{config_dir}}/elpa" ] && ln -sfn "{{config_dir}}/elpa" "$home/elpa" || true
    # Appended to the eln load path by early-init.el.
    export JOTAIN_ELN_PATH="$store/share/emacs/native-lisp"
    exec ./result/bin/emacs --init-directory="$home" {{ARGS}}

# Like run-built, with --debug-init and debug-on-error.
[group('build')]
run-built-debug *ARGS:
    just run-built --debug-init --eval '(setq debug-on-error t)' {{ARGS}}

# Every debugging facility on (etc/debug-init.el); messages, warnings,
# backtraces and stderr go to var/debug/<timestamp>/. stdout stays on the
# tty, so GUI and -nw both work. M-x jotain-debug-dump-now flushes mid-run.
[group('build')]
[doc('Launch with full debugging on; logs to var/debug/<timestamp>/')]
run-built-debug-log *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    ts="$(date +%Y%m%d-%H%M%S)"
    dir="{{config_dir}}/var/debug/$ts"
    mkdir -p "$dir"
    export JOTAIN_DEBUG_DIR="$dir"
    echo "Debug session → $dir"
    # Tee only stderr, leaving the interactive tty on stdout intact.
    exec 2> >(tee "$dir/stderr.log" >&2)
    just run-built --debug-init --load "{{config_dir}}/etc/debug-init.el" {{ARGS}}

# Needs xvfb-run (dev shell). First run is slow (cache pull + MELPA
# bootstrap); raise the timeout if a cold cache needs it.
# Headless screenshot under Xvfb via jotain-screenshot, saved to `out` (PNG).
[group('build')]
[linux]
screenshot out="var/screenshots/headless.png":
    #!/usr/bin/env bash
    set -euo pipefail
    command -v xvfb-run >/dev/null 2>&1 || {
        echo "xvfb-run not on PATH — enter the dev shell (nix develop)"; exit 1; }
    just build
    out="{{out}}"
    case "$out" in /*) ;; *) out="{{config_dir}}/$out" ;; esac
    mkdir -p "$(dirname "$out")"
    JOTAIN_SCREENSHOT_OUT="$out" timeout 600 xvfb-run -a -s '-screen 0 1920x1080x24' \
        ./result/bin/emacs --init-directory="{{config_dir}}" \
        --eval '(run-at-time 3 nil (lambda ()
                  (condition-case err
                      (progn (jotain-screenshot (getenv "JOTAIN_SCREENSHOT_OUT"))
                             (kill-emacs 0))
                    (error (message "jotain-screenshot failed: %S" err)
                           (kill-emacs 2)))))'
    test -s "$out" || { echo "FAIL: no screenshot written"; exit 1; }
    echo "Screenshot → $out"


# Build the Nix module options reference (HTML).
[group('build')]
docs:
    nix build .#docs -o result-docs
    @echo "Docs built → result-docs/index.html"

# init-docs.el picks up result-info/ automatically.
# Build the Info manual (jotain.info) from docs/ and the generated references.
[group('build')]
info:
    nix build .#info -o result-info
    @echo "Info manual → result-info/share/info/jotain.info"
    @echo "Open with 'just run-built' then C-h i d m Jotain RET."

# Build the per-package reference (HTML + texi + Mintlify .mdx).
[group('build')]
build-packages-doc:
    nix build .#packages-doc -o result-packages-doc
    @echo "Packages doc → result-packages-doc/index.html"

# Build the docstring-level API reference for /help/api/ (heavy).
[group('build')]
build-api-doc:
    nix build .#emacs-api-doc -o result-api-doc
    @echo "API reference → result-api-doc/html/index.html"
    @echo "Load log      → result-api-doc/generate.log (check the skipped list)"

# Required after editing any `;;; @doc` block (packages-doc-in-sync).
# Regenerate docs/configuration/package-reference.mdx from `;;; @doc` markers.
[group('build')]
docs-refresh-packages: build-packages-doc
    cp result-packages-doc/package-reference.mdx \
       docs/configuration/package-reference.mdx
    @echo "Refreshed docs/configuration/package-reference.mdx"

# Loads the full config and introspects each language's mode routing,
# grammar, eglot server, formatter and tools; fails on a routing regression.
# Build the live per-language capability matrix.
[group('build')]
lang-matrix:
    nix build .#lang-eval-matrix -o result-lang-matrix
    @echo "Language matrix → result-lang-matrix/matrix.md (+ matrix.json, index.html)"

# Heavy: bundles the language servers. Checks that each LSP responds.
# Run the end-to-end LSP probe over the curated language subset.
[group('build')]
lang-eval-live:
    nix build .#lang-eval-live -o result-lang-live
    @echo "Live LSP probe → result-lang-live/live.md (+ live.json)"

# Required after editing etc/lang-eval/jotain-lang-registry.el.
# Regenerate docs/reference/language-support.mdx from the language registry.
[group('build')]
docs-refresh-lang-matrix:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{ config_dir }}"
    out=$(nix build --no-link --print-out-paths .#lang-eval-doc)
    cp "$out/language-support.mdx" docs/reference/language-support.mdx
    echo "Refreshed docs/reference/language-support.mdx"

# Build both HTML docs and the Info manual.
[group('build')]
docs-all: docs info

# Landing page, docs, manuals, man pages and the generated references,
# under the /jotain base path; deploy.yml publishes it from main.
# Build the full page.jylhis.com/jotain site.
[group('build')]
site:
    nix build .#site -o result-site
    @echo "Site → result-site/public/index.html"

# Build the site and serve it locally under /jotain/, as in production.
[group('build')]
serve-site: site
    #!/usr/bin/env bash
    set -euo pipefail
    d=$(mktemp -d)
    trap 'rm -rf "$d"' EXIT
    ln -s "{{ config_dir }}/result-site/public" "$d/jotain"
    echo "Serving → http://localhost:8080/jotain/"
    python3 -m http.server -d "$d" 8080


# ── Format ──────────────────────────────────────────────────────────

# Format all Nix files.
[group('format')]
fmt:
    nix fmt


# ── Pins ────────────────────────────────────────────────────────────

# Update flake inputs.
[group('pins')]
update:
    nix flake update

# Covers the extra Emacs packages, ECA server, vendored npm LSPs and the
# design pin. Scope with pin names (`just update-pins combobulate eca`);
# `--list` shows them.
# Bump the hand-pinned upstreams that flake.lock does not manage.
[group('pins')]
update-pins *PINS:
    bash scripts/update-pins.sh {{ PINS }}

# Run after bumping nix/design-pin.nix (ds-in-sync fails until then).
# Re-vendor website/public/ds from the pinned jylhis/design rev.
[group('pins')]
ds-sync:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    out=$(nix build --no-link --print-out-paths .#ds-assets)
    # Wipe first so files dropped upstream do not linger.
    rm -rf website/public/ds
    mkdir -p website/public/ds
    cp -r "$out/." website/public/ds/
    chmod -R u+w website/public/ds
    echo "Re-vendored website/public/ds from $(nix eval --raw --file nix/design-pin.nix rev)"


# ── Cleanup ─────────────────────────────────────────────────────────

# Remove .elc files, autosaves, the eln-cache and the result symlink.
[group('clean')]
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{config_dir}}" -name '*.elc' -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '*~'    -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '#*#'   -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '.#*'   -type f -delete 2>/dev/null || true
    rm -rf "{{config_dir}}/var/eln-cache" 2>/dev/null || true
    rm -f  "{{config_dir}}"/result "{{config_dir}}"/result-* 2>/dev/null || true
    echo "Cleaned compiled artifacts."

# Run clean, then remove elpa/ and var/ (forces a full re-fetch).
[group('clean')]
clean-all: clean
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf "{{config_dir}}/elpa"      2>/dev/null || true
    rm -rf "{{config_dir}}/var"       2>/dev/null || true
    echo "Nuked elpa/ and var/."
