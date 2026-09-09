# Jotain Emacs configuration — task runner.
#
# All recipes assume the devenv shell is active. Enter it with
# `devenv shell`, or prefix any command with `devenv shell --`,
# e.g. `devenv shell -- just check`. direnv users can create their
# own (untracked) .envrc with `eval "$(devenv direnvrc)"` + `use devenv`.

config_dir := justfile_directory()

# Emacs build flavours target the current system by default. Override
# with `just system=x86_64-linux build-nox` etc.
system := arch() + "-" + if os() == "macos" { "darwin" } else { "linux" }

# List available recipes.
default:
    @just --list --justfile "{{justfile()}}"


# ── Check / compile ─────────────────────────────────────────────────

# Run all checks: eval, flake, devenv, linting.
[group('check')]
check:
    nix flake check

# Run the ERT tests under test/ via the flake check (the dev shell has
# no emacs; the check builds one). Direct equivalent once Emacs is back
# in the shell:
#   emacs --batch -L lisp -L test \
#       --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
#       -l ert -f ert-run-tests-batch-and-exit
[group('check')]
test:
    nix build .#checks.{{system}}.elisp-test --no-link --print-build-logs


# Benchmark startup against the Nix-built Emacs. The dev shell has no
# emacs, so — like run-built — this builds one via nix, then runs the
# bench/ harness (which resets user-emacs-directory back to the repo
# root, so it measures the real INTERPRETED startup: the honest
# run-built baseline). Writes a report to OUTPUT and prints it. The
# harness self-terminates via kill-emacs on emacs-startup-hook.
#
# Needs a display on x86_64: like run-built it launches the pgtk GUI
# build, which aborts with no Wayland/X display (the aarch64 branch uses
# the -nox build). For a headless run, prefix with `xvfb-run` (in the dev
# shell on Linux, as `just screenshot` does) — pgtk runs under X via GDK.
[group('check')]
bench-built output="var/bench/startup.txt":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    platform="$(uname -s)-$(uname -m)"
    case "$platform" in
        Linux-aarch64) target=build-nox-full ;;
        *)             target=build          ;;
    esac
    echo "Platform: $platform → just $target"
    just "$target"
    out="{{output}}"; case "$out" in /*) ;; *) out="{{config_dir}}/$out" ;; esac
    mkdir -p "$(dirname "$out")"
    echo "Benchmarking → $out"
    JOTAIN_BENCH_OUTPUT="$out" \
        ./result/bin/emacs --init-directory="{{config_dir}}/bench"
    echo
    cat "$out"


# ── Build (nix) ─────────────────────────────────────────────────────

# Build the full distribution (Emacs + every grammar) for the current system.
[group('build')]
build:
    nix-build

# Build a bare Emacs (no tree-sitter grammars): the matrix GUI for the
# platform — pgtk/Wayland on Linux, patched NS on Darwin (the latter is
# a from-source build by design; see emacs.nix).
[group('build')]
build-bare:
    nix-build --argstr system {{system}} emacs.nix

# Build a terminal-only Emacs (--without-x --without-ns).
[group('build')]
build-nox:
    nix-build --arg noGui true --argstr system {{system}} emacs.nix

# Build from git master (the revision pinned by emacs-overlay, binary-cached).
[group('build')]
build-git:
    nix-build --arg variant '"git"' --argstr system {{system}} emacs.nix

# Build the IGC (Memory Pool System GC) branch.
[group('build')]
build-igc:
    nix-build --arg variant '"igc"' --argstr system {{system}} emacs.nix

# Build IGC with ccache. On Darwin, nix-community.cachix.org has no
# prebuilt igc, so plain build-igc is already a from-source build there
# (plan.md §3) — ccache makes repeat local rebuilds cheaper. Needs a
# ccache sandbox exception set up first; see the useCcache doc comment
# in emacs.nix. No-op improvement elsewhere: everywhere else, igc is a
# cache hit and this just adds ccache overhead for nothing.
[group('build')]
build-igc-ccache:
    nix-build --arg variant '"igc"' --arg useCcache true --argstr system {{system}} emacs.nix

# Build a bare aarch64-linux nox Emacs (Termux/Android) — kept for
# cache-parity testing of emacs.nix; `run-built` uses build-nox-full.
[group('build')]
build-android:
    nix-build --arg noGui true --argstr system aarch64-linux emacs.nix

# Build the full terminal-only distribution (noGui Emacs + packages +
# grammars) for the current system.
[group('build')]
build-nox-full:
    nix build .#emacs-nox -o result

# Auto-detect platform, build, then launch Emacs with this configuration.
[group('build')]
run-built *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    platform="$(uname -s)-$(uname -m)"
    case "$platform" in
        Darwin-arm64)  target=build       ;;
        Darwin-*)      target=build       ;;
        Linux-aarch64) target=build-nox-full ;;
        *)             target=build       ;;
    esac
    echo "Platform: $platform → just $target"
    just "$target"
    echo "Build output: $(readlink result)"
    echo "Launching Emacs from result/bin/emacs..."
    ./result/bin/emacs --init-directory="{{config_dir}}" {{ARGS}}

# Launch the FRESH instance from the AOT-compiled config (.elc + store
# .eln) instead of interpreted .el — the fastest cold start. Assembles a
# writable init-directory under var/fast-home whose entry files and lisp/
# symlink into the .#config-compiled store derivation: realpath resolves
# each symlink to the store path the .eln was hashed against, so the
# store .eln loads (same mechanism as the HM daemon). var/, elpa/ and
# templates/ symlink back to the repo so state and installed packages
# stay warm and writable. early-init still loads from .elc (its .eln is
# structurally unreachable); the win is init.el + every lisp/init-*.el
# loading as native code. NOTE: reflects the LAST build — after editing
# config, re-run to recompile; use plain run-built while actively editing.
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
    # Compiled entry files + lisp/ from the store (symlinks; realpath →
    # store path → store .eln hits).
    for f in early-init.el early-init.elc init.el init.elc lisp; do
        ln -sfn "$store/$f" "$home/$f"
    done
    # Writable, shared state — reuse the repo's warm caches and packages.
    ln -sfn "{{config_dir}}/var" "$home/var"
    ln -sfn "{{config_dir}}/templates" "$home/templates"
    [ -e "{{config_dir}}/elpa" ] && ln -sfn "{{config_dir}}/elpa" "$home/elpa" || true
    # Store AOT .eln for init.el + lisp/ (appended to the eln load path
    # by early-init.el).
    export JOTAIN_ELN_PATH="$store/share/emacs/native-lisp"
    exec ./result/bin/emacs --init-directory="$home" {{ARGS}}

# Same, with init debugging enabled.
[group('build')]
run-built-debug *ARGS:
    just run-built --debug-init --eval '(setq debug-on-error t)' {{ARGS}}

# Like run-built-debug, but with every debugging facility on (--debug-init,
# debug-on-error, unbounded *Messages*, verbose warnings + native-comp,
# full eglot/jsonrpc traffic) and every message, warning and error backtrace
# mirrored into var/debug/<timestamp>/ (gitignored via var/). stderr is teed
# there live too; stdout stays on the tty so a GUI or -nw Emacs both work.
# Inspect the files afterwards, or M-x jotain-debug-dump-now to flush mid-run.
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
    # Redirect only fd 2 through tee: stderr (build logs + Emacs native-comp /
    # GTK / --debug-init output) is saved and still shown, without touching
    # stdout, so the interactive Emacs tty is left intact.
    exec 2> >(tee "$dir/stderr.log" >&2)
    just run-built --debug-init --load "{{config_dir}}/etc/debug-init.el" {{ARGS}}

# Headless screenshot: build Emacs, launch under Xvfb with this config,
# capture the frame via jotain-screenshot, write OUT (PNG). Linux only;
# needs xvfb-run from the devenv shell. First run is slow: nix build
# (cache pull) + MELPA package bootstrap — raise the timeout if a cold
# cache needs it.
[group('build')]
[linux]
screenshot out="var/screenshots/headless.png":
    #!/usr/bin/env bash
    set -euo pipefail
    command -v xvfb-run >/dev/null 2>&1 || {
        echo "xvfb-run not on PATH — enter the devenv shell (direnv/devenv shell)"; exit 1; }
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


# Build option reference documentation (HTML for GitHub Pages).
[group('build')]
docs:
    nix build .#docs -o result-docs
    @echo "Docs built → result-docs/index.html"

# Build the bundled Info manual (jotain.info) from docs/*.mdx + options.
# Loaded automatically by init-docs.el when result-info/ exists.
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

# Build the generated docstring-level API reference for every bundled
# package (etc/elisp-doc). Heavy: realizes the config package closure and
# runs a batch Emacs over it. Output feeds the site's /help/api/ and the
# Info manual's Emacs API Reference appendix.
[group('build')]
build-api-doc:
    nix build .#emacs-api-doc -o result-api-doc
    @echo "API reference → result-api-doc/html/index.html"
    @echo "Load log      → result-api-doc/generate.log (check the skipped list)"

# Regenerate docs/configuration/package-reference.mdx from the
# `;;; @doc` markers in lisp/init-*.el. Run after editing any
# `;;; @doc` block; CI's `packages-doc-in-sync` check will fail
# otherwise.
[group('build')]
docs-refresh-packages: build-packages-doc
    cp result-packages-doc/package-reference.mdx \
       docs/configuration/package-reference.mdx
    @echo "Refreshed docs/configuration/package-reference.mdx"

# Build the Tier-1 live language capability matrix: load the full config and
# introspect what each language is wired for (actual mode routing, tree-sitter
# readiness, resolved eglot server, formatter, on-PATH markers). Fails on a
# routing/override regression (JOTAIN_LANG_EVAL_STRICT).
[group('build')]
lang-matrix:
    nix build .#lang-eval-matrix -o result-lang-matrix
    @echo "Language matrix → result-lang-matrix/matrix.md (+ matrix.json, index.html)"

# Run the Tier-2 end-to-end live LSP probe over the curated language subset.
# Heavy: the derivation bundles the language servers. Answers "does the LSP
# actually respond", not just "is it wired".
[group('build')]
lang-eval-live:
    nix build .#lang-eval-live -o result-lang-live
    @echo "Live LSP probe → result-lang-live/live.md (+ live.json)"

# Regenerate docs/reference/language-support.mdx from the language registry
# (etc/lang-eval/jotain-lang-registry.el). Run after editing the registry;
# CI's `lang-eval-doc-in-sync` check will fail otherwise.
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

# Build the full page.jylhis.com/jotain site: landing SPA + docs pages +
# manual (HTML/Info) + man pages + GNU Emacs/Elisp manuals + options
# and package references. Published to GitHub Pages by deploy.yml on push
# to main (see .github/workflows/deploy.yml). The site is built under the
# /jotain base path (nix/site.nix baseHref).
[group('build')]
site:
    nix build .#site -o result-site
    @echo "Site → result-site/public/index.html"

# Build and locally serve the full site. The site is served under /jotain/
# (baseHref), so mount it there for a faithful preview of production.
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


# ── Lock synchronization ────────────────────────────────────────────

# Inputs shared between flake.nix and devenv.yaml — both locks must agree on these revs.
shared_inputs := "nixpkgs treefmt-nix emacs-overlay"

# Update flake inputs, then sync devenv.yaml/devenv.lock to the new revs.
[group('pins')]
update:
    #!/usr/bin/env bash
    set -euo pipefail
    nix flake update
    just sync-devenv all
    echo "Done."

# Sync devenv.yaml/devenv.lock to the revs already in flake.lock.
[group('pins')]
sync-devenv scope="shared":
    #!/usr/bin/env bash
    # Deliberately does NOT run `nix flake update`, so it is safe on a
    # Dependabot PR — those bump flake.lock alone, and
    # .github/workflows/sync-devenv.yml runs exactly this recipe to make
    # such a PR self-consistent.
    #
    # scope=shared (default) re-locks only the shared inputs. scope=all
    # re-resolves every devenv input including the unpinned `devenv`
    # module input itself, which is what `just update` has always done —
    # fine with a human reading the diff, but unreviewed drift in an
    # automated commit.
    set -euo pipefail
    case "{{ scope }}" in
        shared | all) ;;
        *)
            echo "ERROR: scope must be 'shared' or 'all', got '{{ scope }}'" >&2
            exit 1
            ;;
    esac
    tmpfile=$(mktemp)
    cp devenv.yaml "$tmpfile"
    for input in {{ shared_inputs }}; do
        node=$(jq -r ".nodes.root.inputs.\"$input\" // empty" flake.lock)
        if [ -z "$node" ]; then
            echo "ERROR: input '$input' missing from flake.lock root inputs" >&2
            exit 1
        fi
        owner=$(jq -r ".nodes.\"$node\".locked.owner" flake.lock)
        repo=$(jq -r ".nodes.\"$node\".locked.repo" flake.lock)
        rev=$(jq -r ".nodes.\"$node\".locked.rev" flake.lock)
        echo "Syncing devenv.yaml: $input -> $rev"
        sed -i.bak "s|url: github:$owner/$repo/[^[:space:]]*|url: github:$owner/$repo/$rev|" "$tmpfile"
        rm -f "$tmpfile.bak"
    done
    mv "$tmpfile" devenv.yaml
    if [ "{{ scope }}" = "all" ]; then
        devenv update
    else
        for input in {{ shared_inputs }}; do
            devenv update "$input"
        done
    fi

# Verify that flake.lock and devenv.lock agree on every shared input's rev.
[group('pins')]
verify:
    #!/usr/bin/env bash
    # Shares one implementation with the `locks-in-sync` flake check.
    set -euo pipefail
    SHARED_INPUTS="{{ shared_inputs }}" bash scripts/verify-locks.sh .

# Re-vendor website/public/ds from the jylhis/design rev pinned in
# nix/design-pin.nix — the same pin the Emacs themes are built from.
# Run after bumping that pin; the `ds-in-sync` flake check fails until
# the committed copy matches.
[group('pins')]
ds-sync:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{config_dir}}"
    out=$(nix build --no-link --print-out-paths .#ds-assets)
    # Wipe first: this is what removes fonts retired by an upstream type
    # change (v2 dropped all eight Literata/JetBrains Mono slices).
    rm -rf website/public/ds
    mkdir -p website/public/ds
    cp -r "$out/." website/public/ds/
    chmod -R u+w website/public/ds
    echo "Re-vendored website/public/ds from $(nix eval --raw --file nix/design-pin.nix rev)"


# ── Cleanup ─────────────────────────────────────────────────────────

# Remove .elc files, autosaves, and the eln-cache.
[group('clean')]
clean:
    #!/usr/bin/env bash
    set -euo pipefail
    find "{{config_dir}}" -name '*.elc' -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '*~'    -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '#*#'   -type f -delete 2>/dev/null || true
    find "{{config_dir}}" -name '.#*'   -type f -delete 2>/dev/null || true
    rm -rf "{{config_dir}}/var/eln-cache" 2>/dev/null || true
    rm -rf "{{config_dir}}/eln-cache"     2>/dev/null || true
    rm -f  "{{config_dir}}/result"        2>/dev/null || true
    echo "Cleaned compiled artifacts."

# Nuke installed packages and persistent state — forces a full re-fetch.
[group('clean')]
clean-all: clean
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf "{{config_dir}}/elpa"      2>/dev/null || true
    rm -rf "{{config_dir}}/var"       2>/dev/null || true
    rm -rf "{{config_dir}}/.dev-home" 2>/dev/null || true
    echo "Nuked elpa/, var/, .dev-home/."
