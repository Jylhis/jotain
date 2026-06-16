# Jotain Emacs configuration — task runner.
#
# All recipes assume the devenv shell is active (direnv handles this
# automatically via .envrc). If you don't use direnv, prefix any
# command with `devenv shell --`, e.g. `devenv shell -- just check`.

config_dir := justfile_directory()

# Emacs build flavours target the current system by default. Override
# with `just system=x86_64-linux build-nox` etc.
system := `nix eval --impure --raw --expr 'builtins.currentSystem'`

# List available recipes.
default:
    @just --list --justfile "{{justfile()}}"


# ── Run ─────────────────────────────────────────────────────────────
#
# All recipes in this section depend on `emacs` / `emacsclient` being
# on PATH. Emacs is temporarily not installed into the devenv shell
# (see top-of-file note in devenv.nix), so they print a notice and
# exit 0 instead of failing cryptically. Use `just run-built` to build
# Emacs via Nix and launch it with this configuration.

# [DISABLED] Launch Emacs with this config in isolation (--init-directory).
[group('run')]
run *ARGS:
    @echo "just run is disabled — emacs is not in the devenv shell."
    @echo "Try: just run-built  (builds Emacs via Nix, then launches it)"
# Original:
#   emacs --init-directory={{config_dir}} {{ARGS}}

# [DISABLED] Launch with --debug-init and debug-on-error.
[group('run')]
debug *ARGS:
    @echo "just debug is disabled — emacs is not in the devenv shell."
    @echo "Try: just run-built  (then re-enable when wired back up)"
# Original:
#   emacs --init-directory={{config_dir}} --debug-init \
#         --eval '(setq debug-on-error t)' {{ARGS}}

# [DISABLED] Launch in the terminal (-nw) — exercises kkp + clipetty.
[group('run')]
tty *ARGS:
    @echo "just tty is disabled — emacs is not in the devenv shell."
# Original:
#   emacs -nw --init-directory={{config_dir}} {{ARGS}}

# [DISABLED] Run a foreground Emacs daemon with this config.
[group('run')]
daemon *ARGS:
    @echo "just daemon is disabled — emacs is not in the devenv shell."
# Original:
#   emacs --fg-daemon --init-directory={{config_dir}} {{ARGS}}

# [DISABLED] Connect a graphical emacsclient frame to the running daemon.
[group('run')]
client *ARGS:
    @echo "just client is disabled — emacsclient is not in the devenv shell."
# Original:
#   emacsclient -c --alternate-editor='emacs --init-directory={{config_dir}}' {{ARGS}}

# [DISABLED] Connect a terminal emacsclient frame to the running daemon.
[group('run')]
client-tty *ARGS:
    @echo "just client-tty is disabled — emacsclient is not in the devenv shell."
# Original:
#   emacsclient -t --alternate-editor='emacs -nw --init-directory={{config_dir}}' {{ARGS}}

# [DISABLED] Lightweight `emacs -Q -nw` for quick edits.
[group('run')]
quick *ARGS:
    @echo "just quick is disabled — emacs is not in the devenv shell."
# Original:
#   emacs -Q -nw --eval "(load-theme 'wombat t)" {{ARGS}}


# ── Check / compile ─────────────────────────────────────────────────

# Run all checks: eval, flake, devenv, linting.
[group('check')]
check:
    nix flake check

# Equivalent coverage lives in the `elisp-lint` flake check; `just check`
# runs it via `nix flake check`.
# [DISABLED] Parse every .el file (no compile, no package install).
[group('check')]
check-elisp:
    @echo "just check-elisp is disabled — emacs is not in the devenv shell."
    @echo "Equivalent: just check  (runs the elisp-lint flake check)"
# Original:
#   emacs -Q --batch --eval '(check-parens for early-init.el init.el lisp/init-*.el)'

# Equivalent coverage lives in the `elisp-compile` flake check.
# [DISABLED] Byte-compile everything; requires packages installed.
[group('check')]
compile:
    @echo "just compile is disabled — emacs is not in the devenv shell."
    @echo "Equivalent: just check  (runs the elisp-compile flake check)"
# Original:
#   emacs --batch --init-directory={{config_dir}} \
#       --eval '(setq byte-compile-error-on-warn t)' \
#       --eval '(byte-recompile-directory "{{config_dir}}/lisp" 0 t)' \
#       -f batch-byte-compile early-init.el init.el

# [DISABLED] Run ERT tests under test/ if any exist.
[group('check')]
test:
    @echo "just test is disabled — emacs is not in the devenv shell."
    @echo "(There is no test/ directory yet; re-enable alongside Emacs.)"
# Original:
#   emacs --batch -L lisp -L test -l ert \
#       $(find test -name 'test-*.el' -exec echo -l {} \;) \
#       -f ert-run-tests-batch-and-exit


# Wrapper init files live in bench/ — kept on disk for when re-enabled.
# [DISABLED] Benchmark startup: launch Emacs, collect metrics, print results.
[group('check')]
bench output="":
    @echo "just bench is disabled — emacs is not in the devenv shell."
# Original:
#   JOTAIN_BENCH_OUTPUT=... emacs --init-directory={{config_dir}}/bench

# [DISABLED] Benchmark file-open: open representative files and time each hook.
[group('check')]
bench-open output="":
    @echo "just bench-open is disabled — emacs is not in the devenv shell."
# Original:
#   JOTAIN_BENCH_OPEN_OUTPUT=... emacs --init-directory={{config_dir}}/bench


# ── Build (nix) ─────────────────────────────────────────────────────

# Build the full distribution (Emacs + every grammar) for the current system.
[group('build')]
build:
    nix-build

# Build a bare Emacs (no tree-sitter grammars).
[group('build')]
build-bare:
    nix-build --argstr system {{system}} emacs.nix

# Build the bare github:jylhis/emacs Meson fork.
[group('build')]
build-jylhis:
    nix build .#jylhis-emacs -o result-jylhis-emacs

# Note: the full fork-backed package set (`jylhisEmacsPackages`) is no
# longer exposed as a flake `packages` output — the Meson fork still
# crashes byte-compiling some bundled Emacs packages. It remains
# reachable via the Home Manager `services.jotain.emacsBackend = "jylhis"`
# option, which consumes the overlay attribute directly.

# Build with --with-pgtk for Wayland.
[group('build')]
build-pgtk:
    nix-build --arg withPgtk true --argstr system {{system}} emacs.nix

# Build with --with-x-toolkit=gtk3 (X11 + GTK3).
[group('build')]
build-gtk3:
    nix-build --arg withGTK3 true --argstr system {{system}} emacs.nix

# Build a terminal-only Emacs (--without-x --without-ns).
[group('build')]
build-nox:
    nix-build --arg noGui true --argstr system {{system}} emacs.nix

# Build the macport variant (Darwin only).
[group('build')]
build-macport:
    nix-build --arg variant '"macport"' --argstr system {{system}} emacs.nix

# Build from git master (the revision pinned by emacs-overlay, binary-cached).
[group('build')]
build-git:
    nix-build --arg variant '"git"' --argstr system {{system}} emacs.nix

# Build the IGC (Memory Pool System GC) branch.
[group('build')]
build-igc:
    nix-build --arg variant '"igc"' --argstr system {{system}} emacs.nix

# Build for aarch64-linux nox (Termux/Android).
[group('build')]
build-android:
    nix-build --arg noGui true --argstr system aarch64-linux emacs.nix

# Auto-detect platform, build, then launch Emacs with this configuration.
[group('build')]
run-built *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    platform="$(uname -s)-$(uname -m)"
    case "$platform" in
        Darwin-arm64)  target=build       ;;
        Darwin-*)      target=build       ;;
        Linux-aarch64) target=build-android ;;
        *)             target=build       ;;
    esac
    echo "Platform: $platform → just $target"
    just "$target"
    echo "Build output: $(readlink result)"
    echo "Launching Emacs from result/bin/emacs..."
    ./result/bin/emacs --debug-init \
        --eval '(setq debug-on-error t)' \
        --init-directory="{{config_dir}}" {{ARGS}}


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
    @echo "Open with 'just run' then C-h i d m Jotain RET."

# Build the per-package reference (HTML + texi + Mintlify .mdx).
[group('build')]
build-packages-doc:
    nix build .#packages-doc -o result-packages-doc
    @echo "Packages doc → result-packages-doc/index.html"

# Regenerate docs/configuration/package-reference.mdx from the
# `;;; @doc` markers in lisp/init-*.el. Run after editing any
# `;;; @doc` block; CI's `packages-doc-in-sync` check will fail
# otherwise.
[group('build')]
docs-refresh-packages: build-packages-doc
    cp result-packages-doc/package-reference.mdx \
       docs/configuration/package-reference.mdx
    @echo "Refreshed docs/configuration/package-reference.mdx"

# Build both HTML docs and the Info manual.
[group('build')]
docs-all: docs info


# ── Format ──────────────────────────────────────────────────────────

# Format all Nix files.
[group('format')]
fmt:
    nix fmt


# ── Lock synchronization ────────────────────────────────────────────

# Inputs shared between flake.nix and devenv.yaml — both locks must agree on these revs.
shared_inputs := "nixpkgs treefmt-nix emacs-overlay"

# Update flake inputs and sync matching devenv.yaml URLs to the new revs.
[group('pins')]
update:
    #!/usr/bin/env bash
    set -euo pipefail
    nix flake update
    tmpfile=$(mktemp)
    cp devenv.yaml "$tmpfile"
    for input in {{ shared_inputs }}; do
        owner=$(jq -r ".nodes.\"$input\".locked.owner" flake.lock)
        repo=$(jq -r ".nodes.\"$input\".locked.repo" flake.lock)
        rev=$(jq -r ".nodes.\"$input\".locked.rev" flake.lock)
        echo "Syncing devenv.yaml: $input -> $rev"
        sed -i.bak "s|url: github:$owner/$repo/[^[:space:]]*|url: github:$owner/$repo/$rev|" "$tmpfile"
        rm -f "$tmpfile.bak"
    done
    mv "$tmpfile" devenv.yaml
    devenv update
    echo "Done."

# Verify that flake.lock and devenv.lock agree on every shared input's rev.
[group('pins')]
verify:
    #!/usr/bin/env bash
    set -euo pipefail
    fail=0
    for input in {{ shared_inputs }}; do
        flake_rev=$(jq -r ".nodes.\"$input\".locked.rev" flake.lock)
        devenv_rev=$(jq -r ".nodes.\"$input\".locked.rev" devenv.lock)
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
