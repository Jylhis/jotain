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
    @just --list --justfile {{justfile()}}


# ── Run ─────────────────────────────────────────────────────────────

# Launch Emacs with this config in isolation (--init-directory).
[group('run')]
run *ARGS:
    emacs --init-directory={{config_dir}} {{ARGS}}

# Launch with --debug-init and debug-on-error.
[group('run')]
debug *ARGS:
    emacs --init-directory={{config_dir}} --debug-init \
          --eval '(setq debug-on-error t)' {{ARGS}}

# Launch in the terminal (-nw) — exercises kkp + clipetty.
[group('run')]
tty *ARGS:
    emacs -nw --init-directory={{config_dir}} {{ARGS}}


# ── Check / compile ─────────────────────────────────────────────────

# Run all checks: eval, flake, devenv, linting.
[group('check')]
check:
    nix flake check

# Parse every .el file (no compile, no package install).
[group('check')]
check-elisp:
    #!/usr/bin/env bash
    set -euo pipefail
    emacs -Q --batch --eval '
      (let ((failed nil))
        (dolist (f (append (list "early-init.el" "init.el")
                           (directory-files "lisp" t "^init-.*\\.el$")))
          (condition-case err
              (with-temp-buffer
                (insert-file-contents f)
                (emacs-lisp-mode)
                (check-parens)
                (message "OK: %s" (file-name-nondirectory f)))
            (error
             (message "FAIL %s: %S" (file-name-nondirectory f) err)
             (setq failed t))))
        (when failed (kill-emacs 1)))'

# Byte-compile everything; requires packages installed (run `just run` first).
[group('check')]
compile:
    #!/usr/bin/env bash
    set -euo pipefail
    emacs --batch \
        --init-directory={{config_dir}} \
        --eval '(setq byte-compile-error-on-warn t)' \
        --eval '(byte-recompile-directory "{{config_dir}}/lisp" 0 t)' \
        -f batch-byte-compile early-init.el init.el

# Run ERT tests under test/ if any exist.
[group('check')]
test:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -d test ]; then
        echo "No test/ directory yet."
        exit 0
    fi
    emacs --batch \
        -L lisp -L test \
        -l ert \
        $(find test -name 'test-*.el' -exec echo -l {} \;) \
        -f ert-run-tests-batch-and-exit


# Benchmark startup: launch Emacs, collect metrics, print results.
# Wrapper init files live in bench/ — see bench/early-init.el.
[group('check')]
bench output="":
    #!/usr/bin/env bash
    set -euo pipefail
    results="$(mktemp "${TMPDIR:-/tmp}/jotain-bench-results.XXXXXX")"
    trap 'rm -f "$results"' EXIT

    JOTAIN_BENCH_OUTPUT="$results" \
        emacs --init-directory="{{config_dir}}/bench" 2>/dev/null

    cat "$results"
    if [ -n "{{output}}" ]; then
        cp "$results" "{{output}}"
        echo ""
        echo "Results saved to {{output}}"
    fi

# Benchmark file-open: open representative files and time each hook.
[group('check')]
bench-open output="":
    #!/usr/bin/env bash
    set -euo pipefail
    results="$(mktemp "${TMPDIR:-/tmp}/jotain-bench-open.XXXXXX")"
    trap 'rm -f "$results"' EXIT

    JOTAIN_BENCH_OPEN_OUTPUT="$results" \
        emacs --init-directory="{{config_dir}}/bench" 2>/dev/null

    cat "$results"
    if [ -n "{{output}}" ]; then
        cp "$results" "{{output}}"
        echo ""
        echo "Results saved to {{output}}"
    fi


# ── Build (nix) ─────────────────────────────────────────────────────

# Build the full distribution (Emacs + every grammar) for the current system.
[group('build')]
build:
    nix-build

# Build a bare Emacs (no tree-sitter grammars).
[group('build')]
build-bare:
    nix-build --argstr system {{system}} emacs.nix

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

# Build from current git master (first run will report a hash to fill in).
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
        --init-directory={{config_dir}} {{ARGS}}


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
shared_inputs := "nixpkgs treefmt-nix"

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
    find {{config_dir}} -name '*.elc' -type f -delete 2>/dev/null || true
    find {{config_dir}} -name '*~'    -type f -delete 2>/dev/null || true
    find {{config_dir}} -name '#*#'   -type f -delete 2>/dev/null || true
    find {{config_dir}} -name '.#*'   -type f -delete 2>/dev/null || true
    rm -rf {{config_dir}}/var/eln-cache 2>/dev/null || true
    rm -rf {{config_dir}}/eln-cache     2>/dev/null || true
    rm -f  {{config_dir}}/result        2>/dev/null || true
    echo "Cleaned compiled artifacts."

# Nuke installed packages and persistent state — forces a full re-fetch.
[group('clean')]
clean-all: clean
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf {{config_dir}}/elpa      2>/dev/null || true
    rm -rf {{config_dir}}/var       2>/dev/null || true
    rm -rf {{config_dir}}/.dev-home 2>/dev/null || true
    echo "Nuked elpa/, var/, .dev-home/."
