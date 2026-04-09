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

# Parse every .el file (no compile, no package install).
[group('check')]
check:
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
[group('check')]
bench output="":
    #!/usr/bin/env bash
    set -euo pipefail
    results="$(mktemp "${TMPDIR:-/tmp}/jotain-bench-results.XXXXXX")"
    script="$(mktemp "${TMPDIR:-/tmp}/jotain-bench-script.XXXXXX")"
    trap 'rm -f "$results" "$script"' EXIT

    cat > "$script" << 'ELISP'
    (add-hook 'emacs-startup-hook
      (lambda ()
        (let* ((init-time (float-time (time-subtract after-init-time before-init-time)))
               (total-time (float-time (time-subtract (current-time) before-init-time)))
               (gc-pct (if (> init-time 0) (* 100.0 (/ gc-elapsed init-time)) 0.0))
               (pkg-count (if (bound-and-true-p package-activated-list)
                              (length package-activated-list) 0))
               (outfile (getenv "JOTAIN_BENCH_OUTPUT")))
          (when outfile
            (with-temp-file outfile
              (insert
               (format "Emacs Startup Benchmark\n")
               (format "═══════════════════════════════════════\n")
               (format "Init time            %.3f seconds\n" init-time)
               (format "Total startup time   %.3f seconds\n" total-time)
               (format "GC count             %d\n" gcs-done)
               (format "GC time              %.3f seconds (%.1f%%)\n" gc-elapsed gc-pct)
               (format "Features loaded      %d\n" (length features))
               (format "Packages activated   %d\n" pkg-count))))
          (kill-emacs 0))))
    ELISP

    JOTAIN_BENCH_OUTPUT="$results" \
        emacs --init-directory={{config_dir}} -l "$script"

    cat "$results"
    if [ -n "{{output}}" ]; then
        cp "$results" "{{output}}"
        echo ""
        echo "Results saved to {{output}}"
    fi


# ── Build (nix) ─────────────────────────────────────────────────────
#
# `default.nix' wraps `emacs.nix' with all tree-sitter grammars from
# nixpkgs. `emacs.nix' alone builds a bare Emacs binary. Both default to
# the npins-pinned nixpkgs-unstable channel; pass --arg pkgs '<nixpkgs>'
# to use NIX_PATH instead.

# Build the full distribution (Emacs + every grammar) for the current system.
[group('build')]
build:
    nix-build --argstr system {{system}} default.nix

# Build a bare Emacs (no tree-sitter grammars).
[group('build')]
build-bare:
    nix-build --argstr system {{system}} emacs.nix

# Build with --with-pgtk for Wayland.
[group('build')]
build-pgtk:
    nix-build --arg withPgtk true --argstr system {{system}} default.nix

# Build with --with-x-toolkit=gtk3 (X11 + GTK3).
[group('build')]
build-gtk3:
    nix-build --arg withGTK3 true --argstr system {{system}} default.nix

# Build a terminal-only Emacs (--without-x --without-ns).
[group('build')]
build-nox:
    nix-build --arg noGui true --argstr system {{system}} default.nix

# Build the macport variant (Darwin only).
[group('build')]
build-macport:
    nix-build --arg variant '"macport"' --argstr system {{system}} default.nix

# Build from current git master (first run will report a hash to fill in).
[group('build')]
build-git:
    nix-build --arg variant '"git"' --argstr system {{system}} default.nix

# Build the IGC (Memory Pool System GC) branch.
[group('build')]
build-igc:
    nix-build --arg variant '"igc"' --argstr system {{system}} default.nix

# Build for aarch64-linux nox (Termux/Android).
[group('build')]
build-android:
    nix-build --arg noGui true --argstr system aarch64-linux default.nix

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


# ── Format ──────────────────────────────────────────────────────────

# Format Nix (and anything else treefmt knows about).
[group('format')]
fmt:
    treefmt


# ── Pin management (npins) ──────────────────────────────────────────

# Update every npins-managed source.
[group('pins')]
update-pins:
    npins update

# Update one named pin.
[group('pins')]
update-pin NAME:
    npins update {{NAME}}

# Show all current pins.
[group('pins')]
show-pins:
    npins show


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
