# Jotain Emacs configuration — task runner.
#
# All recipes assume the devenv shell is active (direnv handles this
# automatically via .envrc). If you don't use direnv, prefix any
# command with `devenv shell --`, e.g. `devenv shell -- just check`.

config_dir := justfile_directory()

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
    echo "Cleaned compiled artifacts."

# Nuke installed packages and no-littering state — forces a full re-fetch.
[group('clean')]
clean-all: clean
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf {{config_dir}}/elpa      2>/dev/null || true
    rm -rf {{config_dir}}/var       2>/dev/null || true
    rm -rf {{config_dir}}/etc       2>/dev/null || true
    rm -rf {{config_dir}}/.dev-home 2>/dev/null || true
    echo "Nuked elpa/, var/, etc/, .dev-home/."
