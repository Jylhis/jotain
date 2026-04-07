[private]
default:
  @just --list

# ── x86_64-linux variants (all binary-cached) ──────────────────────

linux:
  nix-build --argstr system x86_64-linux default.nix

linux-gtk3:
  nix-build --arg withGTK3 true --argstr system x86_64-linux default.nix

linux-nox:
  nix-build --arg noGui true --argstr system x86_64-linux default.nix

linux-pgtk:
  nix-build --arg withPgtk true --argstr system x86_64-linux default.nix

# ── x86_64-darwin variants (all binary-cached) ─────────────────────

macos:
  nix-build --argstr system x86_64-darwin default.nix

macos-gtk3:
  nix-build --arg withGTK3 true --argstr system x86_64-darwin default.nix

macos-nox:
  nix-build --arg noGui true --argstr system x86_64-darwin default.nix

macos-pgtk:
  nix-build --arg withPgtk true --argstr system x86_64-darwin default.nix

macos-macport:
  nix-build --arg variant '"macport"' --argstr system x86_64-darwin default.nix

# ── aarch64-linux ──────────────────────────────────────────────────

android:
  nix-build --arg noGui true --argstr system aarch64-linux default.nix

# ── Dev tools ──────────────────────────────────────────────────────

ellsp:
  nix-build -E 'with import <nixpkgs> {}; callPackage ./nix/ellsp.nix {}'

elsa:
  nix-build -E 'with import <nixpkgs> {}; callPackage ./nix/elsa.nix { emacs = emacs-nox; }'

# ── Run & clean ────────────────────────────────────────────────────

run *ARGS:
  #!/usr/bin/env bash
  set -euo pipefail
  platform="$(uname -s)-$(uname -m)"
  echo "Platform: $platform"
  case "$platform" in
    Darwin-*) target=macos ;;
    Linux-aarch64) target=android ;;
    *) target=linux ;;
  esac
  echo "Building target: $target"
  just "$target"
  echo "Build output: $(readlink result)"
  echo "Launching Emacs..."
  ./result/bin/emacs --debug-init --eval '(setq debug-on-error t)' --init-directory='{{justfile_directory()}}' {{ARGS}}

clean:
  rm -rf eln-cache result custom.el transient elpa auto-save-list

# ── Tests / lint / check ───────────────────────────────────────────

# Run the full ERT suite via the aggregate loader.  Prefers the
# devenv-provided emacs when available so the recipe works on hosts
# without a system emacs in PATH.
test:
  #!/usr/bin/env bash
  set -euo pipefail
  if command -v emacs >/dev/null 2>&1; then
    emacs --batch -L lisp -L test -l test/jotain-run-tests.el -f ert-run-tests-batch-and-exit
  else
    devenv shell -- emacs --batch -L lisp -L test -l test/jotain-run-tests.el -f ert-run-tests-batch-and-exit
  fi

# Lint every .el file under lisp/ and test/.
lint:
  #!/usr/bin/env bash
  set -euo pipefail
  status=0
  for f in lisp/*.el test/*.el; do
    [ -e "$f" ] || continue
    ./scripts/elisp-lint.sh "$f" || status=$?
  done
  exit "$status"

# Format the tree via treefmt (provided by devenv).
fmt:
  treefmt

# Verify nix expressions evaluate.
nix-check:
  ./scripts/nix-eval-check.sh

# Run lint + nix-check + tests — the same gates CI runs.
check: lint nix-check test
