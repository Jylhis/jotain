# Repository Guidelines

## Project Structure & Module Organization

Jotain is an Emacs 31 configuration (floor: Emacs 30.1) with a Nix build layer. Startup files are `early-init.el` and `init.el`; feature modules live in `lisp/init-*.el` and are loaded from `init.el` in order. Nix build and module code lives at the root (`flake.nix`, `emacs.nix`, `overlay.nix`, `module.nix`, `module-system.nix`) with helpers in `nix/` (`mk-overlay.nix` is the overlay implementation). Documentation sources are in `docs/`, benchmark wrappers are in `bench/`, and ERT tests live in `test/` — every `test/*.el` file is loaded by the test check.

## Build, Test, and Development Commands

Use the devenv shell; with direnv this is automatic. Without it, prefix commands with `devenv shell --`. The shell provides tooling only — Emacs itself is **not** in it, so the direct-launch and in-shell compile recipes (`just run`, `debug`, `tty`, `check-elisp`, `compile`, `bench`, …) are disabled stubs that print a notice and exit 0.

- `just run-built [ARGS]`: build Emacs via Nix for this platform, then launch `result/bin/emacs` using this repo as `--init-directory`.
- `just run-built-debug [ARGS]`: same, with `--debug-init` and `debug-on-error`.
- `just check`: run `nix flake check`, including package builds, Nix linting, Elisp paren checks, byte-compilation (warnings as errors), and the ERT tests.
- `just test`: build only the `elisp-test` flake check (ERT tests from `test/`, loads every `test/*.el`).
- `just fmt`: format Nix files via the flake formatter.
- `just docs-all`: build HTML docs and the bundled Info manual.

## Coding Style & Naming Conventions

Elisp files use `-*- lexical-binding: t; -*-`, one concern per `lisp/init-*.el` module, and end with `(provide 'init-<concern>)`. Add new modules to `init.el` at the right load point. Prefer `setopt` for `defcustom` values. Built-ins and Nix-provided packages in `use-package` blocks should use `:ensure nil`. Keep LSP hooks and formatter wiring centralized in `lisp/init-prog.el`. Nix formatting is controlled by `nix/treefmt.nix` with `nixfmt`, `deadnix`, and `statix`.

For Emacs internals and Elisp practice, use the source-cited skills in `.claude/skills/` (`emacs-internals`, `elisp-dev`), indexed at `.claude/knowledge/emacs/README.md`, rather than answering from memory.

## Testing Guidelines

Place ERT files under `test/` — the elisp-test check globs and loads every `*.el` in that directory, so new test files are picked up automatically without registration. Keep tests focused on module behavior or helper functions and load project code with `-L lisp`. Run `just test` for ERT alone, and `just check` (the full `nix flake check`, including the `elisp-lint` syntax check) before opening a PR.

## Commit & Pull Request Guidelines

Recent commits use short, imperative subjects such as `Add todo` or `Pin nix-ts-mode to Nix to prevent MELPA/grammar version conflicts`. Keep subjects concise and describe the behavioral change. PRs should include a summary, relevant issue links, test/check results, and screenshots only when UI-facing Emacs behavior changes.

## Security & Configuration Tips

`flake.lock` is the source of truth for pinned inputs. When updating pins, use `just update` and then `just verify` to keep `flake.lock` and `devenv.lock` aligned. Do not commit generated state such as `elpa/`, `var/`, `result*`, or compiled `*.elc` files.
