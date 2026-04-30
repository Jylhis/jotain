# Repository Guidelines

## Project Structure & Module Organization

Jotain is an Emacs 30+ configuration with a Nix build layer. Startup files are `early-init.el` and `init.el`; feature modules live in `lisp/init-*.el` and are loaded from `init.el` in order. Nix build and module code lives at the root (`flake.nix`, `emacs.nix`, `overlay.nix`, `module.nix`, `module-system.nix`) with helpers in `nix/`. Documentation sources are in `docs/`, benchmark wrappers are in `bench/`, and future ERT tests belong in `test/` as `test-*.el`.

## Build, Test, and Development Commands

Use the devenv shell; with direnv this is automatic. Without it, prefix commands with `devenv shell --`.

- `just run [ARGS]`: launch Emacs using this repo as `--init-directory`.
- `just debug`: launch with `--debug-init` and `debug-on-error`.
- `just check`: run `nix flake check`, including package builds, Nix linting, Elisp paren checks, and byte-compilation.
- `just check-elisp`: run only the fast Elisp syntax check.
- `just compile`: byte-compile the configuration with warnings treated as errors.
- `just test`: run ERT tests from `test/` if present.
- `just fmt`: format Nix files via the flake formatter.
- `just docs-all`: build HTML docs and the bundled Info manual.

## Coding Style & Naming Conventions

Elisp files use `-*- lexical-binding: t; -*-`, one concern per `lisp/init-*.el` module, and end with `(provide 'init-<concern>)`. Add new modules to `init.el` at the right load point. Prefer `setopt` for `defcustom` values. Built-ins and Nix-provided packages in `use-package` blocks should use `:ensure nil`. Keep LSP hooks and formatter wiring centralized in `lisp/init-prog.el`. Nix formatting is controlled by `nix/treefmt.nix` with `nixfmt`, `deadnix`, and `statix`.

## Testing Guidelines

Place ERT files under `test/` using `test-*.el` names. Keep tests focused on module behavior or helper functions and load project code with `-L lisp`. Run `just test` for ERT, `just check-elisp` for syntax-only validation, and `just check` before opening a PR.

## Commit & Pull Request Guidelines

Recent commits use short, imperative subjects such as `Add todo` or `Pin nix-ts-mode to Nix to prevent MELPA/grammar version conflicts`. Keep subjects concise and describe the behavioral change. PRs should include a summary, relevant issue links, test/check results, and screenshots only when UI-facing Emacs behavior changes.

## Security & Configuration Tips

`flake.lock` is the source of truth for pinned inputs. When updating pins, use `just update` and then `just verify` to keep `flake.lock` and `devenv.lock` aligned. Do not commit generated state such as `elpa/`, `var/`, `result*`, or compiled `*.elc` files.
