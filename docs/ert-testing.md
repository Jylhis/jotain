# ERT testing

The ERT suite lives under `test/`. The `elisp-test` flake check globs and
loads **every** `*.el` file in that directory — any file name works, and
new test files need no registration anywhere.

Current suite:

- `test-smoke.el` — proves the runner globs `test/*.el` (a canary test).
- `test-ui.el` — theme wiring regression tests (theme tests `skip-unless`
  the out-of-tree `jylhis-themes` package is available).
- `completion-test.el` — the in-buffer completion wiring.
- `devenv-test.el` — the `devenv.el` integration library.

Run it with `just test` (builds the `elisp-test` flake check — the dev
shell has no Emacs, so the check builds one via Nix), or as part of
`just check`. The check runs inside the Nix sandbox: no network, no
subprocesses, no devenv binary — use `skip-unless` for anything that
needs an external tool.

Conventions: prefix every `ert-deftest` with the module or package name
(test names share one global namespace), keep tests side-effect free
(`with-temp-buffer`, `let`-bind options, `cl-letf` to stub functions),
and load project code with `-L lisp`. See the `elisp-dev` skill's
`debugging-and-testing.md` for the full pattern reference.
