# ERT testing

The ERT suite lives under `test/`. The `elisp-test` flake check loads
**every** `*.el` file in that directory, so new test files need no
registration.

Current suite:

- `test-smoke.el`: canary proving the runner globs `test/*.el`.
- `test-ui.el`: theme wiring regression tests (reads `init-ui.el` as
  data, so the theme names can't drift from the design-system pin; the
  theme-file tests `skip-unless` the `jylhis-themes` package loads).
- `test-org-babel.el`: Org Babel wiring, including the
  `jotain-org-babel-confirm-evaluate` trust check.
- `completion-test.el`: the in-buffer completion wiring.
- `devenv-test.el`: the `devenv.el` integration library.
- `lang-eval-test.el`: drift guard for the language-eval registry in
  `etc/lang-eval/`.

Run it with `just test` (builds the `elisp-test` flake check; the dev
shell has no Emacs, so the check builds one via Nix) or as part of
`just check`. The check runs in the Nix sandbox, so tests must not need
the network, subprocesses, or the devenv binary; use `skip-unless` for
anything that needs an external tool.

Conventions: prefix every `ert-deftest` with the module or package name
(test names share one global namespace), keep tests side-effect free
(`with-temp-buffer`, `let`-bound options, `cl-letf` to stub functions),
and load project code with `-L lisp`. See the `elisp-dev` skill's
`debugging-and-testing.md` for the full pattern reference.
