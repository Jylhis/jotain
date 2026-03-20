1. We have a failure in github actions complaining about Node.js 20 actions deprecation but the actual failure is `Process completed with exit code 1.` at `.github/workflows/determinate-ci.yml`, likely in the step `nix flake check`.
2. Wait, the problem is in `nix flake check`. When we ran `nix flake check` it timed out locally.
Let's see if there's any test failure. Wait, there is a `tests/test-completion.el` failure for `corfu-popupinfo-enabled`. Why? Wait, in `test-completion.el`, we check for `(should corfu-popupinfo-mode)`. But `corfu-popupinfo-mode` is turned on asynchronously or globally? No, in `elisp/completion.el`:
```elisp
(use-package corfu
  ...
  :config
  (global-corfu-mode)
  (require 'corfu-history)
  (corfu-history-mode)
  (corfu-popupinfo-mode))
```
Wait! `corfu-popupinfo-mode` is a separate package/file in the `corfu` package, so we need to require it!
`(require 'corfu-popupinfo)`
Because in the test `test-completion.el`, `corfu-history-mode` works but `corfu-history-mode` is required. `global-corfu-mode` is autoloaded.
Let's check `elisp/completion.el`.
