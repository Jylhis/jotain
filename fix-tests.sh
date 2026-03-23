#!/usr/bin/env bash
nix shell nixpkgs#emacs -c emacs --batch -L elisp -L tests -l elisp/platform.el -l elisp/programming.el -l elisp/completion.el -l tests/test-completion.el -f ert-run-tests-batch-and-exit
