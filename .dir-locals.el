;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (fill-column . 80))
 (emacs-lisp-mode
  (sentence-end-double-space . t)
  (projection-commands-build-project . "just build")
  (projection-commands-test-project . "just test-fast")
  (projection-commands-run-project . "just emacs-dev")
  (projection-commands-configure-project . "nix develop")
  (projection-commands-package-project . "just build")
  (projection-commands-install-project . "nix build")))
