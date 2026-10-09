;;; init-lang-qml.el --- QML (Quickshell / Qt Quick) support -*- lexical-binding: t; -*-

;;; Commentary:

;; QML (Qt Modeling Language), used for the owner's Quickshell desktop
;; shell.  It fits none of the grouped language files.  qmlls (eglot) and
;; qmlformat (apheleia) are wired in `init-prog'.

;;; Code:

;;; @doc Tree-sitter QML major mode (xhcoding/qml-ts-mode) for Quickshell
;;; / Qt Quick `.qml' files. Provided by Nix, with the `qmljs' grammar
;;; from treesit-grammars.with-all-grammars. qmlls (eglot) and qmlformat
;;; (apheleia) are wired in init-prog and ride the distribution wrapper
;;; PATH (nix/runtime-deps.nix).
(use-package qml-ts-mode
  :ensure nil
  :mode "\\.qml\\'")

(provide 'init-lang-qml)
;;; init-lang-qml.el ends here
