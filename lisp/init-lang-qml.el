;;; init-lang-qml.el --- QML (Quickshell / Qt Quick) support -*- lexical-binding: t; -*-

;;; Commentary:

;; QML is the Qt Modeling Language, used here for the owner's Quickshell
;; desktop shell.  Like Nix, it earns its own file: it fits none of the
;; grouped language files (web is TS/CSS/HTML, systems is C/C++/Zig).  The
;; eglot hook (qmlls) and the apheleia formatter (qmlformat) are wired in
;; `init-prog' so all language servers and formatters stay in one place.

;;; Code:

;;; @doc Tree-sitter QML major mode (xhcoding/qml-ts-mode), for editing
;;; Quickshell / Qt Quick `.qml' files.  Provided by Nix; its `qmljs'
;;; grammar ships via treesit-grammars.with-all-grammars.  LSP through
;;; eglot → qmlls and format-on-save through apheleia → qmlformat are
;;; both configured in init-prog.  The Qt tooling (qmlls/qmlformat) rides
;;; the distribution wrapper PATH (nix/runtime-deps.nix).
(use-package qml-ts-mode
  :ensure nil ; Provided by Nix
  :mode "\\.qml\\'")

(provide 'init-lang-qml)
;;; init-lang-qml.el ends here
