;;; init-lang-qml.el --- QML (Quickshell / Qt Quick) support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
