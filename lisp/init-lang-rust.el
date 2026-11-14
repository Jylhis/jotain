;;; init-lang-rust.el --- Rust language support -*- lexical-binding: t; -*-

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

;; Built-in `rust-ts-mode' and rust-analyzer workspace settings.  The
;; eglot hook lives in `init-prog'.

;;; Code:

(declare-function my-eglot-set-workspace-config "init-prog" (key settings))

;; `check.command "clippy"' runs clippy on every save, slow on a cold
;; target directory; a project .dir-locals.el can switch back to `check'.
;; `cargo (:features "all")' is left out: costly on large workspaces, so
;; it stays a per-project choice.
(my-eglot-set-workspace-config
 :rust-analyzer
 '(:check (:command "clippy")
   :cargo (:buildScripts (:enable t))
   :procMacro (:enable t)))

;;; @doc Built-in tree-sitter Rust mode. rust-analyzer runs clippy on
;;; save with build scripts and proc macros enabled; format-on-save runs
;;; rustfmt through apheleia.
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-ts-mode-indent-offset 4))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
