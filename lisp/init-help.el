;;; init-help.el --- Help system upgrades -*- lexical-binding: t; -*-

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

;; Built-in help tweaks plus `helpful', a richer replacement for the
;; default `describe-*' commands.

;;; Code:

;;; @doc Built-in help window, selected on display so q dismisses it
;;; at once; navigation inside *Help* (source, Info) reuses its window.
(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  (help-window-keep-selected t)
  :config
  ;; The keystroke log (C-h l) updates live.
  (setopt view-lossage-auto-refresh t))

;;; @doc Built-in echo-area help for buttons and links when point
;;; lingers on them.
(use-package help-at-pt
  :ensure nil
  :custom
  (help-at-pt-display-when-idle t))

;;; @doc Built-in apropos with `apropos-do-all`, so searches also cover
;;; non-interactive functions, all variables, and all symbols.
(use-package apropos
  :ensure nil
  ;; Autoloaded on demand; keep it off the startup path.
  :defer t
  :custom
  (apropos-do-all t))

;;; @doc Replaces the default describe-* commands with richer buffers
;;; that include source, callers, and active keybindings.
(use-package helpful
  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-h F"   . helpful-function)
   ("C-h C"   . helpful-command)
   ([remap display-local-help] . helpful-at-point)
   ([remap describe-symbol] . helpful-symbol)))

(provide 'init-help)
;;; init-help.el ends here
