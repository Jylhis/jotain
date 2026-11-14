;;; init-http.el --- HTTP / REST client (verb) -*- lexical-binding: t; -*-

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

;; Org-based HTTP/REST client.  Child headings inherit and override the
;; parent's URL, headers, and body, so a file of endpoints reads as an
;; outline.

;;; Code:

;;; @doc Org-mode HTTP/REST client.  Write requests as Org headings and
;;; send them with `C-c C-r C-r'; responses (JSON, images, PDF) render
;;; in another window.  `verb-command-map' is on `C-c C-r' in Org
;;; buffers, verb's own convention.  Loads with Org, so it stays off the
;;; startup path.  Uses the built-in url.el; no curl needed.
(use-package verb
  :after org
  :config
  (keymap-set org-mode-map "C-c C-r" verb-command-map))

(provide 'init-http)
;;; init-http.el ends here
