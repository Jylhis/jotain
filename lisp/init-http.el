;;; init-http.el --- HTTP / REST client (verb) -*- lexical-binding: t; -*-

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
