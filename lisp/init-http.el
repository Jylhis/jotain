;;; init-http.el --- HTTP / REST client (verb) -*- lexical-binding: t; -*-

;;; Commentary:

;; Org-based HTTP/REST client.  Child headings inherit and override the
;; parent's URL, headers, and body, so a file of endpoints reads as an
;; outline.

;;; Code:

(use-package verb
  :after org
  :config
  (keymap-set org-mode-map "C-c C-r" verb-command-map))

(provide 'init-http)
;;; init-http.el ends here
