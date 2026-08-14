;;; init-http.el --- HTTP / REST client (verb) -*- lexical-binding: t; -*-

;;; Commentary:

;; Org-based HTTP/REST client.  Requests are written as Org headings;
;; child headings inherit and override the parent's URL, headers, and
;; body, so a file of endpoints reads as an outline.  verb has no
;; external dependencies (it drives the built-in url.el), so nothing
;; here shells out to curl or a language runtime.

;;; Code:

;;; @doc Org-mode HTTP/REST client.  Write requests as Org headings and
;;; send them with `C-c C-r C-r'; responses (JSON, images, PDF) render
;;; in a side buffer.  `verb-command-map' is bound under `C-c C-r' in
;;; Org buffers, per verb's own convention.  Loads with Org (`:after
;;; org'), and since Org itself is deferred until the first `.org' file
;;; verb stays off the startup path.  url.el-backed, so no external
;;; curl/plz dependency.
(use-package verb
  :after org
  :config
  (keymap-set org-mode-map "C-c C-r" verb-command-map))

(provide 'init-http)
;;; init-http.el ends here
