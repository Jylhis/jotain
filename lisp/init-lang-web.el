;;; init-lang-web.el --- Web frontend language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Web frontend modes: TypeScript/TSX, JavaScript, HTML, CSS/SCSS, and
;; `web-mode' for HTML templating languages.  JSON routes to
;; `json-ts-mode' through `jotain-prog-ts-remaps', and all eglot wiring
;; lives in `init-prog.el'.

;;; Code:

;;; @doc Built-in tree-sitter TypeScript / TSX / JSX modes.
;;; typescript-language-server is wired in init-prog.
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)))

;;; @doc Built-in JavaScript major mode pinned to its tree-sitter
;;; variant.
(use-package js
  :ensure nil
  :mode ("\\.js\\'" . js-ts-mode))

;;; @doc Built-in CSS / SCSS major mode pinned to the tree-sitter
;;; variant.
(use-package css-mode
  :ensure nil
  :mode (("\\.css\\'"  . css-ts-mode)
         ("\\.scss\\'" . css-ts-mode)))

;;; @doc Built-in tree-sitter HTML mode (Emacs 31) for plain
;;; `.html`/`.htm`, including embedded JS and CSS. Templating dialects go
;;; to web-mode below.
(use-package mhtml-ts-mode
  :ensure nil
  :mode "\\.html?\\'")

;;; @doc One mode for HTML templating languages: ERB, Mustache, Django,
;;; ASP, JSP, PHP. `M-o` is rebound so web-mode-map does not shadow the
;;; global `other-window`.
(use-package web-mode
  :mode (("\\.phtml\\'"   . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'"     . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'"  . web-mode))
  :bind (:map web-mode-map
              ("M-o" . other-window))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t))

(provide 'init-lang-web)
;;; init-lang-web.el ends here
