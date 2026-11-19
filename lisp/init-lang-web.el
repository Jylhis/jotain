;;; init-lang-web.el --- Web frontend language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Web frontend modes: TypeScript/TSX, JavaScript, HTML, CSS/SCSS, Astro,
;; and `web-mode' for HTML templating languages.  All eglot wiring lives
;; in `init-prog.el'.

;;; Code:

(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.jsx\\'" . tsx-ts-mode))

(use-package js
  :ensure nil
  :mode ("\\.js\\'" . js-ts-mode))

(use-package css-mode
  :ensure nil
  :mode (("\\.css\\'"  . css-ts-mode)
         ("\\.scss\\'" . scss-mode)))

(use-package mhtml-ts-mode
  :ensure nil
  :mode "\\.html?\\'")

(use-package astro-ts-mode
  :mode "\\.astro\\'")

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
