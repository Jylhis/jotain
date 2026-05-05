;;; init-lang-web.el --- Web frontend language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Modes for the web frontend stack: TypeScript/TSX, HTML/CSS, JSON,
;; and `web-mode' for templating languages (ERB, Mustache, Django, ASP).
;; Built-in tree-sitter modes are pinned with :ensure nil; web-mode and
;; vue-mode etc. were dropped during review.
;;
;; Eglot hooks for TypeScript/TSX live centrally in `init-prog.el'.

;;; Code:

;;; @doc Built-in tree-sitter TypeScript / TSX / JSX modes. Eglot wires
;;; @doc typescript-language-server in init-prog.
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)))

;;; @doc Built-in JavaScript major mode pinned to its tree-sitter
;;; @doc variant.
(use-package js
  :ensure nil
  :mode ("\\.js\\'" . js-ts-mode))

;;; @doc Built-in CSS / SCSS major mode pinned to the tree-sitter
;;; @doc variant.
(use-package css-mode
  :ensure nil
  :mode (("\\.css\\'"  . css-ts-mode)
         ("\\.scss\\'" . css-ts-mode)))

;;; @doc One mode for every templating language that mixes HTML with
;;; @doc something else: ERB, Mustache, Django, ASP, JSP, PHP. The
;;; @doc M-o rebind below stops web-mode-map from shadowing our global
;;; @doc other-window binding.
(use-package web-mode
  :mode (("\\.html?\\'"   . web-mode)
         ("\\.phtml\\'"   . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'"     . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'"  . web-mode))
  :bind (:map web-mode-map
              ;; Don't shadow our global M-o → other-window.
              ("M-o" . other-window))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t))

(provide 'init-lang-web)
;;; init-lang-web.el ends here
