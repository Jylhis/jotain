;;; ai.el --- AI and LLM integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered tools and language model integrations.

;;; Code:

(use-package aidermacs :ensure)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'ai)
;;; ai.el ends here