;;; ai.el --- AI tools and assistants configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; AI coding assistants and LLM integrations.

;;; Code:

(use-package claude-code-ide
  :ensure t
  :defer t
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'ai)
;;; ai.el ends here
