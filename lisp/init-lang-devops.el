;;; init-lang-devops.el --- Infrastructure-as-code language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes for files you edit when wiring up CI, containers, and
;; infrastructure: Dockerfile, docker-compose, terraform, gitlab-ci,
;; justfile, ansible. None of these are huge — they mostly exist to
;; pin a `:mode' regex and provide font-lock.

;;; Code:

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package gitlab-ci-mode)

(use-package just-mode)

;; ansible-mode is a minor mode that hooks onto yaml-mode for playbooks.
(use-package ansible
  :defer t)

(provide 'init-lang-devops)
;;; init-lang-devops.el ends here
