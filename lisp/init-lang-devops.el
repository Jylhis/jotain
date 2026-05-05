;;; init-lang-devops.el --- Infrastructure-as-code language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes for files you edit when wiring up CI, containers, and
;; infrastructure: Dockerfile, docker-compose, terraform, gitlab-ci,
;; justfile, ansible. None of these are huge — they mostly exist to
;; pin a `:mode' regex and provide font-lock.

;;; Code:

;;; @doc Dockerfile major mode — syntax for the file you edit every
;;; time you change a container image.
(use-package dockerfile-mode
  :defer t)

;;; @doc YAML-flavoured docker-compose syntax with awareness of compose
;;; keywords and service references.
(use-package docker-compose-mode
  :defer t)

;;; @doc HCL-aware Terraform mode for `.tf` files. Loaded by the mode
;;; regex; LSP comes from terraform-ls (configured in init-prog).
(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

;;; @doc YAML mode tuned for `.gitlab-ci.yml` keywords — includes,
;;; rules, jobs. Saves a lot of typo'd job names on CI debugging
;;; days.
(use-package gitlab-ci-mode
  :defer t)

;;; @doc Major mode for `Justfile` — the project-aware command runner
;;; Jotain itself uses. Pairs with compile-multi for project
;;; commands.
(use-package just-mode
  :defer t)

;;; @doc Ansible minor mode layered on top of yaml-mode for playbook
;;; files. Adds module-name completion and Jinja2 highlighting.
(use-package ansible
  :defer t)

(provide 'init-lang-devops)
;;; init-lang-devops.el ends here
