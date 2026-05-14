;;; init-lang-devops.el --- Infrastructure-as-code language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes for files you edit when wiring up CI, containers, and
;; infrastructure: Dockerfile, docker-compose, terraform, gitlab-ci,
;; justfile, ansible. None of these are huge — they mostly exist to
;; pin a `:mode' regex and provide font-lock.

;;; Code:

(defgroup jotain-devops nil
  "Docker/Podman and infrastructure-as-code settings."
  :group 'convenience)

(defcustom jotain-docker-backend 'podman
  "Container runtime that Docker-aware packages should drive.
`podman' (default) is rootless and daemonless; `docker' uses the
classic dockerd/docker-compose pair.  Changing this re-applies
the relevant command-name variables for `dockerfile-mode' and the
`docker' package, including the TRAMP method used to open files
inside a running container."
  :type '(choice (const :tag "Podman" podman)
                 (const :tag "Docker" docker))
  :group 'jotain-devops
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (jotain--apply-docker-backend)))

(defun jotain--apply-docker-backend ()
  "Apply `jotain-docker-backend' to dockerfile-mode and docker.el.
Sets the runtime command, compose command, and TRAMP method used by
the relevant packages.  Variables are only touched if their owning
package has been loaded, so this is safe to call before or after."
  (let* ((podman (eq jotain-docker-backend 'podman))
         (cmd     (if podman "podman"         "docker"))
         (compose (if podman "podman-compose" "docker-compose")))
    (when (boundp 'dockerfile-mode-command)
      (setq dockerfile-mode-command cmd))
    (when (boundp 'docker-command)
      (setq docker-command cmd))
    (when (boundp 'docker-compose-command)
      (setq docker-compose-command compose))
    (when (boundp 'docker-container-tramp-method)
      (setq docker-container-tramp-method cmd))))

;;; @doc Dockerfile major mode — syntax highlighting plus build
;;; commands (`M-x dockerfile-build-buffer'). The runtime command
;;; name comes from `jotain-docker-backend'.
(use-package dockerfile-mode
  :defer t
  :config (jotain--apply-docker-backend))

;;; @doc Magit-style transient menu for containers, images, volumes
;;; and networks. `C-c d' opens the dispatcher; commands respect
;;; `jotain-docker-backend' (docker vs podman, including compose and
;;; the TRAMP method used to open files inside a running container).
(use-package docker
  :defer t
  :bind ("C-c d" . docker)
  :config (jotain--apply-docker-backend))

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
