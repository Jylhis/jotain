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

;; Forward declaration so `jotain--apply-docker-backend' byte-compiles
;; cleanly under `byte-compile-error-on-warn'.  The defcustom below
;; supplies the real binding; this only silences the compiler.
(defvar jotain-docker-backend)

(defun jotain--apply-docker-backend ()
  "Apply `jotain-docker-backend' to dockerfile-mode.
Sets the runtime command used by `dockerfile-mode'.  The variable is
only touched once its owning package has loaded, so this is safe to
call before or after."
  (let ((cmd (if (eq jotain-docker-backend 'podman) "podman" "docker")))
    (when (boundp 'dockerfile-mode-command)
      (setq dockerfile-mode-command cmd))))

(defcustom jotain-docker-backend 'podman
  "Container runtime that Docker-aware packages should drive.
`podman' (default) is rootless and daemonless; `docker' uses the
classic dockerd pair.  Changing this re-applies the runtime command
name used by `dockerfile-mode'."
  :type '(choice (const :tag "Podman" podman)
                 (const :tag "Docker" docker))
  :group 'jotain-devops
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (jotain--apply-docker-backend)))

;;; @doc Dockerfile major mode — syntax highlighting plus build
;;; commands (`M-x dockerfile-build-buffer`). The runtime command
;;; name comes from `jotain-docker-backend`.
(use-package dockerfile-mode
  :defer t
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

;;; @doc Bazel/Starlark support — major modes for `BUILD`, `WORKSPACE`,
;;; `MODULE.bazel`, `REPO.bazel`, `*.bzl`, `.bazelrc`, `.bazelignore`
;;; and `.bazeliskrc` (auto-mode-alist comes from the package's own
;;; autoloads). `C-c C-f` runs buildifier; format-on-save for the
;;; Starlark-family buffers is wired through apheleia in init-prog.
(use-package bazel
  :defer t)

(provide 'init-lang-devops)
;;; init-lang-devops.el ends here
