;;; init-devenv.el --- devenv.sh project tooling -*- lexical-binding: t; -*-

;;; Commentary:

;; Binds the self-contained lisp/devenv.el library into the configuration.
;;
;; envrc is not enabled (init-prog.el), so the native loader owns each
;; trusted project's environment, `.envrc' or not (hence
;; `devenv-env-defer-to-direnv' nil).  A project must be trusted once
;; with `devenv-allow'; until then the mode line shows devenv[!] and no
;; environment is applied.

;;; Code:

(use-package devenv
  :ensure nil
  :defer t
  :commands (devenv-task-run devenv-script-run devenv-test devenv-build
             devenv-up devenv-down devenv-processes devenv-processes-logs
             devenv-info devenv-search devenv-eval devenv-reload
             devenv-allow devenv-revoke devenv-shell devenv-eglot-setup
             devenv-mcp-setup devenv-env-global-mode)
  :bind ("C-c v" . devenv)
  :custom
  (devenv-env-defer-to-direnv nil)
  ;; devenv-modeline-mode already shows devenv[on]/[off]/[!]; the
  ;; minor-mode lighter would only duplicate it.
  (devenv-env-lighter "")
  :hook ((after-init . devenv-modeline-mode)
         ;; Autoloaded, so devenv.el loads at after-init.
         (after-init . devenv-env-global-mode))
  :init
  ;; Gated on the binary so a machine without devenv keeps eglot's
  ;; stock Nix contact.
  (with-eval-after-load 'eglot
    (when (executable-find "devenv")
      (devenv-eglot-setup))))

(provide 'init-devenv)
;;; init-devenv.el ends here
