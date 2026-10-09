;;; init-devenv.el --- devenv.sh project tooling -*- lexical-binding: t; -*-

;;; Commentary:

;; Binds the self-contained lisp/devenv.el library into Jotain.
;;
;; envrc is not enabled (init-prog.el), so the native loader owns each
;; trusted project's environment, `.envrc' or not (hence
;; `devenv-env-defer-to-direnv' nil).  A project must be trusted once
;; with `devenv-allow'; until then the mode line shows devenv[!] and no
;; environment is applied.

;;; Code:

;;; @doc Native devenv.sh integration (the in-repo `lisp/devenv.el`
;;; library). `C-c v` opens a transient with task and script runners,
;;; `devenv test`/`build` in compilation-mode with Nix error matching,
;;; a process dashboard (start/stop/restart/logs), and introspection
;;; (`devenv eval`/`info`/`search`). `devenv-env-global-mode` applies
;;; each project's `devenv print-dev-env` environment buffer-locally;
;;; `devenv-reload` refreshes it and offers to reconnect eglot.
;;; devenv.nix buffers get the bundled `devenv lsp` server; other Nix
;;; buffers get nixd or nil, the project's own first. `devenv-mcp-setup`
;;; registers the project's `devenv mcp` server with mcp.el for gptel.
;;; `devenv-allow`/`devenv-revoke` manage devenv's auto-activation trust
;;; database, which also gates the loader, and `devenv-modeline-mode`
;;; shows devenv[on]/[off]/[!] in the mode line. Without the `devenv`
;;; binary on PATH, commands fail with a clear error.
(use-package devenv
  :ensure nil
  :defer t
  :commands (devenv-task-run devenv-script-run devenv-test devenv-build
             devenv-up devenv-down devenv-processes devenv-processes-logs
             devenv-info devenv-search devenv-eval devenv-reload
             devenv-allow devenv-revoke devenv-eglot-setup
             devenv-mcp-setup devenv-env-global-mode)
  :bind ("C-c v" . devenv)
  :custom
  (devenv-env-defer-to-direnv nil)
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
