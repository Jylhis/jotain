;;; init-devenv.el --- devenv.sh project tooling -*- lexical-binding: t; -*-

;;; Commentary:

;; Wiring for the in-repo devenv.sh integration library
;; (lisp/devenv.el).  The library is self-contained and reusable; this
;; module only binds it into the Jotain configuration:
;;
;;   - `C-c v' opens the devenv transient (tasks, scripts, processes,
;;     test/build, introspection, environment reload, MCP).
;;   - devenv.nix buffers get the bundled `devenv lsp' language server
;;     (a nixd preconfigured with the project's devenv options) while
;;     `nil' keeps serving every other Nix buffer.
;;
;; Environment loading is done natively by the library's own loader
;; (`devenv-env-global-mode', enabled below): direnv/envrc is disabled
;; (init-prog.el), so `devenv print-dev-env' is evaluated per project and
;; applied buffer-locally.  `devenv-env-defer-to-direnv' is set to nil so
;; the loader owns the environment for every trusted devenv project rather
;; than deferring to a `.envrc'.  Projects must be trusted once with
;; `devenv-allow' (`C-c v'); until then the mode line shows devenv[!] and
;; no environment is applied.
;;
;; The devenv-side counterpart of this integration is the Claude Code
;; MCP wiring in devenv.nix (`claude.code.enable'); see also
;; `devenv-mcp-setup' for exposing `devenv mcp' to gptel via mcp.el.

;;; Code:

;;; @doc Native devenv.sh integration (the in-repo `lisp/devenv.el`
;;; library). `C-c v` opens a transient with task and script runners,
;;; `devenv test`/`build` through compilation-mode with Nix error
;;; matching, a process-manager dashboard (start/stop/restart/logs),
;;; and environment introspection (`devenv eval`/`info`/`search`).
;;; `devenv-reload` re-runs `devenv print-dev-env` and re-applies the
;;; environment buffer-locally via the native loader
;;; (`devenv-env-global-mode`), and offers to reconnect eglot servers.
;;; devenv.nix buffers are routed to the bundled `devenv lsp` server
;;; while `nil` keeps serving other Nix files, and `devenv-mcp-setup`
;;; registers the project's `devenv mcp` server with mcp.el so gptel
;;; can call its tools. `devenv-allow`/`devenv-revoke` manage devenv
;;; 2.1's auto-activation trust database, which also gates the native
;;; env loader, and `devenv-modeline-mode` (enabled here) shows the
;;; per-buffer status — devenv[on]/[off]/[!] — in the mode line.
;;; Everything degrades to a clean error when the `devenv` binary is
;;; not on PATH.
(use-package devenv
  :ensure nil ; In-repo library (lisp/devenv.el)
  :defer t
  :commands (devenv-task-run devenv-script-run devenv-test devenv-build
             devenv-up devenv-down devenv-processes devenv-processes-logs
             devenv-info devenv-search devenv-eval devenv-reload
             devenv-allow devenv-revoke devenv-eglot-setup
             devenv-mcp-setup devenv-env-global-mode)
  :bind ("C-c v" . devenv)
  :custom
  ;; direnv/envrc is disabled (init-prog.el), so the native loader owns the
  ;; environment for every trusted devenv project, not just direnv-less ones.
  (devenv-env-defer-to-direnv nil)
  :hook ((after-init . devenv-modeline-mode)
         ;; Load and apply the project's devenv environment buffer-locally
         ;; (envrc-style) so eglot and CLI tools resolve from the devenv
         ;; toolchain.  Autoloaded, so devenv.el loads at after-init.
         (after-init . devenv-env-global-mode))
  :init
  ;; Route devenv.nix buffers to `devenv lsp' once eglot is loaded.
  ;; Gated on the binary so a machine without devenv keeps eglot's
  ;; stock Nix contact untouched.
  (with-eval-after-load 'eglot
    (when (executable-find "devenv")
      (devenv-eglot-setup))))

(provide 'init-devenv)
;;; init-devenv.el ends here
