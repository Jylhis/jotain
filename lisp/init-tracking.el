;;; init-tracking.el --- Editor activity instrumentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Passive observation of editor usage. The three tools here are
;; complementary: keyfreq counts commands, wakatime counts coding time
;; against project files, activity-watch correlates editor activity
;; with what the rest of your machine is doing.

;;; Code:

;; Records command frequency to disk; `M-x keyfreq-show' ranks the
;; busiest commands. Tiny, no daemon, no network.
(use-package keyfreq
  :diminish keyfreq-mode
  :functions (keyfreq-mode keyfreq-autosave-mode)
  :custom
  (keyfreq-file (jotain-var-file "keyfreq.el"))
  (keyfreq-file-lock (jotain-var-file "keyfreq.lock"))
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Heartbeats to a Wakatime/Wakapi server. The :if guard makes the
;; whole block a no-op when wakatime-cli is not on PATH, so you can
;; install Wakapi later without reconfiguring Emacs.
(use-package wakatime-mode
  :if (and (executable-find "wakatime-cli")
           (getenv "WAKATIME_API_KEY"))
  :diminish wakatime-mode
  :functions (global-wakatime-mode)
  :custom (wakatime-cli-path (executable-find "wakatime-cli"))
  :config (global-wakatime-mode))

;; ActivityWatch — installed but OFF by default. Enable with
;; `M-x global-activity-watch-mode' once the AW server is running,
;; or set the option in custom.el for persistent on.
(use-package activity-watch-mode
  :defer t
  :commands (activity-watch-mode global-activity-watch-mode))

(provide 'init-tracking)
;;; init-tracking.el ends here
