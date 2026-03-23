;;; early-init.el --- Jotain Emacs Configuration --- Early Init -*- lexical-binding: t; -*-
;;
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Package-Requires: ((emacs "30.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Early initialization file for Jotain Emacs
;; 

;;; Code:


;;; From emacs-solo
;; https://github.com/LionyxML/emacs-solo/blob/98bef4f1241980a8d268e5c286b1f3a05df7d425/early-init.el
;; TODO: Benchmark

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil) ; EMACS-31

;;; end emacs-solo


;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Configure tree-sitter grammar search path (provided by Nix)
;; Tree-sitter grammars are installed by Nix and exposed via TREE_SITTER_DIR
;; See: nix/lib/runtime-deps.nix and emacs.nix for grammar provisioning
(when-let ((ts-dir (getenv "TREE_SITTER_DIR")))
  (setq treesit-extra-load-path (list ts-dir)))

;; Package management — conditional on Nix environment
;; Under Nix: disable package.el entirely (packages provided by Nix store)
;; Without Nix: allow package.el to bootstrap packages on first launch
(if (getenv "NIX_PROFILES")
    (progn
      (setq package-enable-at-startup nil)
      (setq package-check-signature nil))
  (setq package-enable-at-startup t))

;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)
;; Default to nil; jotain-platform.el sets to t when not under Nix
(setq use-package-always-ensure nil)
(when (getenv "NIX_PROFILES")
  (setq use-package-ensure-function 'ignore))

;; Native compilation settings (Emacs 30+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence compiler warnings during init
  (setq native-comp-async-report-warnings-errors nil)
  ;; Don't store eln-cache in user-emacs-directory
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))
  ;; Increase native compilation speed
  (setq native-comp-speed 2))

;; UI optimizations - disable UI elements before frame creation
;; Faster to disable these here (before they've been initialized)
;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))
;; TODO: what is the difference between one below and up?
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)


;; TODO: What is this?
;; (setq mode-line-format nil)

;; Prevent unwanted runtime compilation for performance
(setq byte-compile-warnings '(not obsolete))

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

(provide 'early-init)
;;; early-init.el ends here
