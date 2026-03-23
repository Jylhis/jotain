;;; jotain-platform.el --- Platform and environment detection -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:
;; Extends Emacs environment detection with Nix awareness, platform
;; identification, and conditional package archive configuration.
;;
;; Dependencies: jotain-defaults (for version check — loads first in init.el)
;;
;; Key variables:
;;   `jotain/nix-managed-p' — t when running under Nix package management
;;   `jotain/platform'      — symbol identifying the runtime platform

;;; Code:

(require 'jotain-defaults)

;;; Nix Detection

(defvar jotain/nix-managed-p (not (null (getenv "NIX_PROFILES")))
  "Non-nil when Emacs is running in a Nix-managed environment.
Determined by checking for NIX_PROFILES in the environment.")

;;; Platform Detection

(defvar jotain/platform
  (cond
   ((getenv "TERMUX_VERSION") 'android-aarch64)
   ((and (eq system-type 'gnu/linux)
         (string-match-p "aarch64" system-configuration))
    'linux-aarch64)
   ((and (eq system-type 'gnu/linux)
         (string-match-p "x86_64" system-configuration))
    'linux-x86_64)
   (t 'unknown))
  "Symbol identifying the runtime platform.
One of: `linux-x86_64', `linux-aarch64', `android-aarch64', or `unknown'.")

;;; Package Archive Configuration

(unless jotain/nix-managed-p
  ;; Non-Nix: configure package archives for automatic package installation
  (require 'package)
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (setq use-package-always-ensure t))

(provide 'jotain-platform)
;;; jotain-platform.el ends here
