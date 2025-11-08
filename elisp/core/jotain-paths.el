;;; jotain-paths.el --- XDG path management for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Manages XDG-compliant directory structure for Jotain.
;; Keeps user-emacs-directory clean by organizing files properly.

;;; Code:

(require 'jotain-lib)

(defgroup jotain-paths nil
  "Path management for Jotain."
  :group 'jotain)

(defcustom jotain-cache-dir
  (expand-file-name "jotain/" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for Jotain cache files."
  :type 'directory
  :group 'jotain-paths)

(defcustom jotain-data-dir
  (expand-file-name "jotain/" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory for Jotain data files."
  :type 'directory
  :group 'jotain-paths)

(defcustom jotain-config-dir
  (expand-file-name "jotain/" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "Directory for Jotain configuration files."
  :type 'directory
  :group 'jotain-paths)

(defcustom jotain-state-dir
  (expand-file-name "jotain/" (or (getenv "XDG_STATE_HOME") "~/.local/state"))
  "Directory for Jotain state files."
  :type 'directory
  :group 'jotain-paths)

(defun jotain-paths-cache-file (name)
  "Return path to cache file NAME in jotain-cache-dir."
  (expand-file-name name jotain-cache-dir))

(defun jotain-paths-data-file (name)
  "Return path to data file NAME in jotain-data-dir."
  (expand-file-name name jotain-data-dir))

(defun jotain-paths-state-file (name)
  "Return path to state file NAME in jotain-state-dir."
  (expand-file-name name jotain-state-dir))

(defun jotain-paths-initialize ()
  "Initialize Jotain directory structure.
Creates all necessary directories if they don't exist."
  (jotain-lib-ensure-directory jotain-cache-dir)
  (jotain-lib-ensure-directory jotain-data-dir)
  (jotain-lib-ensure-directory jotain-config-dir)
  (jotain-lib-ensure-directory jotain-state-dir))

;; Initialize paths on load
(jotain-paths-initialize)

(provide 'jotain-paths)
;;; jotain-paths.el ends here
