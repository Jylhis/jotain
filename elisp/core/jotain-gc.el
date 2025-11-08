;;; jotain-gc.el --- Garbage collection optimization for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Optimizes garbage collection for better performance.
;; - Increases GC threshold after startup (set low in early-init.el)
;; - Runs GC during idle time
;; - Pauses GC during minibuffer interaction

;;; Code:

(defvar jotain-gc-cons-threshold (* 16 1024 1024)
  "The default value for `gc-cons-threshold'.
16MB is a reasonable balance between GC frequency and pause time.")

(defvar jotain-gc-cons-percentage 0.1
  "The default value for `gc-cons-percentage'.")

(defvar jotain-gc-timer nil
  "Timer for idle garbage collection.")

(defvar jotain-gc-minibuffer-threshold (* 50 1024 1024)
  "Threshold for `gc-cons-threshold' during minibuffer activity.
Higher threshold reduces GC pauses while typing in minibuffer.")

(defun jotain-gc-restore-defaults ()
  "Restore garbage collection to normal thresholds after init."
  (setq gc-cons-threshold jotain-gc-cons-threshold
        gc-cons-percentage jotain-gc-cons-percentage))

(defun jotain-gc-idle-collect ()
  "Run garbage collection when Emacs is idle."
  (when (not (active-minibuffer-window))
    (garbage-collect)))

(defun jotain-gc-minibuffer-enter ()
  "Increase GC threshold when entering minibuffer."
  (setq gc-cons-threshold jotain-gc-minibuffer-threshold))

(defun jotain-gc-minibuffer-exit ()
  "Restore GC threshold when exiting minibuffer."
  (setq gc-cons-threshold jotain-gc-cons-threshold))

(defun jotain-gc-initialize ()
  "Initialize garbage collection optimizations."
  ;; Restore normal GC thresholds after init
  (add-hook 'emacs-startup-hook #'jotain-gc-restore-defaults)
  
  ;; Run GC when idle for 5 seconds
  (setq jotain-gc-timer
        (run-with-idle-timer 5 t #'jotain-gc-idle-collect))
  
  ;; Increase GC threshold during minibuffer use
  (add-hook 'minibuffer-setup-hook #'jotain-gc-minibuffer-enter)
  (add-hook 'minibuffer-exit-hook #'jotain-gc-minibuffer-exit))

;; Initialize GC optimizations
(jotain-gc-initialize)

(provide 'jotain-gc)
;;; jotain-gc.el ends here
