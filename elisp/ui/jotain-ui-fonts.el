;;; jotain-ui-fonts.el --- Font configuration for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Font configuration with sensible defaults and fallbacks.

;;; Code:

(defvar jotain-ui-default-font "Monospace"
  "Default font family to use.")

(defvar jotain-ui-default-font-size 12
  "Default font size.")

(defvar jotain-ui-variable-pitch-font "Sans Serif"
  "Variable pitch font family.")

(defun jotain-ui--font-exists-p (font)
  "Check if FONT is available on this system."
  (find-font (font-spec :name font)))

(defun jotain-ui--setup-fonts ()
  "Set up fonts with fallbacks."
  (let ((mono-fonts '("JetBrains Mono" "Fira Code" "Cascadia Code" 
                      "SF Mono" "Menlo" "Monaco" "Consolas" "Monospace"))
        (variable-fonts '("Inter" "San Francisco" "Segoe UI" "Helvetica" 
                          "Arial" "Sans Serif")))
    
    ;; Set default monospace font
    (catch 'font-found
      (dolist (font mono-fonts)
        (when (jotain-ui--font-exists-p font)
          (set-face-attribute 'default nil
                              :family font
                              :height (* jotain-ui-default-font-size 10))
          (set-face-attribute 'fixed-pitch nil
                              :family font)
          (throw 'font-found t))))
    
    ;; Set variable pitch font
    (catch 'font-found
      (dolist (font variable-fonts)
        (when (jotain-ui--font-exists-p font)
          (set-face-attribute 'variable-pitch nil
                              :family font
                              :height (* jotain-ui-default-font-size 10))
          (throw 'font-found t))))))

;; Set up fonts after frame creation
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'jotain-ui--setup-fonts)
  (add-hook 'after-init-hook #'jotain-ui--setup-fonts))

(provide 'jotain-ui-fonts)
;;; jotain-ui-fonts.el ends here
