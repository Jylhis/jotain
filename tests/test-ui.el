;;; test-ui.el --- Tests for UI and theme system -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the theme system to verify configuration, advice, and
;; preloading behavior.

;;; Code:

(require 'ert)
(require 'ui)

(ert-deftest test-ui-custom-safe-themes ()
  "Test that custom-safe-themes is set to t."
  :tags '(unit ui fast)
  (should (eq custom-safe-themes t)))

(ert-deftest test-ui-load-theme-advice-installed ()
  "Test that load-theme has the disable-all-themes advice."
  :tags '(unit ui fast)
  (should (advice--p (advice--symbol-function 'load-theme)))
  (should (advice-member-p #'jotain-ui--disable-all-themes 'load-theme)))

(ert-deftest test-ui-advice-respects-no-enable ()
  "Test that advice does not disable themes when NO-ENABLE is t."
  :tags '(unit ui fast)
  (let ((disable-called nil))
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (_theme) (setq disable-called t))))
      ;; Simulate load-theme with NO-ENABLE = t
      (jotain-ui--disable-all-themes 'some-theme nil t)
      (should-not disable-called))))

(ert-deftest test-ui-advice-disables-when-enabling ()
  "Test that advice disables all themes when NO-ENABLE is nil."
  :tags '(unit ui fast)
  (let ((disabled-themes nil)
        (custom-enabled-themes '(theme-a theme-b)))
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled-themes))))
      (jotain-ui--disable-all-themes 'some-theme nil nil)
      (should (member 'theme-a disabled-themes))
      (should (member 'theme-b disabled-themes))
      (should (= (length disabled-themes) 2)))))

(ert-deftest test-ui-preload-themes-defined ()
  "Test that jotain-ui--preload-themes is defined."
  :tags '(unit ui fast)
  (should (fboundp 'jotain-ui--preload-themes)))

(ert-deftest test-ui-theme-variables-defined ()
  "Test that theme customization variables are defined and are symbols."
  :tags '(unit ui fast)
  (should (boundp 'jotain-theme-light))
  (should (boundp 'jotain-theme-dark))
  (should (symbolp jotain-theme-light))
  (should (symbolp jotain-theme-dark)))

(ert-deftest test-ui-auto-dark-theme-config ()
  "Test that auto-dark is loaded with correct theme configuration."
  :tags '(unit ui fast)
  (should (boundp 'auto-dark-themes))
  (should (equal auto-dark-themes
                 `((,jotain-theme-light) (,jotain-theme-dark)))))

(provide 'test-ui)
;;; test-ui.el ends here
