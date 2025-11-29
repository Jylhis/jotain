;;; test-platform.el --- Tests for platform detection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the platform detection system to ensure it works correctly
;; across different environments.

;;; Code:

(require 'ert)
(require 'platform)

(ert-deftest test-platform-constants ()
  "Test that platform detection constants are properly defined."
  :tags '(unit platform fast)
  (should (booleanp platform-android-p))
  (should (booleanp platform-macos-p))
  (should (booleanp platform-linux-p))
  (should (booleanp platform-windows-p))
  (should (booleanp platform-gui-p))
  (should (booleanp platform-terminal-p))

  ;; GUI and terminal should be mutually exclusive
  (should (not (and platform-gui-p platform-terminal-p))))

(ert-deftest test-platform-has-feature-p ()
  "Test platform feature detection."
  :tags '(unit platform fast)
  (should (booleanp (platform-has-feature-p 'notifications)))
  (should (booleanp (platform-has-feature-p 'clipboard)))
  (should (booleanp (platform-has-feature-p 'fonts)))
  (should (booleanp (platform-has-feature-p 'native-comp)))
  (should (booleanp (platform-has-feature-p 'treesitter)))

  ;; Unknown features should return nil
  (should (null (platform-has-feature-p 'unknown-feature))))

(ert-deftest test-platform-get-info ()
  "Test platform information gathering."
  :tags '(unit platform fast)
  (let ((info (platform-get-info)))
    (should (listp info))
    (should (assoc 'system-type info))
    (should (assoc 'android-p info))
    (should (assoc 'macos-p info))
    (should (assoc 'linux-p info))
    (should (assoc 'windows-p info))
    (should (assoc 'gui-p info))
    (should (assoc 'terminal-p info))))

(ert-deftest test-platform-macros ()
  "Test platform conditional macros."
  :tags '(unit platform fast)
  (let ((result nil))
    ;; platform-when should work like when
    (platform-when t (setq result 'success))
    (should (eq result 'success))

    (setq result nil)
    (platform-when nil (setq result 'failure))
    (should (null result))

    ;; platform-unless should work like unless
    (setq result nil)
    (platform-unless nil (setq result 'success))
    (should (eq result 'success))

    (setq result nil)
    (platform-unless t (setq result 'failure))
    (should (null result))))

(ert-deftest test-platform-android-detection ()
  "Test Android platform detection logic."
  :tags '(unit platform)
  ;; Test that the constant is defined and is a boolean
  (should (booleanp platform-android-p))

  ;; Test the detection logic manually with mocked environment
  ;; Since platform-android-p is a defconst evaluated at load time,
  ;; we can't change it, but we can verify the logic would work correctly
  (let ((mock-system-type 'gnu/linux)
        (mock-termux-version "1.0")
        (mock-prefix "/data/data/com.termux/files/usr"))

    ;; Simulate the Android detection logic
    (let ((would-detect-android
           (and (eq mock-system-type 'gnu/linux)
                (or mock-termux-version
                    (string-match-p "android" (or mock-prefix ""))))))
      (should would-detect-android)))

  ;; Test non-Android Linux case
  (let ((mock-system-type 'gnu/linux)
        (mock-termux-version nil)
        (mock-prefix "/usr"))
    (let ((would-detect-android
           (and (eq mock-system-type 'gnu/linux)
                (or mock-termux-version
                    (string-match-p "android" (or mock-prefix ""))))))
      (should-not would-detect-android)))

  ;; Test that current platform detection is consistent
  ;; If we're on Linux, android-p should be false unless we have Android indicators
  (when (eq system-type 'gnu/linux)
    (let ((has-termux (getenv "TERMUX_VERSION"))
          (has-android-prefix (and (getenv "PREFIX")
                                   (string-match-p "android" (getenv "PREFIX")))))
      (if (or has-termux has-android-prefix)
          (should platform-android-p)
        (should-not platform-android-p)))))

(provide 'test-platform)
;;; test-platform.el ends here