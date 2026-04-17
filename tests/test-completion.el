;;; test-completion.el --- Tests for completion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for completion.el module including vertico, corfu, consult, embark, and related packages.
;;
;; Tags:
;; - fast: Quick configuration checks (no heavy I/O)
;; - unit: Individual feature tests
;; - integration: Tests requiring loaded packages or multiple components

;;; Code:

(require 'ert)
(require 'completion)

;;; Vertico Configuration

(ert-deftest test-completion/vertico-enabled ()
  "Test that vertico mode is enabled."
  :tags '(fast unit)
  (should (fboundp 'vertico-mode))
  (should (boundp 'vertico-mode))
  (should vertico-mode))

(ert-deftest test-completion/vertico-directory-configured ()
  "Test that vertico-directory extension is configured."
  :tags '(unit)
  (should (fboundp 'vertico-directory-enter))
  (should (fboundp 'vertico-directory-delete-char))
  (should (fboundp 'vertico-directory-delete-word)))

(ert-deftest test-completion/vertico-multiform-enabled ()
  "Test that vertico-multiform mode is enabled."
  :tags '(unit)
  (should (fboundp 'vertico-multiform-mode))
  (should (boundp 'vertico-multiform-mode))
  (should vertico-multiform-mode))

(ert-deftest test-completion/vertico-multiform-configured ()
  "Test that vertico-multiform categories and commands are configured."
  :tags '(unit)
  (should (boundp 'vertico-multiform-categories))
  (should (listp vertico-multiform-categories))
  (should (assoc 'file vertico-multiform-categories))
  (should (boundp 'vertico-multiform-commands))
  (should (listp vertico-multiform-commands)))

(ert-deftest test-completion/vertico-buffer-configured ()
  "Test that vertico-buffer is properly configured."
  :tags '(fast unit)
  (should (boundp 'vertico-buffer-hide-prompt))
  (should-not vertico-buffer-hide-prompt)
  (should (boundp 'vertico-buffer-display-action))
  (should (listp vertico-buffer-display-action)))

;;; Corfu Configuration

(ert-deftest test-completion/corfu-enabled ()
  "Test that global corfu mode is enabled."
  :tags '(fast unit)
  (should (fboundp 'global-corfu-mode))
  (should (boundp 'global-corfu-mode))
  (should global-corfu-mode))

(ert-deftest test-completion/corfu-history-enabled ()
  "Test that corfu-history mode is enabled."
  :tags '(unit)
  (should (fboundp 'corfu-history-mode))
  (should (boundp 'corfu-history-mode))
  (should corfu-history-mode))

(ert-deftest test-completion/kind-icon-configured ()
  "Test that kind-icon is configured for corfu."
  :tags '(unit)
  (should (boundp 'kind-icon-default-face))
  (should (eq kind-icon-default-face 'corfu-default))
  (should (boundp 'kind-icon-blend-background))
  (should-not kind-icon-blend-background))

;;; Cape Configuration

(ert-deftest test-completion/cape-completers-registered ()
  "Test that cape completion functions are registered."
  :tags '(unit)
  (require 'cape)
  (should (boundp 'completion-at-point-functions))
  (should (member #'cape-dabbrev completion-at-point-functions))
  (should (member #'cape-file completion-at-point-functions))
  (should (member #'cape-keyword completion-at-point-functions)))

(ert-deftest test-completion/cape-configured ()
  "Test that cape is properly configured."
  :tags '(fast unit)
  (require 'cape)
  (should (boundp 'cape-file-directory-must-exist))
  (should cape-file-directory-must-exist)
  (should (boundp 'cape-file-prefix))
  (should (listp cape-file-prefix)))

;;; Consult Configuration

(ert-deftest test-completion/consult-available ()
  "Test that consult package is available."
  :tags '(unit)
  (should (fboundp 'consult-buffer))
  (should (fboundp 'consult-line))
  (should (fboundp 'consult-ripgrep))
  (should (fboundp 'consult-fd))
  (should (fboundp 'consult-imenu)))

(ert-deftest test-completion/consult-xref-integration ()
  "Test that consult is integrated with xref."
  :tags '(fast unit)
  (should (boundp 'xref-show-xrefs-function))
  (should (eq xref-show-xrefs-function #'consult-xref))
  (should (boundp 'xref-show-definitions-function))
  (should (eq xref-show-definitions-function #'consult-xref)))

;;; Orderless Configuration

(ert-deftest test-completion/orderless-configured ()
  "Test that orderless completion style is configured."
  :tags '(fast unit)
  (should (boundp 'completion-styles))
  (should (member 'orderless completion-styles))
  (should (member 'partial-completion completion-styles))
  (should (member 'flex completion-styles)))

(ert-deftest test-completion/orderless-category-overrides ()
  "Test that orderless has proper category overrides."
  :tags '(unit)
  (require 'orderless)
  (should (boundp 'completion-category-overrides))
  (should (listp completion-category-overrides))
  ;; Check file category uses partial-completion first
  (let ((file-override (assoc 'file completion-category-overrides)))
    (should file-override)
    (should (member 'partial-completion (cdr (assoc 'styles (cdr file-override)))))))

;;; Marginalia Configuration

(ert-deftest test-completion/marginalia-enabled ()
  "Test that marginalia mode is enabled."
  :tags '(fast unit)
  (should (fboundp 'marginalia-mode))
  (should (boundp 'marginalia-mode))
  (should marginalia-mode))

;;; Embark Configuration

(ert-deftest test-completion/embark-available ()
  "Test that embark is available."
  :tags '(unit)
  (should (fboundp 'embark-act))
  (should (fboundp 'embark-dwim))
  (should (fboundp 'embark-bindings)))

(ert-deftest test-completion/embark-prefix-help ()
  "Test that embark is configured as prefix help command."
  :tags '(fast unit)
  (should (boundp 'prefix-help-command))
  (should (eq prefix-help-command #'embark-prefix-help-command)))

(ert-deftest test-completion/embark-consult-integration ()
  "Test that embark-consult integration is configured."
  :tags '(unit)
  ;; embark-consult loads automatically after embark and consult
  (require 'embark)
  (require 'consult)
  (require 'embark-consult)
  (should (featurep 'embark-consult))
  (should (fboundp 'embark-export)))

;;; Avy Configuration

(ert-deftest test-completion/avy-available ()
  "Test that avy is available."
  :tags '(unit)
  (should (fboundp 'avy-goto-char))
  (should (fboundp 'avy-goto-line))
  (should (fboundp 'avy-goto-word-1)))

(ert-deftest test-completion/avy-configured ()
  "Test that avy is properly configured."
  :tags '(fast unit)
  (require 'avy)
  (should (boundp 'avy-all-windows))
  (should (eq avy-all-windows 'all-frames)))

;;; Zoxide Configuration

(ert-deftest test-completion/zoxide-available ()
  "Test that zoxide is available."
  :tags '(unit)
  (should (fboundp 'zoxide-find-file))
  (should (fboundp 'zoxide-add)))

;;; Integration Tests

(ert-deftest test-completion/completion-system-integrated ()
  "Test that the completion system is properly integrated."
  :tags '(integration)
  ;; Vertico for vertical completion
  (should vertico-mode)
  ;; Corfu for in-buffer completion
  (should global-corfu-mode)
  ;; Marginalia for annotations
  (should marginalia-mode)
  ;; Orderless for flexible matching
  (should (member 'orderless completion-styles))
  ;; Consult for enhanced commands
  (should (fboundp 'consult-buffer))
  ;; Embark for actions
  (should (fboundp 'embark-act)))

(ert-deftest test-completion/minibuffer-enhancements ()
  "Test that minibuffer is enhanced with vertico and consult."
  :tags '(integration)
  ;; Vertico enhances minibuffer
  (should vertico-mode)
  ;; Consult provides better xref
  (should (eq xref-show-xrefs-function #'consult-xref))
  ;; Marginalia adds annotations
  (should marginalia-mode))

(provide 'test-completion)
;;; test-completion.el ends here
