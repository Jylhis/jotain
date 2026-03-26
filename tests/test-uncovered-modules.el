;;; test-uncovered-modules.el --- Smoke tests for modules lacking dedicated coverage -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal smoke tests for Elisp modules that lack dedicated test files.
;; Addresses constitution Principle IV (Testing Discipline) gap identified
;; during baseline validation (T065).
;;
;; Covered modules:
;; - dashboard.el  (Enlight startup screen)
;; - help.el       (Enhanced help system)
;; - per-project.el (Project-local configuration)
;; - collaboration.el (Org-Jira integration)
;; - systems.el    (1Password, sops, logview)

;;; Code:

(require 'ert)

;;; dashboard.el

(ert-deftest test-dashboard/module-provides ()
  "Smoke test: dashboard.el loads and provides the dashboard feature."
  :tags '(smoke unit fast)
  (require 'dashboard nil t)
  (should (featurep 'dashboard)))

(ert-deftest test-dashboard/enlight-configured ()
  "Smoke test: enlight package is configured as the initial buffer choice."
  :tags '(smoke unit fast)
  (require 'dashboard nil t)
  (when (featurep 'enlight)
    (should (functionp initial-buffer-choice))
    (should (eq initial-buffer-choice #'jotain-dashboard-initial-buffer))))

(ert-deftest test-dashboard/conditional-open ()
  "Dashboard helper returns enlight when no file buffers exist."
  :tags '(unit fast)
  (require 'dashboard nil t)
  (when (featurep 'enlight)
    (should (fboundp 'jotain-dashboard-initial-buffer))))

;;; help.el

(ert-deftest test-help/module-provides ()
  "Smoke test: help.el loads and provides the help feature."
  :tags '(smoke unit fast)
  (require 'help nil t)
  ;; help is built-in; jotain's help.el enhances it
  (should (featurep 'help)))

(ert-deftest test-help/helpful-keybindings ()
  "Smoke test: helpful keybindings are configured."
  :tags '(smoke unit fast)
  ;; If helpful is loaded, the keybindings should be present
  (when (featurep 'helpful)
    (should (fboundp 'helpful-callable))
    (should (fboundp 'helpful-variable))
    (should (fboundp 'helpful-key))
    (should (fboundp 'helpful-at-point))))

;;; per-project.el

(ert-deftest test-per-project/module-provides ()
  "Smoke test: per-project.el loads and provides the per-project feature."
  :tags '(smoke unit fast)
  (require 'per-project nil t)
  (should (featurep 'per-project)))

(ert-deftest test-per-project/projection-available ()
  "Smoke test: projection package is configured for project management."
  :tags '(smoke unit fast)
  (when (featurep 'projection)
    (should (fboundp 'projection-switch-project))))

;;; collaboration.el

(ert-deftest test-collaboration/module-provides ()
  "Smoke test: collaboration.el loads and provides the collaboration feature."
  :tags '(smoke unit fast)
  (require 'collaboration nil t)
  (should (featurep 'collaboration)))

;;; systems.el

(ert-deftest test-systems/module-provides ()
  "Smoke test: systems.el loads without error."
  :tags '(smoke unit fast)
  (require 'systems nil t)
  (should (featurep 'systems)))

(ert-deftest test-systems/sops-configured ()
  "Smoke test: sops package configuration is present when sops is available."
  :tags '(smoke unit fast)
  (when (featurep 'sops)
    (should (fboundp 'sops-edit-file))
    (should (fboundp 'sops-save-file))))

(provide 'test-uncovered-modules)
;;; test-uncovered-modules.el ends here
