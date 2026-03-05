;;; test-ai.el --- Tests for AI and new package loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke tests verifying all new packages from 003-bleeding-edge-ide load correctly.
;; These tests require packages to be present (provided by Nix build).

;;; Code:

(require 'ert)
(require 'test-helpers)

;;; AI packages (ai.el)

(ert-deftest test-ai/gptel-loads ()
  "Test that gptel package loads successfully."
  :tags '(smoke)
  (should (require 'gptel nil t)))

(ert-deftest test-ai/minuet-loads ()
  "Test that minuet package loads successfully."
  :tags '(smoke)
  (should (require 'minuet nil t)))

(ert-deftest test-ai/mcp-loads ()
  "Test that mcp package loads successfully."
  :tags '(smoke)
  (should (require 'mcp nil t)))

;;; Programming packages (programming.el)

(ert-deftest test-ai/envrc-loads ()
  "Test that envrc package loads successfully."
  :tags '(smoke)
  (should (require 'envrc nil t)))

(ert-deftest test-ai/inheritenv-loads ()
  "Test that inheritenv package loads successfully."
  :tags '(smoke)
  (should (require 'inheritenv nil t)))

(ert-deftest test-ai/apheleia-loads ()
  "Test that apheleia package loads successfully."
  :tags '(smoke)
  (should (require 'apheleia nil t)))

(ert-deftest test-ai/eglot-booster-loads ()
  "Test that eglot-booster package loads successfully."
  :tags '(smoke)
  (should (require 'eglot-booster nil t)))

(ert-deftest test-ai/combobulate-loads ()
  "Test that combobulate package loads successfully."
  :tags '(smoke)
  (should (require 'combobulate nil t)))

(ert-deftest test-ai/claude-code-ide-loads ()
  "Test that claude-code-ide package loads successfully."
  :tags '(smoke)
  (should (require 'claude-code-ide nil t)))

;;; UI packages (ui.el)

(ert-deftest test-ai/doom-modeline-loads ()
  "Test that doom-modeline package loads successfully."
  :tags '(smoke)
  (should (require 'doom-modeline nil t)))

(ert-deftest test-ai/indent-bars-loads ()
  "Test that indent-bars package loads successfully."
  :tags '(smoke)
  (should (require 'indent-bars nil t)))

(ert-deftest test-ai/pulsar-loads ()
  "Test that pulsar package loads successfully."
  :tags '(smoke)
  (should (require 'pulsar nil t)))

;;; Git packages (git.el)

(ert-deftest test-ai/forge-loads ()
  "Test that forge package loads successfully."
  :tags '(smoke)
  (should (require 'forge nil t)))

(ert-deftest test-ai/git-timemachine-loads ()
  "Test that git-timemachine package loads successfully."
  :tags '(smoke)
  (should (require 'git-timemachine nil t)))

(provide 'test-ai)
;;; test-ai.el ends here
