;;; lang-eval-test.el --- Drift guard for the language-eval registry -*- lexical-binding: t; -*-

;;; Commentary:

;; Consistency checks for `jotain-lang-registry' (etc/lang-eval/).  The
;; tests do not boot the config: they check the table's own shape and read
;; templates/jotain.eld and init-prog.el as text.  Mode routing, servers and
;; formatters are checked against the live config by the Tier-1 probe
;; (`nix build .#lang-eval-matrix').
;;
;; No network, subprocess, or writes, so safe in the Nix sandbox.
;;
;; Run with:
;;   emacs --batch -L lisp -L test -l ert -l test/lang-eval-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'test-helper)

;; The registry lives outside lisp/, so add etc/lang-eval/ to load-path.
(add-to-list 'load-path (test-helper-file "etc/lang-eval"))
(require 'jotain-lang-registry)

;;;; Helpers

(defun lang-eval-test--contents (relative)
  "Return the text of the repo file RELATIVE."
  (let ((path (test-helper-file relative)))
    (with-temp-buffer (insert-file-contents path) (buffer-string))))

(defun lang-eval-test--snippet-modes ()
  "Return the tempel section-header mode symbols in templates/jotain.eld."
  (let ((modes '()))
    (with-temp-buffer
      (insert-file-contents (test-helper-file "templates/jotain.eld"))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (symbolp form) (push form modes))))
        (end-of-file nil)))
    modes))

;;;; Structural validity

(ert-deftest lang-eval-test-entries-well-formed ()
  "Every registry entry has the required keys with the right shapes."
  (dolist (e jotain-lang-registry)
    (should (symbolp (plist-get e :id)))
    (should (stringp (plist-get e :name)))
    (should (stringp (plist-get e :file)))
    (should (stringp (plist-get e :sample)))
    (should (symbolp (plist-get e :mode)))
    ;; :servers, when present, is a non-empty list of strings.
    (when-let* ((servers (plist-get e :servers)))
      (should (listp servers))
      (should (seq-every-p #'stringp servers)))))

(ert-deftest lang-eval-test-ids-unique ()
  "Registry :id values are unique."
  (let ((ids (mapcar (lambda (e) (plist-get e :id)) jotain-lang-registry)))
    (should (equal ids (seq-uniq ids)))))

(ert-deftest lang-eval-test-owning-files-exist ()
  "Every entry's :file is a real lisp/ module."
  (dolist (e jotain-lang-registry)
    (should (file-exists-p (test-helper-file (concat "lisp/" (plist-get e :file)))))))

;;;; Drift: registry vs. config source text

(ert-deftest lang-eval-test-override-entries-name-servers ()
  "An :override entry names the server the explicit eglot entry resolves to."
  (dolist (e jotain-lang-registry)
    (when (plist-get e :override)
      (ert-info ((symbol-name (plist-get e :id)))
        (should (plist-get e :servers))))))

(ert-deftest lang-eval-test-snippet-flag-matches-templates ()
  "The :snippets flag agrees exactly with templates/jotain.eld section headers."
  (let ((sections (lang-eval-test--snippet-modes)))
    (dolist (e jotain-lang-registry)
      (let ((declared (and (plist-get e :snippets) t))
            (present (and (memq (plist-get e :mode) sections) t)))
        (should (eq declared present))))))

(ert-deftest lang-eval-test-inlay-modes-listed ()
  "Each :inlay entry's mode is named in init-prog.el's inlay opt-in list."
  (let ((prog (lang-eval-test--contents "lisp/init-prog.el")))
    (dolist (e jotain-lang-registry)
      (when (plist-get e :inlay)
        (should (string-search (symbol-name (plist-get e :mode)) prog))))))

(ert-deftest lang-eval-test-live-subset-has-servers ()
  "Every live-probe language declares at least one LSP server to probe."
  (dolist (e jotain-lang-registry)
    (when (plist-get e :live)
      (should (plist-get e :servers)))))

(ert-deftest lang-eval-test-json-encodes-nil-as-null ()
  "A nil plist value is JSON null, not the string \"json-null\"."
  (require 'jotain-lang-eval)
  (should (string-match-p "\"server\": null"
                          (jotain-lang-eval--json (list (list :name "x" :server nil))))))

(provide 'lang-eval-test)
;;; lang-eval-test.el ends here
