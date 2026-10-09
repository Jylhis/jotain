;;; lang-eval-test.el --- Drift guard for the language-eval registry -*- lexical-binding: t; -*-

;;; Commentary:

;; Drift guard for `jotain-lang-registry' (etc/lang-eval/).  The tests do
;; not boot the config: they read init-prog.el, init-lang-*.el and
;; templates/jotain.eld as text and check the registry agrees with them.
;; Actual mode routing and eglot resolution are checked by the Tier-1
;; probe (`nix build .#lang-eval-matrix').
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

;; The registry lives outside lisp/, so add etc/lang-eval/ to load-path.
(defconst lang-eval-test--root
  (locate-dominating-file
   (or load-file-name buffer-file-name default-directory) "init.el")
  "Repository root, located by walking up to the directory holding init.el.")

(add-to-list 'load-path (expand-file-name "etc/lang-eval" lang-eval-test--root))
(require 'jotain-lang-registry)

;;;; Helpers

(defun lang-eval-test--file (relative)
  "Return the absolute path of RELATIVE within the repo root."
  (expand-file-name relative lang-eval-test--root))

(defun lang-eval-test--contents (relative)
  "Return the text of the repo file RELATIVE."
  (let ((path (lang-eval-test--file relative)))
    (with-temp-buffer (insert-file-contents path) (buffer-string))))

(defun lang-eval-test--snippet-modes ()
  "Return the tempel section-header mode symbols in templates/jotain.eld."
  (let ((modes '()))
    (with-temp-buffer
      (insert-file-contents (lang-eval-test--file "templates/jotain.eld"))
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
    (should (file-exists-p (lang-eval-test--file (concat "lisp/" (plist-get e :file)))))))

;;;; Drift: registry vs. config source text

(ert-deftest lang-eval-test-override-servers-present ()
  "Each :override language's server appears in init-prog.el's server-programs.
These are the servers Jotain sets explicitly (not eglot built-ins), so a
removed override must fail here."
  (let ((prog (lang-eval-test--contents "lisp/init-prog.el")))
    (dolist (e jotain-lang-registry)
      (when (plist-get e :override)
        (let ((servers (plist-get e :servers)))
          (should
           (seq-some (lambda (s) (string-search s prog)) servers)))))))

(ert-deftest lang-eval-test-override-formatters-present ()
  "apheleia formatter overrides declared in the registry appear in init-prog.el.
Only the formatters Jotain adds itself are checked; apheleia built-ins
(ruff/prettier/rustfmt/clang-format) are not in our source."
  (let ((prog (lang-eval-test--contents "lisp/init-prog.el"))
        (ours '("meson" "zig" "goimports" "buildifier" "nixfmt" "qmlformat")))
    (dolist (e jotain-lang-registry)
      (let ((f (plist-get e :formatter)))
        (when (and f (member f ours))
          (should (string-search f prog)))))))

(ert-deftest lang-eval-test-modes-registered ()
  "Each non-skip entry's mode is named in its owning file or init-prog.el.
Catches a language module that stops registering its major mode, and the
tree-sitter routing table in init-prog.el for remapped languages."
  (let ((prog (lang-eval-test--contents "lisp/init-prog.el")))
    (dolist (e jotain-lang-registry)
      (unless (plist-get e :skip-mode)
        (let* ((own (lang-eval-test--contents (concat "lisp/" (plist-get e :file))))
               (mode (symbol-name (plist-get e :mode)))
               (classic (and (plist-get e :classic)
                             (symbol-name (plist-get e :classic)))))
          (should (or (string-search mode own)
                      (string-search mode prog)
                      (and classic (or (string-search classic own)
                                       (string-search classic prog))))))))))

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

(provide 'lang-eval-test)
;;; lang-eval-test.el ends here
