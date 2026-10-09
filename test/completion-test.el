;;; completion-test.el --- Tests for the in-buffer completion wiring -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch-safe tests for the capf layer that `lisp/init-completion.el' and
;; `lisp/init-snippets.el' build on.  No network, subprocesses or writes
;; outside a temp buffer, so safe in the Nix sandbox.
;;
;; Sections 1-3 pin down core capf behaviour that `docs/design/completion.md'
;; depends on and the documentation does not settle:
;;
;;   1. A capf with no `:exclusive' (like eglot's) suppresses every capf
;;      after it, and `cape-capf-nonexclusive' undoes that.
;;
;;   2. The `:exclusive no' fallthrough ignores `completion-styles':
;;      `completion--capf-wrapper' decides with a bare `try-completion' (its
;;      own FIXME says non-prefix completion will not work there).
;;
;;   3. How `cape-capf-super' propagates exclusivity.
;;
;; Section 4 checks this configuration's own wiring.
;;
;; Run with:
;;   emacs --batch -L lisp -L test -l ert -l test/completion-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cape)
(require 'orderless)

;;;; Helpers

;; Each capf's bounds span the text already in the buffer, so the wrapper's
;; prefix test sees a real prefix.

(defun completion-test--capf-exclusive ()
  "Capf shaped like eglot's: no `:exclusive' key at all, so exclusive."
  (list (line-beginning-position) (point) '("alpha" "beta")))

(defun completion-test--capf-nonexclusive ()
  "Capf shaped like tempel's: explicitly `:exclusive no'."
  (list (line-beginning-position) (point) '("alpha" "beta") :exclusive 'no))

(defun completion-test--capf-substring ()
  "Non-exclusive capf whose candidate matches under orderless but not by prefix."
  (list (line-beginning-position) (point) '("barfoo") :exclusive 'no))

(defun completion-test--capf-later ()
  "Capf standing in for the cape fallbacks placed after the semantic one."
  (list (line-beginning-position) (point) '("later-capf-ran")))

(defun completion-test--winning-collection (capfs)
  "Return the candidate list `completion-at-point' would use from CAPFS.
Runs the same `run-hook-wrapped' dispatch that `completion-at-point'
uses, so the `:exclusive' handling under test is the real one."
  (let* ((completion-at-point-functions capfs)
         (res (run-hook-wrapped 'completion-at-point-functions
                                #'completion--capf-wrapper 'all)))
    (and (consp res) (nth 2 (cdr res)))))

;;;; 1. Exclusivity and cape-capf-nonexclusive

(ert-deftest completion-test-exclusive-capf-shadows-later-capfs ()
  "A capf declaring no `:exclusive' wins even when nothing it offers matches.
This is why eglot suppresses the cape fallbacks placed after it."
  (with-temp-buffer
    (insert "zzz")
    (should (equal (completion-test--winning-collection
                    (list #'completion-test--capf-exclusive
                          #'completion-test--capf-later))
                   '("alpha" "beta")))))

(ert-deftest completion-test-nonexclusive-capf-falls-through ()
  "An `:exclusive no' capf yields to the next capf when its prefix misses."
  (with-temp-buffer
    (insert "zzz")
    (should (equal (completion-test--winning-collection
                    (list #'completion-test--capf-nonexclusive
                          #'completion-test--capf-later))
                   '("later-capf-ran")))))

(ert-deftest completion-test-cape-nonexclusive-unshadows ()
  "`cape-capf-nonexclusive' turns an exclusive capf into one that yields.
This is the fix for eglot suppressing capfs ordered after it."
  (with-temp-buffer
    (insert "zzz")
    (should (equal (completion-test--winning-collection
                    (list (cape-capf-nonexclusive
                           #'completion-test--capf-exclusive)
                          #'completion-test--capf-later))
                   '("later-capf-ran")))))

(ert-deftest completion-test-nonexclusive-capf-kept-when-prefix-matches ()
  "The fallthrough is conditional: a matching prefix keeps the first capf."
  (with-temp-buffer
    (insert "al")
    (should (equal (completion-test--winning-collection
                    (list #'completion-test--capf-nonexclusive
                          #'completion-test--capf-later))
                   '("alpha" "beta")))))

;;;; 2. The fallthrough versus completion-styles

(ert-deftest completion-test-fallthrough-ignores-completion-styles ()
  "The `:exclusive no' fallthrough is prefix-only and ignores `completion-styles'.
\"foo\" matches \"barfoo\" under orderless but not by prefix, yet the capf
is discarded under both style settings -- the wrapper decides with a bare
`try-completion' before any style runs.  Consequence for this config: a
non-exclusive capf loses candidates a non-prefix style would have matched."
  (with-temp-buffer
    (insert "foo")
    (dolist (styles '((orderless basic) (basic)))
      (let ((completion-styles styles))
        (should (equal (completion-test--winning-collection
                        (list #'completion-test--capf-substring
                              #'completion-test--capf-later))
                       '("later-capf-ran")))))))

(ert-deftest completion-test-orderless-itself-matches-substring ()
  "Orderless does match \"foo\" against \"barfoo\" -- the style is not at fault.
Pairs with the test above: the candidate is reachable by the style but the
capf carrying it was already discarded by the prefix-only fallthrough."
  (let ((completion-styles '(orderless)))
    (should (member "barfoo" (completion-all-completions
                              "foo" '("barfoo" "unrelated") nil 3)))))

;;;; 3. What the merged capf reports

;; `lisp/init-snippets.el' merges the snippet capf with eglot's via
;; `cape-capf-super'.  Whether the merge is exclusive decides whether the
;; cape fallbacks after it run, and whether section 2's prefix-only
;; fallthrough can discard its snippet names.

(ert-deftest completion-test-super-merges-both-collections ()
  "`cape-capf-super' yields one collection holding both sources' candidates."
  (with-temp-buffer
    (insert "al")
    (let ((collection (completion-test--winning-collection
                       (list (cape-capf-super
                              #'completion-test--capf-nonexclusive
                              #'completion-test--capf-later)))))
      (should collection)
      (let ((all (all-completions "" collection)))
        (should (member "alpha" all))
        (should (member "later-capf-ran" all))))))

(ert-deftest completion-test-super-propagates-exclusivity ()
  "`cape-capf-super' is non-exclusive only when EVERY input is non-exclusive.
One exclusive input makes the merge exclusive.  This is the config's
actual shape: `init-snippets.el' merges the snippet capf (`:exclusive
no') with eglot's (no `:exclusive' key, hence exclusive), so the merged
capf is exclusive and suppresses the cape fallbacks after it."
  (with-temp-buffer
    (insert "zzz")
    (should (eq 'no (plist-get
                     (nthcdr 3 (funcall (cape-capf-super
                                         #'completion-test--capf-nonexclusive
                                         #'completion-test--capf-substring)))
                     :exclusive)))
    (should-not (plist-get
                 (nthcdr 3 (funcall (cape-capf-super
                                     #'completion-test--capf-nonexclusive
                                     #'completion-test--capf-exclusive)))
                 :exclusive))))

(ert-deftest completion-test-super-with-exclusive-input-shadows ()
  "A merge containing an exclusive input suppresses later capfs...
...and wrapping it in `cape-capf-nonexclusive' lets them run again.
This is why `init-snippets.el' wraps its eglot merge."
  (with-temp-buffer
    (insert "zzz")
    (let ((merged (cape-capf-super #'completion-test--capf-nonexclusive
                                   #'completion-test--capf-exclusive)))
      (should-not (equal (completion-test--winning-collection
                          (list merged #'completion-test--capf-later))
                         '("later-capf-ran")))
      (should (equal (completion-test--winning-collection
                      (list (cape-capf-nonexclusive merged)
                            #'completion-test--capf-later))
                     '("later-capf-ran"))))))

;;;; 4. This configuration

;; Loading the two modules runs their `:init'/`:custom'/`:bind' side
;; effects here, so the assertions observe real state.  `after-init-hook'
;; never runs in batch, so `global-corfu-mode' stays off.

(require 'corfu)
(require 'tempel)
(require 'init-completion)
(require 'init-snippets)
;; Already loaded by `init-completion's `:init', which has therefore run
;; its keymap edits; required here to make the dependency explicit.
(require 'completion-preview)

(ert-deftest completion-test-defcustom-defaults ()
  "The opt-out knobs carry their documented defaults."
  (should (equal jotain-completion-auto-modes '(prog-mode-hook)))
  (should (equal jotain-completion-auto-delay 0.2))
  (should (equal jotain-completion-auto-prefix 3))
  (should (equal jotain-completion-key "C-M-i"))
  (should (eq jotain-completion-free-return t))
  (should (eq jotain-completion-free-tab nil))
  (should (eq jotain-completion-fallbacks t))
  (should (eq jotain-completion-snippets t))
  (should (eq jotain-completion-eglot-nonexclusive t))
  (should (eq jotain-completion-doc-popup t))
  (should (eq jotain-completion-inline-preview t))
  (should (key-valid-p jotain-completion-key)))

(ert-deftest completion-test-corfu-preview-current-off ()
  "The popup inserts nothing until asked: `corfu-preview-current' is nil.
corfu's default `insert' commits the selected candidate on further input."
  (should (eq corfu-preview-current nil)))

(ert-deftest completion-test-tab-indents-and-completes ()
  "TAB indents and completes: `tab-always-indent' is `complete'.
That is the default, with `jotain-completion-free-tab' nil."
  (should (eq jotain-completion-free-tab nil))
  (should (eq tab-always-indent 'complete)))

(ert-deftest completion-test-auto-popup-is-opt-in-per-mode ()
  "Auto-popup is off globally and opted into per mode hook, buffer-locally."
  (should (eq (default-value 'corfu-auto) nil))
  (should (memq #'jotain-completion--enable-auto prog-mode-hook))
  (should-not (memq #'jotain-completion--enable-auto text-mode-hook))
  (with-temp-buffer
    (jotain-completion--enable-auto)
    (should (local-variable-p 'corfu-auto))
    (should (eq corfu-auto t)))
  ;; The opt-in never touches the global value.
  (should (eq (default-value 'corfu-auto) nil)))

(ert-deftest completion-test-corfu-map-frees-return-and-accepts-on-tab ()
  "RET is removed from `corfu-map'; TAB accepts the selected candidate.
TAB and `<tab>' run `corfu-insert', which also runs the candidate's
`:exit-function' (expanding snippets).  Navigation still works."
  (should-not (lookup-key corfu-map (kbd "RET")))
  (should (eq (lookup-key corfu-map (kbd "TAB")) #'corfu-insert))
  (should (eq (lookup-key corfu-map [tab]) #'corfu-insert))
  (should (eq (lookup-key corfu-map (kbd "M-n")) #'corfu-next))
  (should (eq (lookup-key corfu-map (kbd "M-p")) #'corfu-previous)))

(ert-deftest completion-test-inline-preview-tab-accepts-ghost-text ()
  "TAB accepts the inline preview; RET is never bound, so Enter stays a newline.
`C-i' is the TAB event; `M-RET' also accepts."
  (should (eq (lookup-key completion-preview-active-mode-map (kbd "C-i"))
              #'completion-preview-insert))
  (should-not (lookup-key completion-preview-active-mode-map (kbd "RET")))
  (should-not (lookup-key completion-preview-active-mode-map [return]))
  (should (eq (lookup-key completion-preview-active-mode-map (kbd "M-RET"))
              #'completion-preview-insert)))

(ert-deftest completion-test-inline-preview-is-global ()
  "Inline preview is enabled globally where Emacs supports it.
Emacs 31 has `global-completion-preview-mode'; on Emacs 30 the config
falls back to the `jotain-completion-auto-modes' hooks."
  (if (fboundp 'global-completion-preview-mode)
      (should (bound-and-true-p global-completion-preview-mode))
    (should (memq #'completion-preview-mode prog-mode-hook))))

(ert-deftest completion-test-one-key-opens-and-accepts ()
  "`C-M-i' resolves to `completion-at-point', which `corfu-map' remaps.
A remap only fires for the command a key resolves to, so binding the
command is what lets the same key accept inside the popup.  The remap
points at `corfu-insert' rather than corfu's `corfu-complete', which only
extends the common prefix and skips the `:exit-function'."
  (should (eq (keymap-global-lookup jotain-completion-key)
              #'completion-at-point))
  (should (eq (lookup-key corfu-map [remap completion-at-point])
              #'corfu-insert)))

(ert-deftest completion-test-corfu-preselects-first ()
  "The top candidate is always selected, so the accept key has something.
`corfu-insert' quits without inserting when nothing is selected."
  (should (eq corfu-preselect 'first)))

(ert-deftest completion-test-tempel-fields-are-off-tab ()
  "Snippet fields move on tempel's own keys and on `C-M-n'/`C-M-p', never TAB.
No field key may collide with corfu's `M-n'/`M-p': `tempel-map' is an
overlay `keymap' property, which outranks corfu's map, so it would steal
the key from the popup mid-snippet."
  (should-not (lookup-key tempel-map (kbd "TAB")))
  (should-not (lookup-key tempel-map [tab]))
  (should-not (lookup-key tempel-map (kbd "<backtab>")))
  (should (eq (lookup-key tempel-map (kbd "C-M-n")) #'tempel-next))
  (should (eq (lookup-key tempel-map (kbd "C-M-p")) #'tempel-previous))
  (should (eq (lookup-key tempel-map (kbd "M-}")) #'tempel-next))
  (should (eq (lookup-key tempel-map (kbd "M-{")) #'tempel-previous))
  (should-not (lookup-key tempel-map (kbd "M-n")))
  (should-not (lookup-key tempel-map (kbd "M-p"))))

(ert-deftest completion-test-capf-wiring ()
  "Snippets lead the buffer-local list; the cape fallbacks are global."
  (should (memq #'jotain-tempel-setup-capf prog-mode-hook))
  (should (memq #'jotain-tempel-setup-capf text-mode-hook))
  (with-temp-buffer
    (jotain-tempel-setup-capf)
    (should (eq (car completion-at-point-functions) #'tempel-complete))
    ;; The `t' sentinel is what lets the global list run afterwards.
    (should (eq (car (last completion-at-point-functions)) t)))
  (let ((global (default-value 'completion-at-point-functions)))
    (should (memq #'cape-dabbrev global))
    (should (memq #'cape-file global))
    (should (memq #'cape-keyword global))))

(provide 'completion-test)
;;; completion-test.el ends here
