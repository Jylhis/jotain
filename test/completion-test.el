;;; completion-test.el --- Tests for the in-buffer completion wiring -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch-safe tests for the capf layer that `lisp/init-completion.el' and
;; `lisp/init-snippets.el' build on.  No network, no subprocesses, no writes
;; outside a temp buffer -- safe inside the Nix sandbox.
;;
;; The first section pins down two behaviours of the core capf protocol that
;; the design in `docs/design/completion.md' depends on and that could not be
;; settled from documentation:
;;
;;   1. An exclusive capf (one that declares no `:exclusive' at all, which is
;;      how eglot's capf is written) suppresses every capf after it, and
;;      whether `cape-capf-nonexclusive' is the documented escape.
;;
;;   2. Whether the `:exclusive no' fallthrough honours `completion-styles'.
;;      `completion--capf-wrapper' decides it with a bare `try-completion',
;;      and its own FIXME warns that "non-prefix completion will not work (or
;;      not right) for completion functions that are non-exclusive".
;;
;; Run with:
;;   emacs --batch -L lisp -L test -l ert -l test/completion-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cape)
(require 'orderless)

;;;; Helpers

;; Three capfs covering the shapes that matter.  Each reports bounds spanning
;; the text already in the buffer, so the wrapper's prefix test sees a real
;; prefix rather than the empty string.

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
;; `cape-capf-super' in LSP buffers.  Whether that merged capf is exclusive
;; decides two things: whether the cape fallbacks after it ever run, and
;; whether the prefix-only fallthrough of section 2 can discard the snippet
;; names it carries.

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

;; Loading the two modules executes their `:init'/`:custom'/`:bind' side
;; effects in this batch process, which is how the assertions below observe
;; real state.  `after-init-hook' never runs in batch, so `global-corfu-mode'
;; stays off and no popup can appear.

(require 'corfu)
(require 'tempel)
(require 'init-completion)
(require 'init-snippets)
;; Loaded after `init-completion' so the `with-eval-after-load' body its
;; `completion-preview' block registers (the R2 keymap edits) fires on this
;; require, letting the assertions below observe the real map state.
(require 'completion-preview)

(ert-deftest completion-test-defcustom-defaults ()
  "The opt-out knobs carry their documented defaults."
  (should (equal jotain-completion-auto-modes '(prog-mode-hook)))
  (should (equal jotain-completion-auto-delay 0.2))
  (should (equal jotain-completion-auto-prefix 3))
  (should (equal jotain-completion-key "C-M-i"))
  (should (eq jotain-completion-free-return t))
  (should (eq jotain-completion-free-tab t))
  (should (eq jotain-completion-fallbacks t))
  (should (eq jotain-completion-snippets t))
  (should (eq jotain-completion-eglot-nonexclusive t))
  (should (eq jotain-completion-doc-popup t))
  (should (eq jotain-completion-inline-preview t))
  (should (key-valid-p jotain-completion-key)))

(ert-deftest completion-test-corfu-preview-current-off ()
  "The popup inserts nothing until asked: `corfu-preview-current' is nil.
corfu's own default is `insert', which commits the selected candidate on
further input -- exactly the accidental-accept this config avoids."
  (should (eq corfu-preview-current nil)))

(ert-deftest completion-test-tab-indents-only ()
  "TAB is on the stock default, so `indent-for-tab-command' cannot complete."
  (should (eq tab-always-indent t)))

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

(ert-deftest completion-test-corfu-map-frees-return-and-tab ()
  "RET and TAB are removed from `corfu-map', and navigation still works."
  (should-not (lookup-key corfu-map (kbd "RET")))
  (should-not (lookup-key corfu-map (kbd "TAB")))
  (should-not (lookup-key corfu-map [tab]))
  (should (eq (lookup-key corfu-map (kbd "M-n")) #'corfu-next))
  (should (eq (lookup-key corfu-map (kbd "M-p")) #'corfu-previous)))

(ert-deftest completion-test-inline-preview-tab-is-freed ()
  "The inline preview never lets TAB (or RET) accept a candidate.
`completion-preview-active-mode-map' ships `C-i' -> `completion-preview-insert',
and `C-i' IS the TAB event, so leaving it bound would let TAB accept the
preview -- violating the TAB-indents-only rule.  This config removes it and
puts the whole-candidate accept on `M-RET' instead; RET is never bound by
the mode, so Enter stays a newline."
  (should-not (lookup-key completion-preview-active-mode-map (kbd "C-i")))
  (should-not (lookup-key completion-preview-active-mode-map (kbd "TAB")))
  (should-not (lookup-key completion-preview-active-mode-map [tab]))
  (should-not (lookup-key completion-preview-active-mode-map (kbd "RET")))
  (should-not (lookup-key completion-preview-active-mode-map [return]))
  (should (eq (lookup-key completion-preview-active-mode-map (kbd "M-RET"))
              #'completion-preview-insert)))

(ert-deftest completion-test-inline-preview-is-per-mode-like-auto ()
  "Inline preview rides the same mode list as the auto-popup, so prose stays quiet.
`completion-preview-mode' is enabled in `jotain-completion-auto-modes'
buffers (prog-mode) and nowhere prose lives (text-mode)."
  (should (memq #'completion-preview-mode prog-mode-hook))
  (should-not (memq #'completion-preview-mode text-mode-hook)))

(ert-deftest completion-test-one-key-opens-and-accepts ()
  "`C-M-i' resolves to `completion-at-point', which `corfu-map' remaps.
Binding the command rather than leaving the stock `complete-symbol' is
what makes the same key accept inside the popup -- a remap only fires
for the command the key actually resolves to.  The remap is repointed
from corfu's default `corfu-complete' (extends the common prefix, does
not run a capf `:exit-function') to `corfu-insert' (inserts the selected
candidate with status `finished', so it commits and expands snippets).
This is why no separate accept key is needed."
  (should (eq (keymap-global-lookup jotain-completion-key)
              #'completion-at-point))
  (should (eq (lookup-key corfu-map [remap completion-at-point])
              #'corfu-insert)))

(ert-deftest completion-test-corfu-preselects-first ()
  "The top candidate is always selected, so `C-M-i' has something to insert.
`corfu-insert' quits without inserting when nothing is selected; `first'
guarantees a selection (and the `corfu-current' highlight that shows it).
Safe because RET/TAB are freed -- only the explicit `C-M-i' commits."
  (should (eq corfu-preselect 'first)))

(ert-deftest completion-test-tempel-fields-are-off-tab ()
  "Snippet fields move on tempel's own keys and on `C-M-n'/`C-M-p', never TAB.
No field key may collide with corfu's `M-n'/`M-p': `tempel-map' rides on
an overlay `keymap' property, which outranks corfu's minor-mode map, so
a shared key would be stolen from the popup mid-snippet."
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
