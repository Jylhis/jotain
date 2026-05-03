;;; find-file-bench.el --- Per-file-open benchmark harness -*- lexical-binding: t; -*-

;;; Commentary:

;; Measures the cost of opening files after the full Jotain config is loaded.
;; Instruments `find-file', hook variables, and known-expensive global minor
;; mode functions to produce a per-file and per-hook timing breakdown.
;;
;; Invoked by bench/init.el when JOTAIN_BENCH_OPEN_OUTPUT is set.

;;; Code:

(require 'cl-lib)
(require 'seq)

;; Defined in bench/early-init.el; declare to silence byte-compiler.
(defvar jotain-bench--real-dir)

;;;; State

(defvar jotain-bench-open--per-file nil
  "List of (FILE-NAME MODE TOTAL-SECS MINOR-MODES HOOK-TIMINGS).")

(defvar jotain-bench-open--hook-accum nil
  "Alist of (FUNC-NAME . (CALLS TOTAL-SECS)).")

(defvar jotain-bench-open--current-hooks nil
  "Per-file hook timings being collected for the current `find-file'.")

;;;; Hook instrumentation

(defun jotain-bench-open--record (name elapsed)
  "Record ELAPSED seconds for hook/function NAME."
  (push (cons name elapsed) jotain-bench-open--current-hooks)
  (let ((entry (assoc name jotain-bench-open--hook-accum)))
    (if entry
        (setcdr entry (list (1+ (car (cdr entry)))
                            (+ (cadr (cdr entry)) elapsed)))
      (push (list name 1 elapsed) jotain-bench-open--hook-accum))))

(defun jotain-bench-open--wrap-hook-fn (fn)
  "Return advice that times calls to FN and records the result."
  (let ((fname (if (symbolp fn) (symbol-name fn)
                 (format "%s" fn))))
    (lambda (orig-fn &rest args)
      (let ((start (current-time)))
        (prog1 (apply orig-fn args)
          (jotain-bench-open--record
           fname
           (float-time (time-subtract (current-time) start))))))))

(defvar jotain-bench-open--advised nil
  "Functions we have advised (for cleanup).")

(defun jotain-bench-open--instrument-hook-var (hook-var)
  "Add timing advice to every function on HOOK-VAR."
  (when (boundp hook-var)
    (let ((fns (symbol-value hook-var)))
      (when (and fns (not (functionp fns)))
        ;; It's a list of functions
        (dolist (fn fns)
          (when (and (symbolp fn) (fboundp fn)
                     (not (memq fn jotain-bench-open--advised)))
            (let ((wrapper (jotain-bench-open--wrap-hook-fn fn)))
              (advice-add fn :around wrapper
                          '((name . jotain-bench-open-timing)))
              (push fn jotain-bench-open--advised))))))))

(defun jotain-bench-open--instrument-function (fn)
  "Add timing advice to a single function FN."
  (when (and (symbolp fn) (fboundp fn)
             (not (memq fn jotain-bench-open--advised)))
    (let ((wrapper (jotain-bench-open--wrap-hook-fn fn)))
      (advice-add fn :around wrapper
                  '((name . jotain-bench-open-timing)))
      (push fn jotain-bench-open--advised))))

(defun jotain-bench-open--cleanup ()
  "Remove all timing advice."
  (dolist (fn jotain-bench-open--advised)
    (advice-remove fn 'jotain-bench-open-timing))
  (setq jotain-bench-open--advised nil))

;;;; Test file list

(defun jotain-bench-open--test-files ()
  "Return list of (FILE . DESCRIPTION) pairs to benchmark."
  (let ((dir jotain-bench--real-dir))
    (cl-remove-if-not
     (lambda (entry) (file-exists-p (car entry)))
     (list
      (cons (expand-file-name "init.el" dir)
            "Elisp (init.el)")
      (cons (expand-file-name "flake.nix" dir)
            "Nix (flake.nix)")
      (cons (expand-file-name "lisp/init-prog.el" dir)
            "Elisp (init-prog.el)")
      (cons (expand-file-name "Justfile" dir)
            "Build file (Justfile)")
      (cons (expand-file-name "docs/jotain.texi" dir)
            "Texinfo (jotain.texi)")
      (cons (expand-file-name "devenv.nix" dir)
            "Nix (devenv.nix)")))))

;;;; Known-expensive functions to instrument directly

(defvar jotain-bench-open--target-functions
  '(;; envrc
    envrc--update envrc-mode
    ;; zoxide
    zoxide-add
    ;; sops
    sops--find-file-hook sops-mode
    ;; diff-hl
    diff-hl-update diff-hl-mode turn-on-diff-hl-mode
    ;; dtrt-indent
    dtrt-indent-mode dtrt-indent--try-set-once
    ;; treesit-auto
    treesit-auto--set-major-mode treesit-auto-mode
    ;; editorconfig
    editorconfig-mode editorconfig-find-and-apply
    ;; flymake
    flymake-mode flymake--init-flymake
    ;; apheleia
    apheleia-mode
    ;; breadcrumb
    breadcrumb-local-mode
    ;; indent-bars
    indent-bars-mode
    ;; hl-todo
    hl-todo-mode
    ;; display-line-numbers
    display-line-numbers-mode
    ;; hl-line
    hl-line-mode
    ;; display-fill-column-indicator
    display-fill-column-indicator-mode
    ;; subword
    subword-mode
    ;; global-sops
    global-sops-mode
    ;; jinx (text-mode)
    jinx-mode jinx--load-module
    ;; visual-line / variable-pitch (text-mode)
    visual-line-mode variable-pitch-mode visual-wrap-prefix-mode
    ;; save-place
    save-place-find-file-hook
    ;; auto-revert
    auto-revert--global-adopt-current-buffer
    ;; recentf
    recentf-track-opened-file
    ;; Core find-file internals (where "hidden" time lives)
    normal-mode set-auto-mode hack-local-variables
    ;; Font-lock / fontification
    font-lock-mode font-lock-default-fontify-buffer
    font-lock-ensure font-lock-fontify-region
    ;; Tree-sitter
    treesit-font-lock-fontify-region treesit-major-mode-setup
    ;; vc (often the biggest single cost)
    vc-refresh-state vc-backend vc-git-registered
    ;; Native compilation triggers
    native-compile-async
    ;; set-auto-mode internals — what makes mode selection slow?
    magic-fallback-mode-alist-entry
    set-auto-mode-0
    ;; treesit-auto intercepts mode selection
    treesit-auto--set-major-mode
    treesit-auto--maybe-remap treesit-auto--set-major-mode-remap
    ;; Actual major-mode functions
    emacs-lisp-mode nix-ts-mode nix-mode just-mode texinfo-mode
    ;; Package activation / autoload resolution
    package-activate-all
    ;; Async native comp (may fire during mode setup)
    comp--native-compile)
  "Functions to instrument with timing advice.")

;;;; Main benchmark runner

(defun jotain-bench-open--run ()
  "Open test files, collect timings, write report, then exit."
  ;; Instrument hook variables
  (dolist (hook '(find-file-hook
                  after-change-major-mode-hook
                  prog-mode-hook
                  text-mode-hook
                  conf-mode-hook
                  nix-mode-hook
                  nix-ts-mode-hook
                  emacs-lisp-mode-hook))
    (jotain-bench-open--instrument-hook-var hook))

  ;; Instrument known-expensive functions
  (dolist (fn jotain-bench-open--target-functions)
    (jotain-bench-open--instrument-function fn))

  ;; Open each test file and measure.
  (let ((files (jotain-bench-open--test-files)))
    (dolist (entry files)
      (let* ((file (car entry))
             (desc (cdr entry))
             (jotain-bench-open--current-hooks nil)
             (start (current-time))
             (buf (find-file-noselect file)))
        (with-current-buffer buf
          (let ((elapsed (float-time (time-subtract (current-time) start)))
                (mode major-mode)
                (minors (cl-remove-if-not
                         (lambda (m) (and (boundp m) (symbol-value m)))
                         minor-mode-list)))
            (push (list (file-name-nondirectory file)
                        desc
                        mode
                        elapsed
                        (length minors)
                        (copy-sequence jotain-bench-open--current-hooks))
                  jotain-bench-open--per-file)))
        (kill-buffer buf))))

  ;; Reverse to get insertion order
  (setq jotain-bench-open--per-file (nreverse jotain-bench-open--per-file))

  ;; Write report
  (jotain-bench-open--write-report)

  ;; Cleanup
  (jotain-bench-open--cleanup))

;;;; Report writer

(defun jotain-bench-open--write-report ()
  "Write the benchmark report to JOTAIN_BENCH_OPEN_OUTPUT."
  (let* ((outfile (getenv "JOTAIN_BENCH_OPEN_OUTPUT"))
         (results jotain-bench-open--per-file)
         (total-time (apply #'+ (mapcar (lambda (r) (nth 3 r)) results)))
         (file-count (length results))
         (avg-time (if (> file-count 0) (/ total-time file-count) 0))
         ;; Sort hook accum by total time descending
         (sorted-hooks (sort (copy-sequence jotain-bench-open--hook-accum)
                             (lambda (a b) (> (nth 2 a) (nth 2 b))))))
    (when outfile
      (with-temp-file outfile
        ;; Summary
        (insert
         (format "Jotain File-Open Benchmark\n")
         (format "══════════════════════════════════════════════════════\n\n")
         (format "SUMMARY\n")
         (format "──────────────────────────────────────────────────────\n")
         (format "  Files opened           %8d\n" file-count)
         (format "  Total find-file time   %8.3f s\n" total-time)
         (format "  Avg per file           %8.3f s\n\n" avg-time))

        ;; Per-file timing
        (insert
         (format "PER-FILE TIMING\n")
         (format "──────────────────────────────────────────────────────\n")
         (format "  %-22s %-24s %8s  %s\n" "File" "Mode" "Time(s)" "Minors")
         (format "  %-22s %-24s %8s  %s\n"
                 (make-string 22 ?─) (make-string 24 ?─) "────────" "──────"))
        (dolist (r results)
          (insert (format "  %-22s %-24s %8.3f  %d\n"
                          (nth 0 r)
                          (symbol-name (nth 2 r))
                          (nth 3 r)
                          (nth 4 r))))

        ;; Per-file hook breakdown
        (insert (format "\nPER-FILE HOOK BREAKDOWN\n")
                (format "══════════════════════════════════════════════════════\n")
                (format "  (Note: nested calls double-count, e.g. normal-mode\n")
                (format "  includes set-auto-mode. Use to identify hot paths.)\n"))
        (dolist (r results)
          (let* ((hooks (nth 5 r))
                 (sorted (sort (copy-sequence hooks)
                               (lambda (a b) (> (cdr a) (cdr b)))))
                 (hook-sum (apply #'+ (mapcar #'cdr hooks)))
                 (file-total (nth 3 r))
                 (unaccounted (max 0 (- file-total hook-sum))))
            (insert (format "\n  %s (%s) — total: %.3fs\n"
                            (nth 0 r) (symbol-name (nth 2 r)) file-total)
                    (format "  %-40s %8s\n" "Hook/Function" "Time(s)")
                    (format "  %-40s %8s\n" (make-string 40 ?─) "────────"))
            (dolist (h sorted)
              (when (> (cdr h) 0.001)
                (insert (format "  %-40s %8.3f\n" (car h) (cdr h)))))
            (insert (format "  %-40s %8.3f\n" "(unaccounted)" unaccounted))))

        ;; Cumulative hook ranking
        (insert (format "\nCUMULATIVE HOOK RANKING (across all files)\n")
                (format "──────────────────────────────────────────────────────\n")
                (format "  %-40s %5s  %8s  %8s\n" "Hook/Function" "Calls" "Total(s)" "Avg(ms)")
                (format "  %-40s %5s  %8s  %8s\n"
                        (make-string 40 ?─) "─────" "────────" "────────"))
        (dolist (h sorted-hooks)
          (let ((name (nth 0 h))
                (calls (nth 1 h))
                (total (nth 2 h)))
            (when (> total 0.001)
              (insert (format "  %-40s %5d  %8.3f  %8.1f\n"
                              name calls total
                              (* 1000.0 (/ total calls)))))))

        ;; Configuration
        (insert (format "\nCONFIGURATION\n")
                (format "──────────────────────────────────────────────────────\n")
                (format "  native-comp available       %s\n"
                        (if (and (fboundp 'native-comp-available-p)
                                 (native-comp-available-p))
                            "yes" "no"))
                (format "  treesit available            %s\n"
                        (if (featurep 'treesit) "yes" "no"))
                (format "  envrc-global-mode            %s\n"
                        (if (and (boundp 'envrc-global-mode)
                                 envrc-global-mode)
                            "yes" "no"))
                (format "  global-sops-mode             %s\n"
                        (if (and (boundp 'global-sops-mode)
                                 global-sops-mode)
                            "yes" "no")))))))

(provide 'find-file-bench)
;;; find-file-bench.el ends here
