;;; init.el --- Startup benchmark wrapper -*- lexical-binding: t; -*-

;;; Commentary:

;; Wrapper init that loads the real init.el and installs an
;; `emacs-startup-hook' that writes a timing report to the file named by
;; the JOTAIN_BENCH_OUTPUT environment variable, then exits.
;;
;; Relies on `jotain-bench--real-dir' and `jotain-bench--results' set up
;; by the sibling bench/early-init.el.

;;; Code:

(load (expand-file-name "init.el" jotain-bench--real-dir) nil t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (cond
             ;; File-open benchmark mode
             ((getenv "JOTAIN_BENCH_OPEN_OUTPUT")
              (load (expand-file-name "bench/find-file-bench.el"
                                      jotain-bench--real-dir) nil t)
              (jotain-bench-open--run)
              (kill-emacs 0))

             ;; Startup benchmark mode (original)
             ((getenv "JOTAIN_BENCH_OUTPUT")
              (let* ((init-time (float-time (time-subtract after-init-time before-init-time)))
                     (total-time (float-time (time-subtract (current-time) before-init-time)))
                     (gc-pct (if (> init-time 0) (* 100.0 (/ gc-elapsed init-time)) 0.0))
                     (pkg-count (if (bound-and-true-p package-activated-list)
                                    (length package-activated-list) 0))
                     (outfile (getenv "JOTAIN_BENCH_OUTPUT"))
                     (sorted (sort (copy-sequence jotain-bench--results)
                                   (lambda (a b) (> (cdr a) (cdr b)))))
                     (init-modules (seq-filter (lambda (x) (string-prefix-p "init-" (car x))) sorted))
                     (network-ops (seq-filter (lambda (x) (string-prefix-p "NETWORK:" (car x))) sorted))
                     (other-slow (seq-filter (lambda (x)
                                               (and (not (string-prefix-p "init-" (car x)))
                                                    (not (string-prefix-p "NETWORK:" (car x)))
                                                    (> (cdr x) 0.010)))
                                             sorted))
                     (sum-init (apply #'+ (or (mapcar #'cdr init-modules) '(0))))
                     (sum-network (apply #'+ (or (mapcar #'cdr network-ops) '(0))))
                     (sum-other (apply #'+ (or (mapcar #'cdr other-slow) '(0)))))
                (when outfile
                  (with-temp-file outfile
                    (insert
                     (format "Jotain Startup Benchmark\n")
                     (format "══════════════════════════════════════════════════════\n\n")
                     (format "SUMMARY\n")
                     (format "──────────────────────────────────────────────────────\n")
                     (format "  Init time              %8.3f s\n" init-time)
                     (format "  Total startup time     %8.3f s\n" total-time)
                     (format "  GC count               %8d\n" gcs-done)
                     (format "  GC time                %8.3f s  (%.1f%%)\n" gc-elapsed gc-pct)
                     (format "  Features loaded        %8d\n" (length features))
                     (format "  Packages activated     %8d\n\n" pkg-count)
                     (format "TIME BUDGET\n")
                     (format "──────────────────────────────────────────────────────\n")
                     (format "  init-* modules         %8.3f s  (%5.1f%%)\n" sum-init (* 100.0 (/ sum-init init-time)))
                     (format "  Network (refresh)      %8.3f s  (%5.1f%%)\n" sum-network (* 100.0 (/ sum-network init-time)))
                     (format "  Other slow loads       %8.3f s  (%5.1f%%)\n\n" sum-other (* 100.0 (/ sum-other init-time))))
                    (when network-ops
                      (insert (format "NETWORK OPERATIONS\n")
                              (format "──────────────────────────────────────────────────────\n"))
                      (dolist (e network-ops)
                        (insert (format "  %-40s %8.3f s\n" (car e) (cdr e))))
                      (insert "\n"))
                    (insert (format "PER-MODULE TIMING (init-* modules)\n")
                            (format "──────────────────────────────────────────────────────\n")
                            (format "  %-40s %8s  %5s\n" "Module" "Time(s)" "%init")
                            (format "  %-40s %8s  %5s\n" (make-string 40 ?─) "────────" "─────"))
                    (dolist (e init-modules)
                      (insert (format "  %-40s %8.3f  %5.1f%%\n"
                                      (car e) (cdr e) (* 100.0 (/ (cdr e) init-time)))))
                    (insert (format "\nOTHER SLOW FEATURES (>10ms)\n")
                            (format "──────────────────────────────────────────────────────\n")
                            (format "  %-40s %8s  %5s\n" "Feature" "Time(s)" "%init")
                            (format "  %-40s %8s  %5s\n" (make-string 40 ?─) "────────" "─────"))
                    (dolist (e other-slow)
                      (insert (format "  %-40s %8.3f  %5.1f%%\n"
                                      (car e) (cdr e) (* 100.0 (/ (cdr e) init-time)))))
                    (insert (format "\nCONFIGURATION\n")
                            (format "──────────────────────────────────────────────────────\n")
                            (format "  package-quickstart          %s\n"
                                    (if (bound-and-true-p package-quickstart) "yes" "no"))
                            (format "  native-comp available       %s\n"
                                    (if (and (fboundp 'native-comp-available-p) (native-comp-available-p)) "yes" "no")))))
                (kill-emacs 0))))))

(provide 'init)
;;; init.el ends here
