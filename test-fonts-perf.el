(require 'benchmark)

(defvar available-fonts nil)

(defun setup ()
  (setq available-fonts (font-family-list)))

(defun test-member ()
  (seq-find (lambda (font-spec)
              (member (car font-spec) available-fonts))
            '(("NonExistent1" . 1)
              ("NonExistent2" . 1)
              ("NonExistent3" . 1)
              ("DejaVu Sans Mono" . 100))))

(defun test-hash-table ()
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (font available-fonts)
      (puthash font t ht))
    (seq-find (lambda (font-spec)
                (gethash (car font-spec) ht))
              '(("NonExistent1" . 1)
                ("NonExistent2" . 1)
                ("NonExistent3" . 1)
                ("DejaVu Sans Mono" . 100)))))

(setup)

(message "Benchmarking member:")
(benchmark 1000 '(test-member))

(message "Benchmarking hash table (including creation):")
(benchmark 1000 '(test-hash-table))

(message "Done")
