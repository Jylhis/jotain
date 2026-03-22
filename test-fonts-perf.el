(require 'benchmark)

(defun test-list-lookup ()
  (let ((available-fonts (font-family-list))
        (search-font "Arial"))
    (benchmark-run 1000
      (member search-font available-fonts))))

(defun test-hash-lookup ()
  (let ((available-fonts (make-hash-table :test 'equal))
        (search-font "Arial"))
    (dolist (f (font-family-list))
      (puthash f t available-fonts))
    (benchmark-run 1000
      (gethash search-font available-fonts))))

(message "List lookup (1000x): %S" (test-list-lookup))
(message "Hash lookup (1000x): %S" (test-hash-lookup))
