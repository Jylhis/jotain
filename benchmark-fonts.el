(require 'benchmark)

(defun generate-dummy-fonts (n)
  (let ((res nil))
    (dotimes (i n)
      (push (format "Font %d" i) res))
    (push "Target Font 1" res)
    (push "Target Font 2" res)
    (push "Target Font 3" res)
    res))

(setq dummy-font-list (generate-dummy-fonts 2000))

(defun test-list-lookup ()
  (let ((targets '("Target Font 1" "Target Font 2" "Target Font 3")))
    (seq-find (lambda (target)
                (member target dummy-font-list))
              targets)))

(setq dummy-font-hash (make-hash-table :test 'equal))
(dolist (f dummy-font-list)
  (puthash f t dummy-font-hash))

(defun test-hash-lookup ()
  (let ((targets '("Target Font 1" "Target Font 2" "Target Font 3")))
    (seq-find (lambda (target)
                (gethash target dummy-font-hash))
              targets)))

(message "List lookup benchmark:")
(benchmark 1000 '(test-list-lookup))

(message "Hash lookup benchmark:")
(benchmark 1000 '(test-hash-lookup))
