;;; test-dashboard.el --- Tests for dashboard.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'dired)
(require 'enlight)
(require 'dashboard)

(ert-deftest dashboard-initial-buffer-no-file-visiting-buffers ()
  "Test that the dashboard opens when there are no file-visiting or dired buffers."
  ;; Create a temporary buffer list that does not contain file or dired buffers
  (cl-letf (((symbol-function 'buffer-list)
             (lambda ()
               (list (get-buffer-create "*scratch*")
                     (get-buffer-create "*Messages*")))))
    (should (equal (jotain-dashboard-initial-buffer) (enlight)))))

(ert-deftest dashboard-initial-buffer-with-file-visiting-buffer ()
  "Test that the dashboard is skipped when there is a file-visiting buffer."
  (let ((file-buf (get-buffer-create "test-file.txt")))
    (unwind-protect
        (with-current-buffer file-buf
          (setq buffer-file-name "/tmp/test-file.txt")
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda ()
                       (list (get-buffer-create "*scratch*")
                             file-buf))))
            (should-not (equal (jotain-dashboard-initial-buffer) (enlight)))
            (should (equal (jotain-dashboard-initial-buffer) file-buf))))
      (kill-buffer file-buf))))

(ert-deftest dashboard-initial-buffer-with-dired-buffer ()
  "Test that the dashboard is skipped when there is a dired buffer."
  (let ((dir-buf (get-buffer-create "test-dir")))
    (unwind-protect
        (with-current-buffer dir-buf
          (setq dired-directory "/tmp/test-dir/")
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda ()
                       (list (get-buffer-create "*scratch*")
                             dir-buf))))
            (should-not (equal (jotain-dashboard-initial-buffer) (enlight)))
            (should (equal (jotain-dashboard-initial-buffer) dir-buf))))
      (kill-buffer dir-buf))))

(provide 'test-dashboard)
;;; test-dashboard.el ends here
