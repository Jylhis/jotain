;;; init-review.el --- Whole-repository review queue -*- lexical-binding: t; -*-

;;; Commentary:

;; Binds the self-contained lisp/repo-review.el library into the configuration,
;; with org-remark for marginal notes that never touch the sources.

;;; Code:

(use-package repo-review
  :ensure nil
  :bind (:map project-prefix-map
              ("R" . repo-review-start)
              ("V" . repo-review-pullreq))
  :custom
  (repo-review-queue-dir (jotain-var-file "repo-review/")))

(use-package org-remark
  :defer t)

(provide 'init-review)
;;; init-review.el ends here
