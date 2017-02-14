(ignore-errors
  (require-package 'erlang))

(when (package-installed-p 'erlang)
  (require 'erlang-start))

(when (package-installed-p 'company)
  (require 'company))
(when (package-installed-p 'company-distel)
  (require 'company-distel))
(add-to-list 'company-backends 'company-distel)

(provide 'init-erlang)
