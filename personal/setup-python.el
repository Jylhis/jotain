(require 'use-package)

(use-package cython-mode
  :straight t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode)))


(use-package python
  :straight nil
  :after
  (highlight-indentation)
  :init
  (progn
    (add-hook 'python-mode-hook 'highlight-indentation-mode)
    ;; (add-hook 'python-mode-hook 'eldoc-mode)
    ;;    (add-hook 'python-mode-hook 'sphinx-doc-mode))
    )
  :bind (:map python-mode-map
	      (
               ("C-c i" . python-insert-docstring-with-google-style-at-point)
               ))
  :config

  (when (executable-find "ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
  )
(use-package python-insert-docstring
  :straight t)
;;(use-package blacken
 ;; :straight t)

;;(use-package python-black
;;  :straight t)




(use-package lsp-pyright
  :straight t
  :after lsp-mode


  :custom
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off")
  (lsp-pyright-python-executable-cmd "python3")

  )


(use-package pyvenv
  :straight t
  :after python
  :config
  (defun dn-activate-pyvenv ()
    "Activate python environment according to the `.venv' file."
    (interactive)
    (pyvenv-mode)
    (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
      (if (file-exists-p pfile)
          (pyvenv-workon (with-temp-buffer
                           (insert-file-contents pfile)
                           (nth 0 (split-string (buffer-string))))))))
  )
