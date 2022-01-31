(defun compile-with-local-compilation-error-regexp-alist (compile &rest args)
  (let ((alist compilation-error-regexp-alist))
    (with-current-buffer (apply compile args)
      (setq-local compilation-error-regexp-alist alist))))

(use-package compile
  :defer t
  :config
  (push '(jest "at [^(]+(\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))\\(\n +.*\\)+" 1 2 3) compilation-error-regexp-alist-alist)
  (push 'jest compilation-error-regexp-alist)
  (advice-add 'compile :around #'compile-with-local-compilation-error-regexp-alist))

(provide 'setup-compile)
