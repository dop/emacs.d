(defun compile-with-local-compilation-error-regexp-alist (compile &rest args)
  (let ((alist compilation-error-regexp-alist))
    (with-current-buffer (apply compile args)
      (setq-local compilation-error-regexp-alist alist))))

(defvar notify--ok-sound "Glass")
(defvar notify--fail-sound "Blow")

(defun notify-on-compilation-finish (buf msg)
  (let ((sound (if (string-match-p "exited abnormally" msg)
                   notify--fail-sound
                 notify--ok-sound)))
    (do-applescript (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                            msg (buffer-name buf) sound))))

(use-package compile
  :defer t
  :config
  (add-hook 'compilation-finish-functions #'notify-on-compilation-finish)
  (push '(jest "at \\([^[:space:]]+ \\)?(?\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))?" 2 3 4) compilation-error-regexp-alist-alist)
  (push 'jest compilation-error-regexp-alist)
  (advice-add 'compile :around #'compile-with-local-compilation-error-regexp-alist))

(provide 'setup-compile)
