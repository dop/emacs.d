;; Stripping non-color ANSI escape codes.
;;
;; from https://emacs.stackexchange.com/a/38531
(defvar ansi-ignored-escape-codes
  (rx (or
       ;; icon name escape sequences
       (regexp "\033\\][0-2];.*?\007")
       ;; non-SGR CSI escape sequences
       (regexp "\033\\[\\??[0-9;]*[^0-9;m]")
       ;; noop
       (regexp "\012\033\\[2K\033\\[1F"))))

(defun ansi-escape-codes-filter (string)
  (replace-regexp-in-string ansi-ignored-escape-codes "" string))

(defun xterm-color-compilation-filter ()
  (let* ((inhibit-read-only t)
         (start compilation-filter-start)
         (end   (point))
         (input (delete-and-extract-region start end)))
    (goto-char (point-max))
    (insert (xterm-color-filter input))))

;; Nice colors for tools that produce color escape codes.
(use-package xterm-color
  :after commint
  :hook ((compilation-filter . xterm-color-compilation-filter))
  :config
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter 0)
  (add-hook 'comint-preoutput-filter-functions #'ansi-escape-codes-filter 1)
  (remove-hook 'comint-output-filter-functions 'ansi-color-process-output))

(provide 'setup-xterm-color)
