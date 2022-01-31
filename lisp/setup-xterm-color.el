(defun xterm-color-compilation-filter ()
  (let* ((inhibit-read-only t)
         (start compilation-filter-start)
         (end   (point))
         (input (delete-and-extract-region start end)))
    (goto-char (point-max))
    (insert (xterm-color-filter input))))

;; Nice colors for tools that produce color escape codes.
(use-package xterm-color
  :commands xterm-color-filter
  :hook ((compilation-filter . xterm-color-compilation-filter))
  :config
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

(provide 'setup-xterm-color)
