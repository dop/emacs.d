(defun dynamic-cursor-mode--set-cursor (&optional type)
  (let ((next-cursor-type
         (cond
          (type)
          (buffer-read-only 'hbar)
          (t '(bar . 2)))))
    (unless (equal next-cursor-type cursor-type)
      (setq cursor-type next-cursor-type))))

(define-minor-mode dynamic-cursor-mode
  "Change cursor according to mode."
  :global t
  :lighter nil
  (if dynamic-cursor-mode
      (progn
        (add-hook 'post-self-insert-hook #'dynamic-cursor-mode--set-cursor)
        (setq set-cursor-according-to-mode-timer
              (run-with-idle-timer 1 t #'dynamic-cursor-mode--set-cursor 'box)))
    (remove-hook 'post-self-insert-hook #'dynamic-cursor-mode--set-cursor)
    (cancel-timer set-cursor-according-to-mode-timer)
    (loop for buf in (buffer-list) do (with-current-buffer buf (setq cursor-type 'box)))))

(provide 'dynamic-cursor-mode)
