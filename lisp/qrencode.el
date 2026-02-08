;; Depends on qrencode from:
;; - https://fukuchi.org/works/qrencode/
;; - https://github.com/fukuchi/libqrencode
(defun qrencode-string (string)
  "Encodes STRING as PNG QR code using qrencode utility. Result is placed
in a *QR* buffer with image mode enabled."
  (let ((output (get-buffer-create "*QR*"))
        (err    (get-buffer-create " *qrencode errors*")))
    (with-current-buffer output
      (fundamental-mode)
      (erase-buffer))
    (make-process :name "qrencode"
                  :command `("qrencode" "-s" "8" "-o" "-" ,string)
                  :stderr err
                  :buffer output
                  :noquery t
                  :sentinel (lambda (proc event)
                              (when (eq 'exit (process-status proc))
                                (with-current-buffer output
                                  (goto-char 0)
                                  (image-mode)
                                  (keymap-local-set "q" #'kill-buffer-and-window)
                                  (keymap-local-set "Q" #'kill-buffer-and-window))
                                (pop-to-buffer output nil t))))))

(defun qrencode ()
  "Encode region as QR code. If no region is selected, prompts to select a
thing."
  (interactive)
  (mark-and-copy-mark)
  (qrencode-string (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))))


(provide 'qrencode)
;; end of qrencode
