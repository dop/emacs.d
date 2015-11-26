(require 'dp-window)

(defvar dp/mode-line-window-number
  '(:propertize
    (:eval (concat " " (number-to-string
                        (1+ (car (rassoc (selected-window)
                                         (dp/numbered-window-list)))))
                   " "))
    face mode-line-buffer-id)
  "Number of a window position in a `window-list' sorted by
  `dp/window-cmp'")
(put 'dp/mode-line-window-number 'risky-local-variable t)

(defun dp/mode-line-right (&rest format)
  `((:eval
     (progn
       (setq-local dp/mode-line-right-length
                   (length (format-mode-line ',format)))
       (propertize
        " " 'display '((space :align-to
                              (- (+ right left-margin right-margin)
                                 dp/mode-line-right-length))))))
    ,@format))

(setq-default mode-line-position
 '(line-number-mode ("%l" (column-number-mode ":%c"))))

(setq-default mode-line-modes
 '("%[" mode-name mode-line-process minor-mode-alist "%n" "%]"))

;; (setq-default mode-line-format
;;  (list "%e" " "
;;        mode-line-modified " "
;;        mode-line-buffer-identification " "
;;        mode-line-misc-info
;;        mode-line-modes
;;        " "
;;        (dp/mode-line-right
;;         mode-line-position " "
;;         dp/mode-line-window-number)))

(setq-default mode-line-format
              (list "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                    '(vc-mode vc-mode)
                    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
                    (dp/mode-line-right dp/mode-line-window-number)))

(provide 'dp-mode-line)
