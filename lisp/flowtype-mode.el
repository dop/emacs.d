(define-derived-mode flowtype-mode typescript-mode "flowtype")

(add-hook 'flowtype-mode-hook #'subword-mode)
(add-hook 'flowtype-mode-hook #'flycheck-mode)
(flycheck-add-mode 'javascript-flow 'flowtype-mode)
(flycheck-add-mode 'javascript-eslint 'flowtype-mode)

(define-key flowtype-mode-map "\C-c\C-t" #'dp/flow-type-at-pos)

(defun column-number-at-pos ()
  (- (point) (point-at-bol)))

(defun dp/flow-type-at-pos ()
  (interactive)
  (let* ((flow-bin (concat (eproject-root) (eproject-attribute :flow)))
         (command (concat flow-bin " type-at-pos --json " (buffer-file-name)
                          " "
                          (number-to-string (line-number-at-pos))
                          " "
                          (number-to-string (1+ (column-number-at-pos))))))
    (message "%s: %s"
             (symbol-at-point)
             (assocdr 'type
                      (json-read-from-string (shell-command-to-string command))))))

(provide 'flowtype-mode)
