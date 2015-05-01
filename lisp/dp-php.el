;;; PHP

(defvar dp/php-function-def-regexp
  "\\b\\(\\(protected\\|private\\|public\\)\s+\\)?\\(static\s+\\)?function\s+[a-zA-Z0-9_]+\s*?(")

(defmacro dp/php-find-function-or-method (&optional direction)
  "Jump to next or previous function in current buffer."
  `(let ((jump-to)
         (jump-from)
         (search-fun ',(case direction
                         (backward 're-search-backward)
                         (t        're-search-forward))))
     (cl-flet ((match-at () ,(case direction
                               (backward '(match-end 0))
                               (t        '(match-beginning 0)))))
       (save-excursion
         (setq jump-from (point))
         (apply search-fun dp/php-function-def-regexp '(nil t))
         (if (= jump-from (match-at))
             (apply search-fun dp/php-function-def-regexp '(nil t)))
         (when ,(case direction
                  (backward '(> jump-from (match-at)))
                  (t        '(< jump-from (match-at))))
           (setq jump-to (match-beginning 0))))
       (when jump-to
         (goto-char jump-to)
         (recenter)))))

(defun dp/php-next-function ()
  "Jump to next the function in current buffer."
  (interactive)
  (dp/php-find-function-or-method forward))

(defun dp/php-previous-function ()
  "Jump to the previous function in current buffer."
  (interactive)
  (dp/php-find-function-or-method backward))


(defun dp/php-jump-to-function-definition (&optional read-name)
  "Find definition of method or function in a buffer."
  (interactive "P")
  (let (name)
    (setq name (if read-name
                   (read-from-minibuffer "Name: ")
                 (symbol-at-point)))
    (when (symbolp name)
      (setq name (symbol-name name)))
    (if name
        (let ((re (concat "^\s*\\(protected\\|private\\|public\\)?\s*function\s+"
                          name
                          "\s*?("))
              (matched-at))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward re nil t)
            (setq matched-at (match-beginning 0)))
          (when matched-at
            (goto-char matched-at)))
      (message "No name given."))))

(provide 'dp-php)
