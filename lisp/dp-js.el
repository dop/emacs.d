;; Borrowed from http://www.zakrevskis.lt/coding/2017/05/05/emacs-sort-js-imports-on-save.html

(defvar sort-js-imports-wheights nil
  "Pairs of REGEXP and numeric WEIGHT.")

(defvar sort-js-imports-default-weight 0
  "Default WEIGHT to use if line is not matched against `sort-js-imports-wheights'.")

(setq sort-js-imports-wheights
      '(("'.?./" . 1000)
        ("^$" . 500)))

(defun sort-js-imports ()
  (interactive)
  (let ((end-of-imports (save-excursion
                          (goto-char (point-max))
                          (search-backward-regexp "import")
                          (word-search-forward "from" nil t)
                          (line-end-position))))
    (sort-js-imports-region nil (point-min) (get-end-of-js-import))))

(defun sort-js-imports-region (reverse beg end)
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse
                   'forward-line
                   (lambda () (search-forward "from" nil t) (end-of-line))
                   (lambda ()
                     (let ((skip-amount (save-excursion (search-forward "from" nil t)))
                           (skip-amount-end (line-end-position)))
                       (cons skip-amount skip-amount-end)))
                   nil
                   (lambda (a b)
                     (let ((s1 (buffer-substring (car a) (cdr a)))
                           (s2 (buffer-substring (car b) (cdr b))))
                       (calculate-string-priority s1 s2))))))))

(defun calculate-string-priority (s1 s2)
  (< (+ (calculate-js-import-priority s1)
        (if (string< s1 s2) -1 1))
     (calculate-js-import-priority s2)))

(defun calculate-js-import-priority (import)
  (cdr (or (find-if (pcase-lambda (`(,regex . ,_))
                      (string-match regex import))
                    sort-js-imports-wheights)
           (cons nil sort-js-imports-default-weight))))
