;; -*- lexical-binding: t; -*-

(require 'pcase)
(require 'subr-x)

(defvar mark-and-copy-options
  '((?s "sexp"      mark-sexp)
    (?e "sentence"  mark-end-of-sentence)
    (?w "word"      mark-word)
    (?p "paragraph" mark-paragraph)
    (?f "defun"     mark-defun)
    (?a "all"       mark-whole-buffer)))

(defun mark-and-copy--prompt ()
  (format "Copy %s: "
          (mapconcat
           (pcase-lambda (`(,c ,word ,_))
             (let ((pos (cl-position c word)))
               (format "%s(%c)%s" (substring word 0 pos) c (substring word (1+ pos)))))
           mark-and-copy-options
           ", ")))

(defun mark-and-copy ()
  "Mark part of buffer before calling `kill-ring-save'."
  (interactive)
  (when-let* ((option (or (use-region-p) (read-char (mark-and-copy--prompt))))
              (mark (nth 2 (seq-find (lambda (o) (eq option (car o))) mark-and-copy-options))))
    (funcall mark))
  (let ((beg (region-beginning))
        (end (region-end)))
    (pulse-momentary-highlight-region beg end)
    (kill-ring-save beg end)))

(provide 'mark-and-copy)
