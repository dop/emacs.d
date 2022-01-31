;; -*- lexical-binding: t; -*-

(require 'pcase)
(require 'subr-x)

(defmacro define-mark-whole (name mark forward backward)
  `(defun ,(intern (format "mark-whole-%s" name)) ()
     (interactive)
     (let ((p1 (point))
           (p2 (save-excursion (ignore-errors (,backward) (,forward)) (point)))
           (p3 (save-excursion (ignore-errors (,forward) (,backward)) (point))))
       (when (and (not (= p2 p3)) (<= p1 p2))
         (,backward))
       (,mark))))

(defun mark-end-of-sentence-1 ()
  (mark-end-of-sentence 1))

(define-mark-whole sexp
  mark-sexp forward-sexp backward-sexp)
(define-mark-whole word
  mark-word forward-word backward-word)
(define-mark-whole sentence
  mark-end-of-sentence-1 forward-sentence backward-sentence)

(defvar mark-and-copy-options
  '((?s "sexp"      mark-whole-sexp)
    (?e "sentence"  mark-whole-sentence)
    (?w "word"      mark-whole-word)
    (?p "paragraph" mark-paragraph)
    (?f "defun"     mark-defun)
    (?a "all"       mark-whole-buffer)))

(defun mark-and-copy--prompt ()
  (format "Select %s: "
          (mapconcat
           (pcase-lambda (`(,c ,word ,_))
             (let ((pos (cl-position c word)))
               (format "%s(%c)%s" (substring word 0 pos) c (substring word (1+ pos)))))
           mark-and-copy-options
           ", ")))

(defun mark-and-copy-mark ()
  "Mark part of buffer."
  (interactive)
  (when-let* ((option (or (use-region-p) (read-char (mark-and-copy--prompt))))
              (mark (nth 2 (seq-find (lambda (o) (eq option (car o))) mark-and-copy-options))))
    (funcall mark)))

(defun mark-and-copy ()
  "Copy part of buffer."
  (interactive)
  (mark-and-copy-mark)
  (let ((beg (region-beginning))
        (end (region-end)))
    (pulse-momentary-highlight-region beg end)
    (kill-ring-save beg end)))

(provide 'mark-and-copy)
