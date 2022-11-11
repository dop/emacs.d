;; -*- lexical-binding: t; -*-

(defmacro comment (&rest body))

(defun alphanumeric-char-p (c)
  (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9)))

(defun last1 (list)
  "Return car of a last cell."
  (car (last list)))

(defun dot-pair-p (object)
  "Check if OBJECT is a cons cell with atom in cdr."
  (and (consp object) (atom (cdr object))))

(provide 'library)
