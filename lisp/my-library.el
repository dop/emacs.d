;; -*- lexical-binding: t; -*-

(defmacro comment (&rest body))

(defun alphanumeric-char-p (c)
  (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9)))

(defun random-alphanumeric-string (&optional size)
  "Return random string consisting of alphanumeric characters.

Encodes bytes from /dev/random using `base64-encode-string' and
returns at SIZE length string consisting of alphanumeric
characters."
  (let* ((size (or size 12))
         (command (format "head -c %d /dev/random" (+ 2 size)))
         (base64 (base64-encode-string
                  (encode-coding-string (shell-command-to-string command)
                                        'latin-1))))
    (substring (apply #'string (seq-filter #'alphanumeric-char-p base64))
               0 size)))

(ert-deftest random-alphanumeric-string ()
  "Tests basics of `random-alphanumeric-string'."
  (should (eq 'string (type-of (random-alphanumeric-string))))
  (should (= 12 (length (random-alphanumeric-string))))
  (should (= 5 (length (random-alphanumeric-string 5)))))

(defun last1 (seq)
  "Last element of a sequence or nil."
  (let ((len (seq-length seq)))
    (when (> len 0)
      (seq-elt seq (1- len)))))

(defun dot-pair-p (object)
  "Check if OBJECT is a cons cell with atom in cdr."
  (and (consp object) (atom (cdr object))))

(defmacro cond-let (&rest clauses)
  "Like `cond' but each test result is bound to a VAR.

usage: (cond-let ((VAR TEST) BODY...))."
  (let ((tag (gensym "tag")))
    `(catch ',tag
       ,@(mapcar (lambda (clause)
                   (cond ((eq t (car clause))
                          `(throw ',tag
                                  (progn
                                    ,@(cdr clause))))
                         (t
                          `(when-let (,(car clause))
                             (throw ',tag
                                    (progn
                                      ,@(cdr clause)))))))
                 clauses))))

(comment
 (macroexpand-1 '(cond-let ((a 1)
                            (+ a b))))
 (= 1 (cond-let ((a 1) a)))
 (= 1 (cond-let ((a nil) 0) (t 1))))

(defmacro cond-it (&rest clauses)
  "Like `cond-let' but each test is bound to IT symbol."
  `(cond-let ,@(mapcar (lambda (clause)
                         (let ((test (car clause))
                               (body (cdr clause)))
                           `((it ,test) ,@body)))
                       clauses)))

(comment
 (macroexpand-1 '(cond-it (1 it)))
 (= 1 (cond-it (1 it) (t 2)))
 (eq 1 (cond-it (nil nil) (t 1))))

(defmacro collecting (collectors &rest body)
  (declare (indent 1))
  (let ((single (atom collectors))
        (value  (gensym "value")))
    (when single
      (setq collectors (list collectors)))
    `(let ,collectors
       (cl-flet ,(mapcar (lambda (collector)
                           `(,collector (,value) (push ,value ,collector) ,value))
                         collectors)
         ,@body
         ,(if single
              `(nreverse ,(car collectors))
            `(mapcar #'nreverse (list ,@collectors)))))))

(comment
 (macroexpand-1 '(collecting (a b c)
                   (a 1) (b 2) (c 3)))
 (equal '(1) (collecting x (x 1))))


(defun alist-hash-table (alist &rest table-keyword-args)
  "Construct an a hash table from ALIST."
  (let ((table (apply #'make-hash-table table-keyword-args)))
    (mapc (pcase-lambda (`(,key . ,value))
            (setf (gethash key table) value))
          alist)
    table))

(defun hash-table-alist (table)
  "Construct an alist from a TABLE hash table."
  (let ((alist nil))
    (maphash (lambda (key value) (push (cons key value) alist)) table)
    alist))

(defmacro for (spec &rest body)
  (declare (indent 1))
  (let ((var (car spec))
        (from-expr (cadr spec))
        (to-expr (caddr spec))
        (from (gensym "from"))
        (to (gensym "to"))
        (counter (gensym "counter"))
        (iterator (gensym "iterator"))
        (step (gensym "step")))
    `(let* ((,from ,from-expr)
            (,to ,to-expr)
            (,step (if (< ,from ,to) 1 -1))
            (,var ,from))
       (dotimes (,counter (abs (- ,to ,from)))
         ,@body
         (cl-incf ,var ,step)))))

(comment
 (macroexpand-1 '(for ((c '(1 2 3))) (f c)))
 (equal '(1 2 3 4 5) (collecting num (for (x 1 6) (num x))))
 (equal '(10 9 8 7 6) (collecting num (for (x 10 5) (num x)))))

(defmacro sref (seq &rest indeces)
  (if indeces
      `(sref (elt ,seq ,(car indeces)) ,@(cdr indeces))
    seq))

(defun up-directory (file)
  (file-name-directory (directory-file-name (file-name-as-directory file))))

(defun locate-top-dominating-file (file name)
  (when-let ((dominating-file
              (locate-dominating-file file name)))
    (or (locate-top-dominating-file (up-directory dominating-file) name)
        dominating-file)))

(defun url-retrieve-body (url callback &optional cbargs silent inhibit-cookies)
  "Same as `url-retrieve', but extracts only body part as string as passes it to CALLBACK."
  (let ((buf (current-buffer)))
    (url-retrieve url (lambda (status)
                        (beginning-of-buffer)
                        (forward-paragraph)
                        (next-line)
                        (beginning-of-line)
                        (let ((body (buffer-substring (point) (point-max))))
                          (with-current-buffer buf (funcall callback body))))
                  cbargs silent inhibit-cookies)))

(defun ressoc (k v alist)
  (cons (cons k v)
        (cl-remove k alist :key #'car)))

(provide 'my-library)
