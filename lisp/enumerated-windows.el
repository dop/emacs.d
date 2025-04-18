;; -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'cl-macs)

(defun enumerated-windows--compare (w1 w2 &optional direction)
  "Compare windows W1 and W2.
DIRECTION default can be:
- LRTB (left to right, top to bottom);
- TBRL (top to bottom, left to right);
- TBLR (top to bottom, left to right).

Defaults to TBLR.  Returns t
if W1 should precede W2, nil otherwise."
  (unless direction (setq direction 'lrtb))
  (pcase-let ((`(,l1 ,t1 ,b1 ,r1) (window-edges w1))
              (`(,l2 ,t2 ,b2 ,r2) (window-edges w2)))
    (cl-case direction
      (tblr (or (< l1 l2) (and (< t1 t2) (= l1 l2))))
      (tbrl (or (< l2 l1) (and (< t1 t2) (= l1 l2))))
      (lrtb (or (< t1 t2) (and (< l1 l2) (= t1 t2)))))))

(defun enumerated-windows-list ()
  "Like `window-list', but returns alist in form (INDEX . WINDOW)
where INDEX starts from 0 and increments by 1. If N is provided,
returns a WINDOW with index N."
  (let ((wins (sort (window-list nil -1) #'enumerated-windows--compare)))
    (cl-loop for i from 0 below (length wins)
             collect (cons i (nth i wins)))))

(defun enumerated-windows-get (n)
  (cdr (assoc n (enumerated-windows-list))))

(defun enumerated-windows-select (n)
  (select-window (enumerated-windows-get n)))

(defun enumerated-windows-select-command ()
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (n (- (elt keys (- (length keys) 1)) 49)))
    (enumerated-windows-select n)))

(defun enumerated-windows--last-key (&optional i)
  (let* ((keys (this-command-keys-vector)))
    (- (elt keys (- (length keys) (or i 1))) 49)))

(defun enumerated-windows-swap ()
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (k (- (elt keys (- (length keys) 2)) 49))
         (n (- (elt keys (- (length keys) 1)) 49))
         (win1 (enumerated-windows-get k))
         (win2 (enumerated-windows-get n)))
    (unless win1 (error "There is no window #%d" k))
    (unless win2 (error "There is no window #%d" n))
    (let ((buf1 (window-buffer win1))
          (buf2 (window-buffer win2)))
      (set-window-buffer win1 buf2)
      (set-window-buffer win2 buf1))))

(defun enumerated-windows-delete ()
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (n (- (elt keys (- (length keys) 1)) 49)))
    (delete-window (enumerated-windows-get n))))

(defvar enumerated-windows-mode-line-number
  '(:propertize (:eval
                 (when-let ((i (car (rassoc (selected-window) (enumerated-windows-list)))))
                   (format "%d " (1+ i))))))
(put 'enumerated-windows-mode-line-number 'risky-local-variable t)

(defvar enumerated-windows-prefix-map
  (let ((map (make-sparse-keymap))
        (digits (cl-loop for d from ?1 to ?9 collect d)))
    (dolist (digit digits)
      (define-key map (string digit) #'enumerated-windows-select-command)
      (define-key map (format "k%c" digit) #'enumerated-windows-delete)
      (dolist (digit2 digits)
        (define-key map (format "s%c%c" digit digit2) #'enumerated-windows-swap)))
    (define-key map "S" #'enumerated-windows-swap)
    map))

(defvar enumerated-windows-prefix
  (kbd "C-x w"))

(define-minor-mode enumerated-windows-mode
  "Enumerate windows."
  :keymap `((,enumerated-windows-prefix . ,enumerated-windows-prefix-map))
  :global t)

(provide 'enumerated-windows)
