(defun dp/numbered-window-list (&optional n)
  "Like `window-list', but returns alist in form (INDEX . WINDOW)
where INDEX starts from 0 and increments by 1. If N is provided,
returns a WINDOW with index N."
  (let* ((wins (sort (window-list nil -1) #'dp/window-cmp))
         (numbered (loop for i from 0 below (length wins)
                         collect (cons i (nth i wins)))))
    (if n
        (cdr (assoc n numbered))
      numbered)))

(defun dp/window-cmp (w1 w2 &optional direction)
  "Compare windows W1 and W2.
DIRECTION can be either lrtb (left to right, top to bottom) or
tbrl (top to bottom, left to right). Defaults to tblr.  Returns t
if W1 should precede W2, nil otherwise."
  (unless direction (setq direction 'tblr))
  (pcase-let ((`(,l1 ,t1 _ _) (window-edges w1))
              (`(,l2 ,t2 _ _) (window-edges w2)))
    (case direction
      (tblr (or (< l1 l2) (and (< t1 t2) (= l1 l2))))
      (lrtb (or (< t1 t2) (and (< l1 l2) (= t1 t2)))))))

(defun dp/jump-to-window ()
  (interactive)
  (let* ((onem (elt (kbd "M-1") 0))
         (onec (elt (kbd "C-1") 0))
         (keys (this-command-keys-vector))
         (n    (- (elt keys (- (length keys) 1)) onem))
         (k    (if (eq 2 (length keys))
                   (- (elt keys 0) onec))))
    (if (and n k)
        (dp/swap-windows (1+ n) (1+ k))
      (let ((win (dp/numbered-window-list n)))
        (if win (select-window win)
          (error "There is no window #%d" (1+ n)))))))

(defun dp/swap-windows (k n)
  (interactive "nFirst window #: \nnSecond window #: ")
  (let ((win1 (dp/numbered-window-list (- k 1)))
        (win2 (dp/numbered-window-list (- n 1))))
    (unless win1 (error "There is no window #%d" k))
    (unless win2 (error "There is no window #%d" n))
    (let ((buf1 (window-buffer win1))
          (buf2 (window-buffer win2)))
      (set-window-buffer win1 buf2)
      (set-window-buffer win2 buf1))))

(provide 'dp-window)
