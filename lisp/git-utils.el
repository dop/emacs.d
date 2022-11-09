;; -*- lexical-binding: t; -*-

(defun git-branches ()
  "Get a list of local branches which first being the current one."
  (let ((branches (split-string (shell-command-to-string "git branch") "\n" 'omit-nulls)))
    (cl-loop for branch in branches
             when (string-prefix-p "* " branch) collect (string-trim branch "[ *]+") into current
             else collect (string-trim branch) into options
             finally (return (cons (car current) options)))))

(defun git-checkout (branch)
  "Checkout a branch."
  (interactive
   (list (pcase-let ((`(,current . ,options) (git-branches)))
           (completing-read (format "Switch from \"%s\" to: " current) options))))
  (shell-command (format "git checkout %s" branch)))

(defun vc-dir-buffer-list ()
  (let ((vc-root (expand-file-name (vc-root-dir))))
    (seq-filter (lambda (buffer)
                  (and (string-match-p "^\\*vc-dir\\*" (buffer-name buffer))
                       (with-current-buffer buffer
                         (string-prefix-p vc-root default-directory))))
                (buffer-list))))

(defun revert-vc-buffers ()
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (revert-buffer nil t)))
        (vc-dir-buffer-list)))

(defun git-checkout-revert-vc-buffers (&rest ignore)
  (revert-vc-buffers))

(advice-add 'git-checkout :after #'git-checkout-revert-vc-buffers)

(defun git-delete-branches (branches)
  "Select and delete branches."
  (interactive
   (list (pcase-let ((`(,current . ,options) (git-branches)))
           (completing-read-multiple "Delete branches: " options nil t)) ))
  (shell-command (format "git branch -D %s" (string-join branches " "))))

(provide 'git-utils)
