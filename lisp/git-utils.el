;; -*- lexical-binding: t; -*-

(defun git-checkout (branch)
  "Checkout a branch."
  (interactive
   (list (pcase-let ((`(,current . ,options) (vc-git-branches)))
           (when options
             (completing-read (format "Switch from \"%s\" to: " current) options)))))
  (if branch
      (if (member branch (vc-git-branches))
          (shell-command (format "git checkout %s" branch))
        (shell-command (format "git checkout -b %s" branch)))
    (message "No local branches to select from.")))

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
   (list (pcase-let ((`(,current . ,options) (vc-git-branches)))
           (completing-read-multiple "Delete branches: " options nil t)) ))
  (shell-command (format "git branch -D %s" (string-join branches " "))))

(defun git-diff ()
  "Create a buffer to see git diff between current staging and master
branch."
  (interactive)
  (let* ((project (project-current))
         (buffer-name (concat "*diff" (if project (concat " " (project-name project)) "") "*"))
         (buf (get-buffer-create buffer-name))
         (populate-diff
          (lambda (&rest ignore)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (shell-command "git diff master -- ." buf))))))
    (funcall populate-diff)
    (with-current-buffer buf
      (setq-local buffer-read-only t)
      (diff-mode)
      (setq-local revert-buffer-function populate-diff)
      (pop-to-buffer buf nil t))))

(provide 'git-utils)
