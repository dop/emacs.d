;; -*- lexical-binding: t; -*-

(defvar git-possible-main-branches '("master" "main"))

(defun git-has-branch-p (branch-name)
  (not (string-empty-p (shell-command-to-string (concat "git rev-parse -q --verify " branch-name)))))

(defun git-get-main-branch ()
  (seq-find #'git-has-branch-p git-possible-main-branches))

(defun git-merge-base (A B)
  (string-trim (shell-command-to-string (format "git merge-base %s %s" A B))))

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

(defun git-log-recents (&rest args &key limit)
  (let* ((commits (shell-command-to-string (format "git log -n %d --format=\"%%h %%s -- %%an\"" (or limit 20))))
         (pairs (mapcar (lambda (line)
                          (let ((pos  (seq-position line 32)))
                            (cons (seq-subseq line 0 pos)
                                  (seq-subseq line (1+ pos)))))
                        (string-split commits "\n" 'omit-nulls))))
    pairs))

(defun git-read-recent-commit ()
  (let* ((vertico-sort-function nil)
         (commits (git-log-recents))
         (collection
          (lambda (str pred flag)
            (cond ((eq 'metadata flag)
                   `(metadata
                     (annotation-function . ,(lambda (cand)
                                               (concat " " (alist-get cand commits nil nil #'equal))))))
                  (nil
                   (try-completion str commits pred))
                  (t
                   (all-completions str commits pred))))))
    (completing-read "Commit: " collection)))

(defun project-run-diff (command)
  (let* ((project (project-current))
         (buffer-name (concat "*diff" (if project (concat " " (project-name project)) "") "*"))
         (buf (get-buffer-create buffer-name))
         (populate-diff
          (lambda (&rest ignore)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (shell-command command buf))))))
    (funcall populate-diff)
    (with-current-buffer buf
      (setq-local buffer-read-only t)
      (diff-mode)
      (setq-local revert-buffer-function populate-diff)
      (pop-to-buffer buf nil t))))

(defun git-diff (&optional arg)
  "Create a buffer to see git diff between current staging and master
branch."
  (interactive "P")
  (let ((commit (if arg (git-read-recent-commit) "master")))
    (project-run-diff (format "git diff %s -- ." commit))))

(defun github-diff ()
  "Create a buffer to see GitHub PR style diff between master and a branch."
  (interactive)
  (let* ((main-branch (git-get-main-branch))
         (merge-base (git-merge-base main-branch "HEAD")))
    (project-run-diff (format "git diff %s -- ."  merge-base))
    (message (shell-command-to-string (format "git diff --shortstat %s -- ."  merge-base)))))

(provide 'git-utils)
