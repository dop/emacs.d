;; -*- lexical-binding: t; -*-

;; An experiment to provide hunk staging in *vc-diff* buffer.
;;
;; This works around `vc-git' check-in flow by stashing working
;; changes (-k flag) so that only staged hunks in marked files will be
;; committed.
;;
;; There is still no way to see what's staged and what's not in
;; *vc-dir* buffer.

(defun hunk-to-patch ()
  "With point is over hunk extract as standalone PATCH."
  (let ((file-name (diff-find-file-name)))
    (format "--- %s\n+++ %s\n%s" file-name file-name
            (apply #'buffer-substring-no-properties (diff-bounds-of-hunk)))))

(defun git-apply-cached (patch)
  "Given a PATCH try to apply it to index."
  (with-temp-buffer
    (insert patch)
    (shell-command-on-region (point-min) (point-max) "git apply -p0 --cached")))

(defun git-diff-cached-files ()
  "Return a list of cached file paths starting from root."
  (split-string (shell-command-to-string "git diff --name-only --cached HEAD")))

(defun git-stash-push-keep-index (files)
  "Stash worktree changes to given FILES."
  (shell-command (concat "git stash push -k " (mapconcat #'identity files " "))))

(defun git-stash-pop ()
  "Pop the stash."
  (shell-command "git stash pop -q"))

(defun git-stash-worktree-files (vc-git-checkin files &rest args)
  "Advice to support git index when using `vc-git-checkin'.

`vc-git-checkin' works by either committing all files passing -a
flag to git commit command, or providing a list of files to
commit. This does not work with, for example, \"git add -p\"
workflow which allows to stage hunks."
  (let* ((cached (git-diff-cached-files))
         (root (expand-file-name (vc-root-dir)))
         (files-localized (mapcar (lambda (file) (string-replace root "" file)) files))
         (stash (and files (cl-intersection cached files-localized))))
    (when stash (git-stash-push-keep-index (mapcar (lambda (file) (expand-file-name file root)) stash)))
    (apply vc-git-checkin files args)
    (when stash (git-stash-pop))))

(advice-add 'vc-git-checkin :around #'git-stash-worktree-files)

(defun git-stage-hunk ()
  (interactive)
  (git-apply-cached (hunk-to-patch)))

(provide 'git-stage-hunk)
