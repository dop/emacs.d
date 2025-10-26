;; -*- lexical-binding: t; -*-

(require 'project)
(require 'subr)

(defun project-list--list ()
  (unless (eq 'unset project--list)
    (project--read-project-list))
  (mapcar #'car project--list))

(defun project-list--entries ()
  (mapcar (lambda (dir) (list nil (vector dir)))
          (project-list--list)))

(defun call-with-marked-projects (fn)
  (save-excursion
    (goto-char 0)
    (while (not (eobp))
      (when (eq ?* (char-after))
        (funcall fn))
      (next-line))))

(defmacro with-marked-projects (&rest body)
  (declare (indent 0))
  `(call-with-marked-projects (lambda () ,@body)))

(defun project-list--get-marked-entries ()
  (let (entries)
    (with-marked-projects
     (push (tabulated-list-get-entry) entries))
    entries))

(defun project-list-mark ()
  (interactive)
  (tabulated-list-put-tag "*" t))

(defun project-list-unmark ()
  (interactive)
  (tabulated-list-put-tag " " t))

(defun project-list-dired ()
  (interactive)
  (dired (aref (tabulated-list-get-entry) 0)))

(defun project-list-forget ()
  (interactive)
  (with-marked-projects
    (let ((entry (tabulated-list-get-entry)))
      (project-forget-project (aref entry 0))
      (tabulated-list-delete-entry)
      (previous-line))))

(defun project-list-trash ()
  (interactive)
  (with-marked-projects
    (let* ((entry (tabulated-list-get-entry))
           (root  (aref entry 0)))
      (move-file-to-trash root)
      (project-forget-project root)
      (tabulated-list-delete-entry)
      (previous-line))))

(defun project-list--sort-name (A B)
  (string> (aref (cadr A) 0) (aref (cadr B) 0)))

(defvar-keymap project-list-mode-map
  :doc "Local keymap for `project-list-mode' buffers."
  :parent tabulated-list-mode-map
  "m" #'project-list-mark
  "u" #'project-list-unmark
  "k" #'project-list-forget
  "d" #'project-list-dired
  "RET" #'project-list-dired
  "x" #'project-list-trash)

(define-derived-mode project-list-mode tabulated-list-mode "Project List"
  "Major mode for browsing a list of projects.

\\<project-list-mode-map>
\\{project-list-mode-map}"
  :interactive nil
  (setq tabulated-list-format `[("Project" 30 project-list--sort-name)])
  (setq tabulated-list-entries #'project-list--entries)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun list-projects ()
  (interactive)
  (let ((buf (get-buffer-create "*Projects*")))
    (with-current-buffer buf
      (project-list-mode))
    (pop-to-buffer buf)))
