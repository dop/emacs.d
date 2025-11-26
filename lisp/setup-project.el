;; -*- lexical-binding: t; -*-

(require 'project-tools)
(defcustom project-preferred-root-resolution 'default
  "Choose how project root is resolved."
  :type 'symbol
  :options '(default top)
  :safe t)

(defun project-vc-top-dir ()
  "Run `project-vc-dir' in top project."
  (interactive)
  (let ((project-preferred-root-resolution 'top))
    (call-interactively #'project-vc-dir)))

(defun project-find-top-regexp ()
  "Run `project-find-regexp' in top project."
  (interactive)
  (let ((project-preferred-root-resolution 'top))
    (call-interactively #'project-find-regexp)))

(defun project-find-top-file ()
  "Run `project-find-file' in top project."
  (interactive)
  (let ((project-preferred-root-resolution 'top))
    (call-interactively #'project-find-file)))

(defun project-compile-file (file)
  (interactive (list (or (buffer-file-name) (read-file-name "File: "))))
  (let* ((root (expand-file-name (project-current-root)))
         (compile-command (concat compile-command (string-replace root "" file))))
    (message "root: %s, file: %s" root file)
    (call-interactively #'project-compile)))

(use-package project
  :pin gnu
  :bind (:map project-prefix-map
              ("C" . project-compile-file)
              ("V" . project-vc-top-dir)
              ("F" . project-find-top-file)
              ("G" . project-find-top-regexp))
  :config
  (advice-add 'project-find-regexp :override #'deadgrep))

(with-eval-after-load "project"
  (defun project-prefixed-buffer-name (mode)
    (concat "*"
            (project-name (project-current))
            " "
            (downcase mode)
            "*")))

;; .envrc project
(with-eval-after-load "project"
  (defun project-envrc-project (dir)
    (let* ((resolve-root
            (cl-case project-preferred-root-resolution
              (top #'locate-top-dominating-file)
              (t   #'locate-dominating-file)))
           (root (funcall resolve-root dir ".envrc")))
      (and root (cons 'envrc root))))

  (cl-defmethod project-roots ((project (head envrc)))
    (list (cdr project)))

  (cl-defmethod project-ignores ((project (head envrc)) dir)
    (mapcar (lambda (dir) (concat dir "/"))
            vc-directory-exclusion-list))

  (add-hook 'project-find-functions #'project-envrc-project))

;; dir-locals project
(with-eval-after-load "project"
  (defun project-dir-locals-project (dir)
    (let ((root (locate-dominating-file dir ".dir-locals.el")))
      (and root (cons 'dir-locals root))))

  (cl-defmethod project-roots ((project (head dir-locals)))
    (list (cdr project)))

  (cl-defmethod project-ignores ((project (head dir-locals)) dir)
    (mapcar (lambda (dir) (concat dir "/"))
            vc-directory-exclusion-list))
  (add-hook 'project-find-functions #'project-dir-locals-project))

;; NPM project
(with-eval-after-load "project"
  (defvar project-npm--name-cache (make-hash-table :test 'equal))

  (defun project-npm--get-and-cache-package-name (dir)
    (let ((name (gethash dir project-npm--name-cache)))
      (unless name
        (setq name (let-alist (json-read-file (expand-file-name "package.json" dir)) .name))
        (setf (gethash dir project-npm--name-cache) name))
      name))

  (defun project-npm-project (dir)
    (let* ((resolve-root
            (cl-case project-preferred-root-resolution
              (top #'locate-top-dominating-file)
              (t   #'locate-dominating-file)))
           (suffix "")
           (root (funcall resolve-root dir "package.json")))
      (when root
        ;; .git is a regular file in a worktree
        (when (file-regular-p (expand-file-name ".git" (locate-dominating-file default-directory ".git")))
          (setq suffix (concat " " (car (vc-git-branches)))))
        (list 'npm
              (concat (project-npm--get-and-cache-package-name root) suffix)
              root))))

  (cl-defmethod project-name ((project (head npm)))
    (cadr project))

  (cl-defmethod project-roots ((project (head npm)))
    (last project))

  (cl-defmethod project-ignores ((project (head npm)) dir)
    (mapcar (lambda (dir) (concat dir "/"))
            vc-directory-exclusion-list))

  (add-hook 'project-find-functions #'project-npm-project))

(provide 'setup-project)
