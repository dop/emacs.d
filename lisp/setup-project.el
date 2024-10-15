;; -*- lexical-binding: t; -*-

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

(use-package project
  :pin gnu
  :bind (:map project-prefix-map
              ("C" . project-compile-file)
              ("V" . project-vc-top-dir)
              ("F" . project-find-top-file)
              ("G" . project-find-top-regexp))
  :config
  (advice-add 'project-find-regexp :override #'deadgrep)

  (defun project-compilation-default-buffer-name (&rest ignore)
    (concat "*" (file-name-nondirectory (directory-file-name (project-root (project-current)))) " compilation*"))

  (setq project-compilation-buffer-name-function #'project-compilation-default-buffer-name)

  (defun project-dir-locals-project (dir)
    (let ((root (locate-dominating-file dir ".dir-locals.el")))
      (and root (cons 'dir-locals root))))

  (cl-defmethod project-roots ((project (head dir-locals)))
    (list (cdr project)))

  (cl-defmethod project-ignores ((project (head dir-locals)) dir)
    (mapcar (lambda (dir) (concat dir "/"))
            vc-directory-exclusion-list))

  (add-hook 'project-find-functions #'project-dir-locals-project)

  (defun project-npm-project (dir)
    (let* ((resolve-root
            (cl-case project-preferred-root-resolution
              (top #'locate-top-dominating-file)
              (t   #'locate-dominating-file)))
           (root (funcall resolve-root dir "package.json")))
      (and root (cons 'npm root))))

  (cl-defmethod project-roots ((project (head npm)))
    (list (cdr project)))

  (cl-defmethod project-ignores ((project (head npm)) dir)
    (mapcar (lambda (dir) (concat dir "/"))
            vc-directory-exclusion-list))

  (add-hook 'project-find-functions #'project-npm-project)

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

(provide 'setup-project)
