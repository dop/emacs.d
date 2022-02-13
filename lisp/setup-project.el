;; -*- lexical-binding: t; -*-

(use-package project
  :pin gnu
  :config
  (advice-add 'risky-local-variable-p :override #'ignore)

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
    (let ((root (locate-dominating-file dir "package.json")))
      (and root (cons 'npm root))))

  (cl-defmethod project-roots ((project (head npm)))
    (list (cdr project)))

  (cl-defmethod project-ignores ((project (head npm)) dir)
    (mapcar (lambda (dir) (concat dir "/"))
            vc-directory-exclusion-list))

  (add-hook 'project-find-functions #'project-npm-project))

(provide 'setup-project)
