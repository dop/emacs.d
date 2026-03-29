;; -*- lexical-binding: t; -*-

(require 'project-tools)

(defcustom project-preferred-root-resolution 'default
  "Choose how project root is resolved."
  :type 'symbol
  :options '(default top)
  :safe t)

(defun project-locate-dominating-file (file name)
  "Wrapper for `locate-dominating-file' that takes `project-preferred-root-resolution' into account."
  (funcall (cl-case project-preferred-root-resolution
             (top #'locate-top-dominating-file)
             (t   #'locate-dominating-file))
           file name))

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

(defun project-top-compile ()
  "Run `project-compile' in top project."
  (interactive)
  (let ((project-preferred-root-resolution 'top))
    (call-interactively #'project-compile)))

(defun project-read-script (project &optional predicate)
  (let* ((scripts (project-scripts project))
         (collection
          (lambda (str pred flag)
            (cond ((eq 'metadata flag)
                   `(metadata
                     (annotation-function . ,(lambda (cand)
                                               (concat " " (alist-get (intern cand) scripts))))))
                  (nil
                   (try-completion str scripts pred))
                  (t
                   (all-completions str scripts pred))))))
    (completing-read (format "%s script: " (project-name project))
                     collection
                     predicate)))

(defun project-run-command (command)
  (interactive (list (project-read-script (project-current t))))
  (project-run (project-current t) command))

(defun project-run-top-command ()
  (interactive)
  (let ((project-preferred-root-resolution 'top))
    (call-interactively #'project-run-command)))

(defun project-relative-path (project path)
  (let ((root (expand-file-name (project-root project))))
    (when (string-prefix-p root path)
      (string-remove-prefix root path))))

(defun project-relative-files (project)
  (mapcar (apply-partially #'project-relative-path project)
          (project-files project)))

(defun project-run-test (file command)
  (interactive (let* ((project (project-current t)))
                 (list (completing-read "Test file: " (project-relative-files project)
                                        (lambda (file)
                                          (and file (string-match-p "\\(\\.\\(spec\\|test\\)\\.\\|__tests?__\\)" file)))
                                        nil
                                        (project-relative-path project (or (buffer-file-name) default-directory)))
                       (project-read-script project
                                            (lambda (candidate)
                                              (or (string-search "test" (symbol-name (car candidate)))
                                                  (string-search "test" (cdr candidate))))))))
  (project-test (project-current t) (concat command " " file)))

(use-package project
  :pin gnu
  :bind (:map project-prefix-map
              ("C" . project-top-compile)
              ("V" . project-vc-top-dir)
              ("F" . project-find-top-file)
              ("G" . project-find-top-regexp)
              ("r" . project-run-command)
              ("R" . project-run-top-command)
              ("t" . project-run-test))
  :config
  (advice-add 'project-find-regexp :override #'deadgrep))

(with-eval-after-load "project"
  (cl-defgeneric project-scripts (project)
    "Returns an alist of (NAME . COMMAND) scripts that can be run in a project.")

  (cl-defgeneric project-runner (project)
    "Return name of runner executable, e.g. \"make\", \"npm\", etc.")

  (cl-defgeneric project-run (project command)
    "Run COMMAND in PROJECT."
    (let ((runner (project-runner project))
          (default-directory (project-root project)))
      (when (and runner (not (executable-find runner)))
        (error "Runner %S not found." runner))
      (when (and (not runner) (not (executable-find command)))
        (error "Command %S not found." command))
      (async-shell-command (if runner
                               (format "%s %s" runner command)
                             command))))

  (cl-defgeneric project-test (project command)
    "Test FILE in PROJECT."
    (error "Not implemented.")))

(with-eval-after-load "project"
  (defun project-prefixed-buffer-name (mode)
    (concat "*"
            (project-name (project-current))
            " "
            (downcase mode)
            "*")))

(defun project-set-shell-command-buffer-name-async (fn &rest args)
  (let ((shell-command-buffer-name-async
         (format "*%s async command*" (project-name (project-current t)))))
    (apply fn args)))

(with-eval-after-load "project"
  (advice-add 'project-async-shell-command :around #'project-set-shell-command-buffer-name-async))

(cl-defmethod project-root (project)
  "Default implementation."
  (cdr project))

;; .envrc project
(with-eval-after-load "project"
  (defun project-envrc-project (dir)
    (when-let* ((root (locate-dominating-file dir ".envrc")))
      (cons 'envrc root)))

  (add-hook 'project-find-functions #'project-envrc-project))

;; dir-locals project
(with-eval-after-load "project"
  (defun project-dir-locals-project (dir)
    (when-let* ((root (locate-dominating-file dir ".dir-locals.el")))
      (cons 'dir-locals root)))

  (add-hook 'project-find-functions #'project-dir-locals-project))

;; bazel project
(with-eval-after-load "project"
  (defun project-bazel-project (dir)
    (when-let* ((root (locate-dominating-file dir "BUILD.bazel")))
      (cons 'bazel root)))

  (add-hook 'project-find-functions #'project-bazel-project))

;; Makefile project
(with-eval-after-load "project"
  (defun project-makefile-project (dir)
    (when-let* ((root (locate-dominating-file dir "Makefile")))
      (cons 'makefile root)))

  (cl-defmethod project-runner ((project (head makefile))) "make")

  (cl-defmethod project-scripts ((project (head makefile)))
    (with-temp-buffer
      (insert (file-contents-string
               (expand-file-name "Makefile" (project-root project))))
      (goto-char 0)
      (collecting scripts
        (while (search-forward-regexp "^\\([a-z0-9]+\\):" nil t)
          (scripts (match-string 1))))))

  (add-hook 'project-find-functions #'project-makefile-project))

;; NPM project
(with-eval-after-load "project"
  (defvar project-npm--package-cache (make-hash-table :test 'equal))

  (defun project-npm--package-cache-get (dir)
    (let* ((package-json (expand-file-name "package.json" dir))
           (mod-time (file-attribute-modification-time (file-attributes package-json)))
           (cached (gethash dir project-npm--package-cache))
           (cached-time (alist-get 'time cached)))
      (unless (and cached cached-time (time-less-p mod-time cached-time))
        (setq cached `((value . ,(json-read-file package-json))
                       (time . ,(current-time))))
        (puthash dir cached project-npm--package-cache))
      (alist-get 'value cached)))

  (defun project-npm--package-name (dir)
    (alist-get 'name (project-npm--package-cache-get dir)))

  (defun project-npm-project (dir)
    (when-let* ((suffix "")
                (root (project-locate-dominating-file dir "package.json")))
      ;; .git is a regular file in a worktree
      (when (file-regular-p (expand-file-name ".git" (locate-dominating-file default-directory ".git")))
        (setq suffix (concat " " (car (vc-git-branches)))))
      (list 'npm
            (concat (project-npm--package-name root) suffix)
            root)))

  (cl-defmethod project-scripts ((project (head npm)))
    (alist-get 'scripts (project-npm--package-cache-get (project-root project))))

  (cl-defmethod project-run ((project (head npm)) command)
    (let* ((name (project-name project))
           (scripts (project-scripts project))
           (shell-command-buffer-name-async (format "*%s %s*" name command))
           (default-directory (project-root project)))
      (async-shell-command (if (assq (intern command) scripts)
                               (format "npm run %s" command)
                             command))))

  (cl-defmethod project-test ((project (head npm)) command)
    (let* ((name (project-name project))
           (default-directory (project-root project))
           (compilation-buffer-name-function
            (lambda (&rest ignore)
              (concat "*" name " " command "*"))))
      (compile (concat "yarn " command))))

  (cl-defmethod project-name ((project (head npm)))
    (cadr project))

  (cl-defmethod project-roots ((project (head npm)))
    (last project))

  (add-hook 'project-find-functions #'project-npm-project))

(provide 'setup-project)
