;; -*- lexical-binding: t -*-

(require 'eproject)
(require 'eproject-tasks)
(require 'eproject-extras)
(require 'json)
(require 's)
(require 'f)
(require 'dash)

(defvar eproject-prefix "C-x p")

(defun eproject-set-key (key command &optional local)
  (let ((set-key (if local #'local-set-key #'global-set-key)))
    (funcall set-key (kbd (format "%s %s" eproject-prefix key)) command)))

(defun eproject-open-shell ()
  (interactive)
  (let ((default-directory (eproject-root))
        (name (eproject-name)))
    (let ((buf (get-buffer-create (concat "*" name " shell*"))))
      (with-current-buffer (shell buf)
        (setq comint-input-ignoredups t)))))

(defun eproject-open-term ()
  (interactive)
  (let* ((default-directory (eproject-root))
         (buf-name (concat "*" (eproject-name) " shell*"))
         (buf      (get-buffer buf-name)))
    (if buf (switch-to-buffer buf)
      (dp/eproject-shell-command-with-path buf-name nil (eproject-attribute :path)))))

(defun eproject-set-local-keys ()
  (mapc (lambda (action)
          ;; use pcase instead of destructuring-bind when there will be other
          ;; types of actions, not only :task
          (destructuring-bind (k _ task) action
            (eproject-set-key k (lexical-let ((task task)) ; because no closures
                                  #'(lambda nil
                                      (interactive)
                                      (dp/eproject-run-task task)))
                              t)))
        (eproject-attribute :keys)))

(defun dp/eproject-run-task (name)
  (eproject-tasks-run
   (car (member-if (lambda (task) (equal name (car task)))
                   (eproject-tasks-get)))))

(defun dp/search-up-for (file-name)
  "Search up file directory tree for a first occurance of file."
  (let ((dir (eproject--scan-parents-for
              (file-name-directory (or (buffer-file-name) default-directory))
              (lambda (dir)
                (file-exists-p (concat (file-name-as-directory dir) file-name))))))
    (if dir
        (concat (file-name-as-directory dir) file-name)
      nil)))

(defun shell-with-name (new-name)
  (let ((buf (get-buffer-create new-name)))
    (shell buf)
    buf))

(defun shell-with-command (command &optional new-name)
  (comint-send-string (get-buffer-process (shell-with-name new-name))
                      (concat command "\n")))

(defun multi-term-with-name (new-name)
  (multi-term)
  (let ((buf (car (last multi-term-buffer-list))))
    (with-current-buffer buf
      (rename-buffer new-name))
    buf))

(defun multi-term-command (command new-name)
  (comint-send-string (multi-term-with-name new-name)
                      (concat command "\n")))

(defun dp/eproject-shell-command-with-path (buffer-name command path)
  (setq path (mapconcat (curry #'concat (eproject-root))
                        (when (stringp path) (setq path (list path)))
                        ":"))
  (let* ((current-paths (mapcar (rcurry #'substring 5)
                                (remove-if-not (curry #'string-match "^PATH=")
                                               process-environment)))
         (path-variable (concat "PATH=" path ":" (mapconcat 'identity current-paths ":")))
         (process-environment (cons path-variable process-environment)))
    (if command
        (shell-with-command command buffer-name)
      (shell-with-name buffer-name))))

(defun dp/eproject-shell-command (command)
  (let* ((name (format "*%s <%s>*" (eproject-name) command))
         (process (get-buffer-process name)))
    (if process
        (progn
          (unless (get-buffer-window name) (switch-to-buffer name))
          (comint-send-string process (concat command "\n")))
      (dp/eproject-shell-command-with-path name command (eproject-attribute :path)))))

(defalias 'pshell 'dp/eproject-shell-command)

(defun eproject-eslint ()
  (let ((eslintrc-file (dp/search-up-for ".eslintrc.json")))
    (when (and eslintrc-file
               (or (eq major-mode 'js2-mode)
                   (eq major-mode 'js2-jsx-mode)))
      (flycheck-mode 1)
      (set-variable 'flycheck-checker 'javascript-eslint)
      (set-variable (make-local-variable 'flycheck-javascript-eslint-executable)
                    (if (eproject-attribute :eslint)
                        (concat (eproject-root) (eproject-attribute :eslint)))
                    "eslint")
      (set-variable (make-local-variable 'flycheck-eslintrc) eslintrc-file)
      (when flycheck-eslintrc
        (let* ((json (json-read-file flycheck-eslintrc))
               (globals (mapcar 'car (cdr (assoc 'globals json))))
               (env (assoc 'env json)))
          (when (assoc 'node env)
            (setq globals (append globals dp/js-node-globals)))
          (when (assoc 'mocha env)
            (setq globals (append globals dp/js-mocha-globals)))
          (when (assoc 'jasmine env)
            (setq globals (append globals dp/js-jasmine-globals)))
          (set-variable 'js2-additional-externs
                        (mapcar 'symbol-name globals))
          (if (< 0 (length globals))
              (js2-reparse t)))))))

(defun eproject-jshint ()
  (let ((jshintrc-file (dp/search-up-for ".jshintrc")))
    (when (and jshintrc-file
               (or (eq major-mode 'js2-mode)
                   (eq major-mode 'js2-jsx-mode)))
      (flycheck-mode 1)
      (set-variable 'flycheck-checker 'javascript-jshint)
      (set-variable (make-local-variable 'flycheck-javascript-jshint-executable)
                    (if (eproject-attribute :jshint)
                        (concat (eproject-root) (eproject-attribute :jshint)))
                    "jshint")
      (set-variable (make-local-variable 'flycheck-jshintrc) jshintrc-file)
      (when flycheck-jshintrc
        (let* ((json (json-read-file flycheck-jshintrc))
               (globals (mapcar 'car (cdr (assoc 'globals json)))))
          (when (assoc 'node json)
            (setq globals (append globals dp/js-node-globals)))
          (when (assoc 'mocha json)
            (setq globals (append globals dp/js-mocha-globals)))
          (when (assoc 'jasmine json)
            (setq globals (append globals dp/js-jasmine-globals)))
          (set-variable 'js2-additional-externs
                        (mapcar 'symbol-name globals))
          (if (< 0 (length globals))
              (js2-reparse t)))))))

(defun dp/git-find-file (&optional directory)
  "Find file in repository using `ido-completing-read'."
  (interactive (list (ignore-errors (file-name-directory (dp/search-up-for ".git")))))
  (find-file
   (concat directory
           (ido-completing-read
            "Find file in repository: "
            (dp/git-find-all-files directory)))))

(defun dp/git-find-all-files (directory &optional fullpath)
  (let ((files (with-temp-buffer
                 (setq default-directory directory)
                 (call-process "git" nil t nil
                               "ls-tree" "-z" "-r" "--name-only" "--full-tree"
                               "HEAD")
                 (s-split "\0" (buffer-string) t))))
    (if fullpath
        (-map (curry 'concat directory) files)
      files)))

(defun dp/git-find-files (regexp directory)
  (-filter (curry 's-match regexp)
           (dp/git-find-all-files directory t)))

(defun ng/find-spec-file (&optional buffer directory)
  (interactive (list
                (current-buffer)
                (ignore-errors (file-name-directory (dp/search-up-for ".git")))))
  (let ((filepath (buffer-file-name buffer))
        spec-filename
        results)
    (if (not filepath)
        (error (format "Current buffer %s is not visiting any file" (buffer-name buffer)))
      (setq spec-filename (concat (file-name-base filepath)
                                  ".spec."
                                  (file-name-extension filepath)))
      (setq results (dp/git-find-files spec-filename directory))
      (if (null results)
          (error (format "Cannot find file %s" spec-filename))
        (find-file (nth 0 results))))
    ))

(defun ng/find-implementation-file (&optional buffer directory)
  (interactive (list
                (current-buffer)
                (ignore-errors (file-name-directory (dp/search-up-for ".git")))))
  (let ((spec-filename (f-filename (buffer-file-name buffer)))
        filename
        results)
    (if (not spec-filename)
        (error (format "Current buffer %s is not visiting any file" (buffer-name buffer)))
      (setq filename (s-replace ".spec." "." spec-filename))
      (setq results (dp/git-find-files filename directory))
      (if (null results)
          (error (format "Cannot find file %s" filename))
        (find-file (nth 0 results))))))

(global-set-key (kbd "C-c t") 'ng/find-spec-file)
(global-set-key (kbd "C-c f") 'ng/find-implementation-file)

(defun dp/trace (label value)
  "Echo LABEL and VALUE to *Messages* buffer and return VALUE."
  (message (format "%s: %S" label value)) value)

(defun eproject-set-git-generic-keys ()
  (local-set-key (kbd "C-x f") 'dp/git-find-file))

(provide 'dp-eproject)
