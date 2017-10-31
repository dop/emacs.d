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

(defun eproject-setup-exec-path ()
  (if-let ((path (eproject-attribute :path))
           (root (eproject-root)))
      (let ((abs-path (concat root path)))
        (unless (-contains? exec-path abs-path)
          (setq-local exec-path (cons abs-path exec-path)))
      )))

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

(defun dp/eproject-shell-command-with-path (buffer-name command path &optional dir)
  (setq path (mapconcat (curry #'concat (eproject-root))
                        (when (stringp path) (setq path (list path)))
                        path-separator))
  (let* ((current-paths (mapcar (rcurry #'substring 5)
                                (remove-if-not (curry #'string-match "^PATH=")
                                               process-environment)))
         (path-variable (concat "PATH=" path path-separator (mapconcat 'identity current-paths path-separator)))
         (process-environment (cons path-variable process-environment)))
    (if command
        (progn
          (when dir
            (shell-with-command (format "cd \"%s\"" dir) buffer-name))
          (shell-with-command command buffer-name))
      (shell-with-name buffer-name))))

(defun dp/eproject-shell-command (command &optional buffer-name dir)
  (let* ((name (or buffer-name (format "*%s <%s>*" (eproject-name) command)))
         (process (get-buffer-process name)))
    (if process
        (progn
          (unless (get-buffer-window name) (switch-to-buffer name))
          (comint-send-string process (concat command "\n")))
      (dp/eproject-shell-command-with-path name command (eproject-attribute :path) dir))))

(defalias 'pshell 'dp/eproject-shell-command)

(defun dp/js2-add-globals (env &optional globals)
  (when (assoc 'node env)
    (setq globals (append globals dp/js-node-globals)))
  (when (assoc 'browser env)
    (setq globals (append globals dp/js-browser-globals)))
  (when (assoc 'mocha env)
    (setq globals (append globals dp/js-mocha-globals)))
  (when (assoc 'jasmine env)
    (setq globals (append globals dp/js-jasmine-globals)))
  (when (assoc 'jest env)
    (setq globals (append globals dp/js-jest-globals)))
  (set-variable 'js2-additional-externs (mapcar 'symbol-name globals))
  (when (and globals (eq major-mode 'js2-mode))
    (js2-reparse t))
  globals)

(defun eproject-jshint ()
  (if-let ((jshintrc-file (dp/search-up-for ".jshintrc"))
           (jshint (eproject-attribute :jshint)))
      (progn
        (set-variable (make-local-variable 'flycheck-javascript-jshint-executable)
                      (concat (eproject-root) jshint))
        (set-variable (make-local-variable 'flycheck-jshintrc) jshintrc-file)
        (when (and flycheck-jshintrc (or (eq major-mode 'js2-mode)
                                         (eq major-mode 'js2-jsx-mode)))
          (let* ((json (json-read-file flycheck-jshintrc))
                 (globals (mapcar 'car (cdr (assoc 'globals json)))))
            (dp/js2-add-globals json globals))))
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)))

(defun eproject-eslint ()
  (if-let ((eslintrc-file (or (if-let ((custom-eslintrc (eproject-attribute :eslintrc)))
                                  (concat (eproject-root) custom-eslintrc))
                              (dp/search-up-for ".eslintrc")
                              (dp/search-up-for ".eslintrc.json")))
           (eslint (eproject-attribute :eslint)))
      (progn
        (setq-local flycheck-javascript-eslint-executable
                    (concat (eproject-root) eslint))
        (setq-local flycheck-eslintrc eslintrc-file)
        (let* ((json (unless (ignore-errors (json-read-file flycheck-eslintrc))
                       (message "Error reading eslint config file %s" flycheck-eslintrc)
                       nil))
               (globals (mapcar 'car (cdr (assoc 'globals json))))
               (env (assoc 'env json)))
          (dp/js2-add-globals env globals)))
    (add-to-list 'flycheck-disabled-checkers 'javascript-eslint)
    (when (not eslintrc-file)
      (message "Disabling eslint, cannot find .eslintrc file."))
    (when (not eslint)
      (message "Disabling eslint, cannot find eslint executable."))))

(defun eproject-flowtype ()
  (if-let ((flowconfig (dp/search-up-for ".flowconfig"))
           (flow-bin (eproject-attribute :flow)))
      (set-variable (make-local-variable 'flycheck-javascript-flow-executable)
                    (concat (eproject-root) flow-bin))
    (add-to-list 'flycheck-disabled-checkers 'javascript-flow)))

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

(defun dp/eproject-run-test-file ()
  (interactive)
  (let ((mocha (eproject-attribute :mocha))
        (jest  (eproject-attribute :jest)))
    (cond
     (mocha (dp/run-test-in 'mocha mocha))
     (jest (dp/run-test-in 'mocha jest)))))

(defun dp/run-test-in (runner command-list)
  (pcase-let ((`(,command . ,args) command-list))
    (let ((file-name (buffer-file-name)))
      (dp/eproject-shell-command
       (format "%s %s %s" command (s-join " " args) file-name)
       (format "*%s %s*" (eproject-attribute :name) (symbol-name runner))
       (eproject-root)))))

(defun dp/trace (label value)
  "Echo LABEL and VALUE to *Messages* buffer and return VALUE."
  (message (format "%s: %S" label value)) value)

(defun eproject-set-git-generic-keys ()
  (local-set-key (kbd "C-x f") 'dp/git-find-file))

(provide 'dp-eproject)
