;;; Eproject
(require 'eproject)
(require 'eproject-tasks)
(require 'eproject-extras)
(require 'json)
(require 's)
(require 'multi-term)

(defvar eproject-prefix "C-x p")

(defun eproject-set-key (key command &optional local)
  (let ((set-key (if local #'local-set-key #'global-set-key)))
    (funcall set-key (kbd (format "%s %s" eproject-prefix key)) command)))

(defun eproject-open-shell ()
  (interactive)
  (let ((default-directory (eproject-root))
        (name (eproject-name)))
    (let ((buf (get-buffer-create (concat "*" name " shell*"))))
      (shell buf))))

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
              (file-name-directory (buffer-file-name))
              (lambda (dir)
                (file-exists-p (concat (file-name-as-directory dir) file-name))))))
    (if dir
        (concat (file-name-as-directory dir) file-name)
      nil)))

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
        (multi-term-command command buffer-name)
      (multi-term-with-name buffer-name))))

(defun dp/eproject-shell-command (command)
  (let* ((name (format "*%s <%s>*" (eproject-name) command))
         (process (get-buffer-process name)))
    (if process
        (progn
          (unless (get-buffer-window name) (switch-to-buffer name))
          (comint-send-string process (concat command "\n")))
      (dp/eproject-shell-command-with-path name command (eproject-attribute :path)))))

(defalias 'pshell 'dp/eproject-shell-command)

(defun eproject-jshint ()
  (when (eq major-mode 'js2-mode)
    (flycheck-mode 1)
    (set-variable 'flycheck-checker 'javascript-jshint)
    (set-variable (make-local-variable 'flycheck-javascript-jshint-executable)
                  (if (eproject-attribute :jshint)
                      (concat (eproject-root) (eproject-attribute :jshint)))
                  "jshint")
    (set-variable (make-local-variable 'flycheck-jshintrc)
                  (dp/search-up-for ".jshintrc"))
    (when flycheck-jshintrc
      (let* ((json (json-read-file flycheck-jshintrc))
             (globals (mapcar 'car (cdr (assoc 'globals json)))))
        (when (assoc 'node json)
          (setq globals (append globals dp/jshint-node-globals)))
        (when (assoc 'mocha json)
          (setq globals (append globals dp/jshint-mocha-globals)))
        (when (assoc 'jasmine json)
          (setq globals (append globals dp/jshint-jasmine-globals)))
        (set-variable 'js2-additional-externs
                      (mapcar 'symbol-name globals))
        (if (< 0 (length globals))
            (js2-reparse t))))))

(defun dp/git-find-file (&optional directory)
  "Find file in repository using `ido-completing-read'."
  (interactive (list (or (ignore-errors (eproject-root)) nil)))
  (with-temp-buffer
    (when directory (setq default-directory directory))
    (call-process "git" nil t nil
                  "ls-tree" "-z" "-r" "--name-only" "--full-tree"
                  "HEAD")
    (find-file
     (concat default-directory
             (ido-completing-read "Find file in repository: "
                                  (s-split "\0" (buffer-string) t))))))

(defun eproject-set-git-generic-keys ()
  (local-set-key (kbd "C-x f") 'dp/git-find-file))

(provide 'dp-eproject)
