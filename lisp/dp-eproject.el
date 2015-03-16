;;; Eproject
(require 'eproject)
(require 'eproject-tasks)
(require 'eproject-extras)
(require 'json)

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

(defun dp/eproject-shell-command (command)
  (let* ((name (format "*%s <%s>*" (eproject-name) command))
         (process (get-buffer-process name)))
    (if process
        (progn
          (message (format "Command '%s' is already running." command))
          (switch-to-buffer name))
      (let* ((path (eproject-attribute :path))
             (new-paths (mapconcat (lambda (p) (concat (eproject-root) p)) (if (listp path) path (list path)) ":"))
             (current-paths (mapcar (lambda (s) (substring s 5))
                                    (remove-if-not (lambda (var) (string-match "^PATH=" var))
                                                   process-environment)))
             (path-variable (concat "PATH=" new-paths ":" (mapconcat 'identity current-paths ":")))
             (process-environment (cons path-variable process-environment)))
        (shell-command command name)))))

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
        (when (assoc 'jasmine json)
          (setq globals (append globals dp/jshint-jasmine-globals)))
        (set-variable 'js2-additional-externs
                      (mapcar 'symbol-name globals))
        (if (< 0 (length globals))
            (js2-reparse t))))))

(provide 'dp-eproject)
