;; -*- lexical-binding: t -*-

(require 'cl)
(require 'dash)
(require 'url)

(defmacro comment (&rest body)
  nil)

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun json-drill (path json)
  "Get value deep inside json. PATH is a list of names and JSON
is alist returned from `json-read' for example."
  (cl-reduce (lambda (alist key) (assocdr key alist)) path :initial-value json))

(defun assoc-set (key value list)
  (append `((,key . ,value)) (assq-delete-all key list)))

(defun assocdr (key list)
  (cdr (assoc key list)))

(defun urlencode (params)
  (let ((pairs (loop for (name . value) in params
                     collect (format "%s=%s" (url-hexify-string name) (url-hexify-string value)))))
    (mapconcat 'identity pairs "&")))

(defun dp/get-org-table-bounds ()
  (let (start end)
    (save-mark-and-excursion
      (beginning-of-line)
      (while (and (looking-at "|") (= 0 (forward-line -1))))
      (when (not (looking-at "|")) (forward-line))
      (setq start (point))
      (while (and (looking-at "|") (= 0 (forward-line))))
      (when (looking-at "|") (goto (end-of-buffer)))
      (setq end (point))
      (when (= start end)
        (error "No org table in sight!"))
      (cons start end))))

(defun mark-org-table ()
  (interactive)
  (destructuring-bind (start . end) (dp/get-org-table-bounds)
    (set-mark end)
    (goto-char start)))

(defun dp/with-region-or (get-start-end fn)
  "Invoke FN on region if it is selected or on (START . END) pair
returned from GET-START-END.

FN should accept two arguments START and END."
  (destructuring-bind (beg . end)
      (if (use-region-p)
          (cons (region-beginning) (region-end))
        (funcall get-start-end))
      (funcall fn beg end)))

(defun dp/with-start-end (fn)
  "Invoke FN with beginning and end of current region or min and
max points of current buffer if there is no selected region."
  (dp/with-region-or (lambda () (cons (point-min) (point-max)))
                     fn))

(defun dp/remove-text-properties-dwim ()
  (interactive)
  (dp/with-start-end
   (lambda (start end) (set-text-properties start end nil))))

(defun dp/with-region-or-symbol-at-point (convert)
  "Invoke CONVERT on string of region or symbol at point."
  (dp/with-region-or
   (lambda () (bounds-of-thing-at-point 'symbol))
   (lambda (start end)
       (let ((name (buffer-substring beg end)))
         (save-excursion
           (delete-region beg end)
           (goto-char beg)
           (insert (funcall convert name)))))))

(defun eval-dwim (&optional start end eval-last-sexp-arg-internal)
  "Replace the preceding sexp with its value."
  (interactive "r\nP")
  (let ((expr (if (and start end)
                  (kill-region start end)
                (backward-kill-sexp))))
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))

(global-set-key (kbd "C-x C-e") 'eval-last-sexp)

(defun color-name-to-rgb-255 (name)
  (mapcar (compose #'round (curry #'* 255))
          (color-name-to-rgb name)))

(defun color-rgba-to-hex (r g b &optional a)
  (concat (format "#%02X%02X%02X" r g b)
          (if a (format "%02X" a) "")))

(defvar dp/custom-user-faces nil
  "Associated list of (FACE . SPEC) to apply to user theme")

(defun dp/apply-custom-user-faces ()
  (let (custom--inhibit-theme-enable)
    (apply
     #'custom-theme-set-faces
     'user
     (loop for (face . specs) in dp/custom-user-faces
           collect (list face (list (list t specs)))))))

(defun dp/reset-custom-user-faces ()
  (let (custom--inhibit-theme-enable)
    (loop for (face . _) in dp/custom-user-faces
          do (ignore-errors
               (face-spec-reset-face face)))))

(defun disable-all-themes ()
  ;; (dp/reset-custom-user-faces)
  (dolist (enabled-theme custom-enabled-themes)
    (disable-theme enabled-theme)))

(defun toggle-theme (theme)
  "Enables or disables a THEME."
  (if (custom-theme-enabled-p theme)
      (disable-theme theme)
    (load-theme theme t)))

(defun switch-to-theme (theme &rest custom-faces)
  (declare (indent 1))
  "Enables THEME and disable all others."
  (interactive
   (list
    (intern (completing-read "Load theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))))
  (disable-all-themes)
  (load-theme theme t)
  (when custom-faces
    (apply #'custom-theme-set-faces theme custom-faces)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun set-cursor-according-to-mode (&optional type)
  (let ((next-cursor-type
         (cond
          (type)
          (buffer-read-only 'hbar)
          (t '(bar . 2)))))
    (unless (equal next-cursor-type cursor-type)
      (setq cursor-type next-cursor-type))))

(defun download-to-current-buffer (url)
  "Downloads URL into current buffer."
  (interactive "sURL: ")
  (download-to-buffer url (current-buffer) nil))

(defun download-to-buffer (url buf &optional with-headers)
  "Downloads URL into buffer BUF."
  (let ((data (url-retrieve-synchronously url)))
    (if data
        (with-current-buffer buf
          (unless with-headers
            (with-current-buffer data
              (goto-char (point-min))
              (kill-region (point-min) (progn (forward-paragraph 1) (point)))
              (kill-line)))
          (save-excursion
            (insert-buffer-substring data))))))

(defun dp/zap-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
                         (search-forward (char-to-string char) nil nil arg)
                         (backward-char)
                         (point))))

(defun dp/terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun dp/camelcase-name-split (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun dp/camelcase-to-constant (s)
  (mapconcat 'upcase (dp/camelcase-name-split s) "_"))

(defun dp/constant-to-camelcase (s)
  (let ((parts (split-string s "_")))
    (concat
     (downcase (car parts))
     (mapconcat 'capitalize (cdr parts) nil))))

(defun dp/convert-camelcase-to-constant ()
  (interactive)
  (dp/with-region-or-symbol-at-point #'dp/camelcase-to-constant))

(defun dp/convert-constant-to-camelcase ()
  (interactive)
  (dp/with-region-or-symbol-at-point #'dp/constant-to-camelcase))

(defvar dp/js-node-globals
  (list 'global 'require 'module 'exports))

(defvar dp/js-browser-globals
  (list 'document 'window 'location))

(defvar dp/js-mocha-globals
  (list 'describe 'it 'after 'before 'afterEach 'beforeEach
        ))

(defvar dp/js-jasmine-globals
  (list 'describe 'ddescribe 'xdescribe
        'afterEach 'beforeEach 'afterAll 'beforeAll
        'it 'iit 'xit
        'expect 'spyOn
        'jasmine
        ))

(defvar dp/js-jest-globals
  (list 'describe 'fdescribe 'xdescribe
        'afterEach 'beforeEach 'afterAll 'beforeAll
        'it 'fit 'xit
        'expect
        'jest
        ))

(defun dp/toggle-ddescribe ()
  (interactive)
  (save-excursion
    (search-backward-regexp "\\_<d?describe(" nil t)
    (case (symbol-at-point)
      ('describe (insert "d"))
      ('ddescribe (delete-forward-char 1)))))

(defun dp/toggle-iit ()
  (interactive)
  (save-excursion
    (search-backward-regexp "\\_<i?it(" nil t)
    (case (symbol-at-point)
      ('it (insert "i"))
      ('iit (delete-forward-char 1)))))


(defun dp/clean-json (start end)
  "Change a region of raw JSON values to JavaScript objects by
remove double quotes of properties and replacing double with
single quotes."
  (interactive "r")
  (json-mode-beautify)
  (replace-regexp "\"\\([^\"]+\\)\" ?:" "\\1:" nil start end)
  (replace-regexp "\"" "'" nil start end))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

;;; color these functions like keywords
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(compose\\)[ \t\n\r]" 1 font-lock-keyword-face)
   ("(\\(curry\\)[ \t\n\r]" 1 font-lock-keyword-face)
   ("(\\(rcurry\\)[ \t\n\r]" 1 font-lock-keyword-face)))

(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.  If region is
active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(defun dp/set-env-path (value)
  (let* ((dirs (mapcar #'directory-file-name (remove-duplicates (parse-colon-path value) :test #'equal)))
         (path (s-join path-separator dirs)))
    (message "PATH = %s" path)
    (setenv "PATH" path)
    (setq eshell-path-env path
          exec-path (append dirs (list exec-directory)))))

(defun dp/parse-env (text)
  (->> (s-split "\n" text)
       (-filter (curry #'s-matches-p "="))
       (-map (curry #'s-split "="))))

(defun dp/update-env (env)
  (loop for (name value) in env
        do (if (string-equal "PATH" name)
               (dp/set-env-path value)
             (message "%s = %s" name value)
             (setenv name value))))

(defun dp/with-env (command fn)
  (async-start (lambda () (shell-command-to-string command))
               fn))

(defun dp/nvm-use (version)
  (let ((bufname (buffer-name (current-buffer))))
    (dp/with-env (concat ". ~/.profile; nvm use " (format "%s" version) "; env")
                 (lambda (result)
                   (let ((values (dp/parse-env result)))
                     (dp/update-env values)
                     (let ((path (getenv "PATH")))
                       (with-current-buffer (get-buffer bufname)
                         (dp/update-env values))))
                   (message (shell-command-to-string "node -v"))))))

(defalias 'eshell/nvm-use 'dp/nvm-use)

(defun dp/update-environment ()
  (dp/with-env ". ~/.profile; env"
               (lambda (result)
                 (->> (dp/parse-env result) (dp/update-env))
                 (message "Updated environment variables."))))

(defun dp/eshell-aliases-from-shell ()
  (async-start
   (lambda () (shell-command-to-string ". ~/.profile; alias"))
   (lambda (result)
     (->> (s-split "\n" result)
          (-filter (curry #'s-matches-p "="))
          (-map (curry #'s-split "="))
          (-map (pcase-lambda (`(,name ,value))
                  (let ((normalized-value (replace-regexp-in-string "\\(^'\\|'$\\)" "" value)))
                    (add-to-list 'eshell-command-aliases-list
                                 (list name value))))))
     (message "Updated EShell aliases."))))

(defun dp/diff-last-2-yanks ()
  "Run ediff on latest two entries in `kill-ring'."
  (interactive)
  ;; Implementation depends on `lexical-binding' being t, otherwise #'clean-up
  ;; will not be saved as closure to `ediff-cleanup-hook' and thus will lose
  ;; reference to itself.
  (let ((a (generate-new-buffer "*diff-yank*"))
        (b (generate-new-buffer "*diff-yank*")))
    (cl-labels ((clean-up ()
                          (kill-buffer a)
                          (kill-buffer b)
                          (remove-hook 'ediff-cleanup-hook #'clean-up)
                          (winner-undo)))
      (add-hook 'ediff-cleanup-hook #'clean-up)
      (with-current-buffer a
        (insert (elt kill-ring 0)))
      (with-current-buffer b
        (insert (elt kill-ring 1)))
      (ediff-buffers a b))))

(defvar tsfmt-executable "tsfmt")

(defun tsfmt-region (start end)
  (let ((name (buffer-file-name))
        (saved-point (point)))
    (call-process-region
     start end tsfmt-executable
     t t nil
     "--stdin"
     "--baseDir" (locate-dominating-file name "tsfmt.json")
     name)
    (goto-char saved-point)))

(defun tsfmt ()
  (interactive)
  (pcase-let ((`(,start . ,end)
               (if (use-region-p)
                   (cons (region-beginning) (region-end))
                 (cons (point-min) (point-max)))))
    (tsfmt-region start end)))

(defun dp/tide-with-inferred-type (fn)
  (tide-command:quickinfo
   (tide-on-response-success-callback response (:ignore-empty t)
     (funcall fn (tide-doc-text (plist-get response :body))))))

(defun dp/tide-yank-inferred-type ()
  (interactive)
  (dp/tide-with-inferred-type
   (lambda (text)
     (kill-new text)
     (message text))))

(defun dp/tide-insert-inferred-type ()
  (interactive)
  (dp/tide-with-inferred-type
   (lambda (text)
     (save-excursion
       (beginning-of-line)
       (newline-and-indent)
       (previous-line)
       (let ((start (point))
             (commented-text
              (with-temp-buffer
                (setq comment-start "//")
                (insert text)
                (comment-region (point-min) (point-max))
                (buffer-string))))
         (insert commented-text)
         (indent-region start (+ start (length commented-text))))))))

(defun dp/insert-linter-disable-next-line ()
  (interactive)
  (let* ((errors (flycheck-overlay-errors-at (point)))
         (typescript-p (eq 'typescript-tslint (flycheck-error-checker (first errors)))))
    (back-to-indentation)
    (open-line 1)
    (insert (format (looking-at-p "" "// %s %s"
                    (if typescript-p
                        "tslint:disable-next-line"
                      "eslint-disable-next-line")
                    (s-join " " (mapcar #'flycheck-error-id errors))))
    (indent-for-tab-command)
    (next-line)
    (indent-for-tab-command))))

(defun projectile-magit-status ()
  (interactive)
  (let ((git-root (magit-toplevel))
        (project-root (projectile-project-root)))
    (if (string-equal git-root project-root)
        (magit-status)
      (let ((git-directory-name (file-name-base (directory-file-name git-root)))
            (project-subdir (directory-file-name (subseq project-root (length git-root)))))
        (with-current-buffer
            (magit-setup-buffer #'magit-status-mode nil
              (magit-buffer-diff-files (list project-subdir))
              (magit-buffer-log-files (list project-subdir))
              (magit-section-show-child-count nil))
          (rename-buffer (concat "magit: " git-directory-name "/" project-subdir)))))))

(defun random-password (&optional size)
  (remove-if-not
   (lambda (c) (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9)))
   (shell-command-to-string (format "head -c %d /dev/random | base64" (or size 12)))))

(defun insert-random-password ()
  (interactive)
  (insert (random-password)))

;; TODO does not work well, messes up buffer.
(defun eshell/clear-buffer ()
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(defun dp/projectile-find-root-with-prettier ()
  (let* ((bin/pretter "node_modules/.bin/prettier")
         (root (locate-dominating-file (projectile-project-root)
                                      (lambda (dir)
                                        (file-exists-p (concat dir bin/pretter))))))
    (when root
      (concat root bin/pretter))))

(defun dp/spec-overview ()
  (interactive)
  (helm-swoop :query "\\<\\(x\\|f\\)?\\(describe\\|it\\)\\(\\.\\(skip\\|only\\|todo\\)\\)?\\>("))

(provide 'dp-functions)
