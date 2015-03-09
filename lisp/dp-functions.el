(defun set-global-font (font-name &optional keep-size)
  (set-frame-font font-name keep-size t))

(defun toggle-theme (theme)
  "Enables or disables a THEME."
  (if (custom-theme-enabled-p theme)
      (disable-theme theme)
    (load-theme theme t)))

(defun switch-to-theme (theme)
  "Enables THEME and disable all others."
  (dolist (enabled-theme custom-enabled-themes)
    (disable-theme enabled-theme))
  (load-theme theme t))

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

(defun set-cursor-according-to-mode ()
  (cond (buffer-read-only
         (setq cursor-type 'box))
        (overwrite-mode
         (setq cursor-type 'hbar))
        (t
         (setq cursor-type 'bar))))

(defun download-to-current-buffer (url)
  "Downloads URL into current buffer."
  (interactive "sURL: ")
  (download-to-buffer url (current-buffer) nil))

(defun download-to-buffer (url buf &optional with-headers)
  "Downloads URL into buffer buf."
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

(defun dp/with-region-or (beg end region-fn fn)
  (let ((b (or (and mark-active beg) (point-at-bol)))
        (e (or (and mark-active end) (point-at-eol))))
    (if (>= b e)
        (funcall fn)
      (funcall region-fn b e))))

(defun dp/comment-uncomment (&optional beg end)
  (interactive "*r")
  (dp/with-region-or beg end
                     '(lambda (beg end)
                        (if (= end (point))
                            (comment-dwim)
                          (comment-or-uncomment-region beg end)))
                     'comment-dwim))

(defun dp/kill-line-or-region (&optional beg end)
  (interactive "*r")
  (dp/with-region-or beg end
                     'kill-region
                     'kill-whole-line))

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

(defun dp/constant-to-camelcase (s &optional capitalize-first)
  (let ((parts (split-string s "_")))
    (if capitalize-first
        (mapconcat 'capitalize parts nil)
      (concat
       (downcase (car parts))
       (mapconcat 'capitalize (cdr parts) nil)))))

(defun dp/with-region-or-thing-at-point (fn &rest additional-arguments)
  (save-excursion
    (destructuring-bind (beg . end)
        (if (use-region-p)
            (cons (region-beginning) (region-end))
          (bounds-of-thing-at-point 'symbol))
      (let ((name (buffer-substring beg end)))
        (delete-region beg end)
        (goto-char beg)
        (insert (apply fn name additional-arguments))))))

(defun dp/convert-camelcase-to-constant ()
  (interactive)
  (dp/with-region-or-thing-at-point 'dp/camelcase-to-constant))

(defun dp/convert-constant-to-camelcase (&optional arg)
  (interactive "P")
  (dp/with-region-or-thing-at-point 'dp/constant-to-camelcase arg))

(defvar dp/jshint-node-globals
  (list 'global 'require 'module 'exports))

(defvar dp/jshint-browser-globals
  (list 'global 'document 'window))

(defvar dp/jshint-jasmine-globals
  (list 'describe 'ddescribe 'xdescribe
        'afterEach 'beforeEach
        'it 'iit 'xit
        'expect
        'jasmine))

(provide 'dp-functions)
