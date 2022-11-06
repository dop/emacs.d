;; -*- lexical-binding: t; -*-

(require 'cl-seq)

(defmacro comment (&rest body))

(defun alphanumeric-char-p (c)
  (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9)))

(defun random-alphanumeric-string (&optional size)
  "Return random string consisting of alphanumeric characters.

Encodes bytes from /dev/random using `base64-encode-string' and
returns at SIZE length string consisting of alphanumeric
characters."
  (let* ((size (or size 12))
	 (command (format "head -c %d /dev/random" (+ 2 size)))
	 (base64 (base64-encode-string
		  (encode-coding-string (shell-command-to-string command)
					'latin-1))))
    (substring (apply #'string (seq-filter #'alphanumeric-char-p base64))
	       0 size)))

(ert-deftest random-alphanumeric-string ()
  "Tests basics of `random-alphanumeric-string'."
  (should (eq 'string (type-of (random-alphanumeric-string))))
  (should (= 12 (length (random-alphanumeric-string))))
  (should (= 5 (length (random-alphanumeric-string 5)))))

(defun insert-random-string ()
  "Insert random alphanumeric string."
  (interactive)
  (insert (random-alphanumeric-string)))

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

(defalias 'rename-buffer-and-file #'rename-file-and-buffer)

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

(defun kill-region-or (command)
  "Interactively call `kill-region', if one is active, or COMMAND."
  (call-interactively
   (if (use-region-p) #'kill-region command)))

(defun kill-region-or-line ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `kill-line'."
  (interactive)
  (kill-region-or #'kill-line))

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (kill-region-or #'backward-kill-word))

(defun ediff-last-2-yanks ()
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
                  (when (functionp 'winner-undo)
                    (winner-undo))))
      (add-hook 'ediff-cleanup-hook #'clean-up)
      (with-current-buffer a
        (insert (elt kill-ring 0)))
      (with-current-buffer b
        (insert (elt kill-ring 1)))
      (ediff-buffers a b))))

(defun open-terminal-app ()
  (interactive)
  (let* ((project (project-current))
         (dir (if project (project-root project) default-directory)))
    (shell-command (format "open -a Terminal.app %s" dir))))

(defun spec-overview ()
  (interactive)
  (occur "\\<\\(x\\|f\\)?\\(describe\\|it\\)\\(\\.\\(skip\\|only\\|todo\\|each\\)\\)?\\>("))

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

(defun advice-list (symbol)
  (let ((list))
    (advice-mapc (lambda (&rest args) (push args x)) symbol)
    list))

(defun project-compile-file (file)
  (interactive (list (or (buffer-file-name) (read-file-name "File: "))))
  (let ((compile-command (concat compile-command file)))
    (call-interactively #'project-compile)))

(defmacro with-project-buffers (options &rest body)
  (declare (indent 1))
  (let ((filter (gensym "filter"))
        (buf (gensym "buf"))
        (bufs (gensym "bufs"))
        (total (gensym "total"))
        (i (gensym "i")))
    `(let* ((,bufs (project-buffers (project-current t)))
            (,total (length ,bufs))
            (,i 0))
       (dolist (,buf ,bufs)
         ,(when (plist-get options :progress)
            `(progn
               (setq ,i (1+ ,i))
               (when (or (zerop (mod ,i 10))
                         (= ,i ,total))
                 (message "%d/%d" ,i ,total))))
         (let ((,filter (or ,(plist-get options :filter) #'buffer-file-name)))
           (when (funcall ,filter ,buf)
             (with-current-buffer ,buf
               ,@body)))))))

(defun project-save-buffers ()
  "Save all project buffers visiting a file."
  (interactive)
  (with-project-buffers ()
    (save-buffer)))

(defun project-revert-buffers ()
  "Revert all project buffers visiting a file."
  (interactive)
  (with-project-buffers (:progress t)
    (ignore-errors
      (revert-buffer nil t))))

(defun project-close-dead-process-buffers ()
  "Close all buffers that have dead process attached."
  (interactive)
  (let ((dead-count 0) (eslint-count 0))
    (cl-flet ((flymake-eslint-buffer-p (buf) (s-starts-with-p " *flymake-eslint*" (buffer-name buf))))
      (with-project-buffers (:filter #'get-buffer-process)
        (unless (process-live-p (get-buffer-process (current-buffer)))
          (kill-buffer)
          (cl-incf dead-count)))
      (with-project-buffers (:filter #'flymake-eslint-buffer-p)
        (kill-buffer)
        (cl-incf eslint-count))
      (message "Dead process buffers: %d. Hanging eslint buffers: %d." dead-count eslint-count))))

(defun open-dictionary-app (text)
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Word or phrase: " (word-at-point t)))))
  (shell-command (concat "open dict://" (url-encode-url text))))

(defun add-hooks (hook functions)
  "Add multiple FUNCTIONS to a HOOK.

Every item of FUNCTIONS can be either function or arguments to
`add-hook' sans HOOK symbol itself."
  (dolist (fn functions)
    (apply #'add-hook hook (if (consp fn) fn (list fn)))))

;; (defun diff-hunk-create-new-file ()
;;   (let ((old (substring-no-properties (car (diff-hunk-file-names t)))))
;;     (when (string= old "/dev/null")
;;       (let ((new (substring-no-properties (car (diff-hunk-file-names)))))
;;         (cons new (file-exists-p new))))))

(defun markdown-live-preview-window-browser (file)
  "Preview FILE with browser.
To be used with `markdown-live-preview-window-function'."
  (browse-url file))

(setq markdown-live-preview-window-function 'markdown-live-preview-window-browser)

(defvar my-eshell-prompt-limit 20)

(defun get-project-name-and-branch (project)
  (pcase project
    (`(,type . ,path)
     (list
      (cond ((eq type 'vc)
             (elt (reverse (split-string path "/")) 1))
            ((eq type 'npm)
             (let-alist (json-read-file (expand-file-name "package.json" path))
               .name)))
      (car (let ((default-directory path))
             (vc-git-branches)))))))

(defun shorten-path (path limit prefix)
  (let ((limited nil))
    (cl-loop for part in (reverse (split-string path "/" t))
             while (< (seq-reduce #'+ (mapcar #'length limited) 0) limit)
             do (push part limited))
    (let ((trimmed (mapconcat #'identity limited "/")))
      (concat (if (/= (length trimmed) (length path)) prefix "")
              trimmed))))

(defun my-eshell-prompt ()
  (let ((path (abbreviate-file-name (eshell/pwd)))
        (limit my-eshell-prompt-limit)
        (limited nil)
        (project (project-current)))
    (with-output-to-string
      (princ (shorten-path path my-eshell-prompt-limit "…"))
      (if (= (user-uid) 0)
          (princ " # ")
        (princ " $ ")))))

(defun overlay-get-eslint-message (overlay)
  "Get text message from OVERLAY if it is from `flymake-eslint--checker' backend."
  (when-let ((diagnostics (and (overlayp overlay)
                               (overlay-get overlay 'flymake-diagnostic))))
    (when (eq (flymake--diag-backend diagnostics) 'flymake-eslint--checker)
      (flymake--diag-text diagnostics))))

(ert-deftest overlay-get-eslint-message ()
  "Tests basics of `overlay-get-eslint-message'."
  (should (null (overlay-get-eslint-message nil)))
  (should (null (overlay-get-eslint-message [])))
  (should (null (overlay-get-eslint-message (make-overlay 0 0)))))

(defun eslint-message-get-rule (text)
  "Extract rule name from eslint message."
  (string-trim (car (s-match "[[0-9a-z/-]+]$" text)) "\\[" "\\]"))

(ert-deftest eslint-message-get-rule ()
  "Tests basics of `eslint-message-get-rule'."
  (should (string= "react-hooks/exhaustive-deps"
                   (eslint-message-get-rule "warning: React Hook useEffect has a missing dependency: 'init'. Either include it or remove the dependency array [react-hooks/exhaustive-deps]")))
  (should (string= "jsx-a11y/anchor-has-content"
                   (eslint-message-get-rule "Anchor must have content [jsx-a11y/anchor-has-content]"))))

(defun ignore-eslint-rules ()
  "Insert eslint-disable-next-line rule pragma for overlay warning on current point."
  (interactive)
  (when-let ((rules (cl-loop for msg in (mapcar #'overlay-get-eslint-message (overlays-at (point)))
                        when msg
                        collect (eslint-message-get-rule msg))))
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    (call-interactively #'comment-dwim)
    (insert "eslint-disable-next-line " (mapconcat #'identity rules " "))))

(defun duplicate-line (&optional n)
  "Duplicate the current line N times.
Also see the `copy-from-above-command' command."
  (interactive "p")
  (let ((line (buffer-substring (line-beginning-position)
                                (line-end-position))))
    (save-excursion
      (forward-line 1)
      (unless (bolp)
        (insert "\n"))
      (dotimes (_ n)
        (insert line "\n")))))

(provide 'misc)
