;; -*- lexical-binding: t; -*-

(require 'cl-seq)
(require 'my-library)

(defun insert-random-string (size)
  "Insert random alphanumeric string."
  (interactive "p")
  (insert (random-alphanumeric-string (if (> size 1) size nil))))

(defalias 'rename-buffer-and-file #'rename-visited-file)
(defalias 'rename-file-and-buffer #'rename-visited-file)

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
      (when-let ((curr (car (vc-git-branches))))
        (princ ":")
        (princ curr))
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

(defun dark-mode ()
  "This seems to be good enough."
  (interactive)
  (set-face-attribute 'default nil
                      :foreground (face-attribute 'default :background)
                      :background (face-attribute 'default :foreground)))

;; TODO I think this does not work well, somehow `nvm-use' is not
;; setting `exec-path' right.
(defun eshell/nvm-use (&optional version)
  (if-let ((version-string
            (or (and version (cond ((stringp version) version)
                                   ((numberp version) (number-to-string version))
                                   (t (warn "%s is not a string or number." version) nil)))
                (if-let ((nvmrc-directory
                          (locate-dominating-file "." ".nvmrc")))
                    (with-temp-buffer
                      (insert-file-contents-literally (expand-file-name ".nvmrc" nvmrc-directory))
                      (string-trim (buffer-string)))
                  (warn "Cannot find .nvmrc.")))))
      (nvm-use version-string
               (lambda ()
                 (message "%s" (getenv "PATH"))
                 (message "Switched to node %s." nvm-current-version)))
    (message "Provide a version.")))

(provide 'my-commands)
