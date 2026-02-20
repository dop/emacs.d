;; -*- lexical-binding: t; -*-

(require 'cl-seq)
(require 'my-library)

(defun insert-random-string (size)
  "Insert random alphanumeric string."
  (interactive "p")
  (insert (random-alphanumeric-string (if (and size (plusp size)) size nil))))

(defun copy-random-string (size)
  "Put random alphanumeric string into a kill-ring."
  (interactive "p")
  (kill-new (random-alphanumeric-string (if (and size (plusp size)) size nil))))

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
         (dir (expand-file-name (if project (project-root project) default-directory))))
    (shell-command (format "open -a Terminal.app '%s'" dir))))

(defun spec-overview ()
  (interactive)
  (occur "\\<\\(x\\|f\\)?\\(describe\\|it\\|test\\)\\(\\.\\(skip\\|only\\|todo\\|each\\)\\)?\\>("))

(defun download-call-back (status callback with-headers)
  (unless with-headers
    (goto-char (point-min))
    (kill-region (point-min) (progn (forward-paragraph 1) (point)))
    (kill-line))
  (funcall callback (buffer-string)))

(defun download-as-string (url callback &optional with-headers)
  "Downloads URL and call CALLBACK with result string."
  (url-retrieve url #'download-call-back (list callback with-headers)))

(defun download-to-buffer (url buf &optional with-headers)
  "Downloads URL into buffer BUF."
  (download-as-string url (lambda (str) (with-current-buffer buf (insert str)))
                      with-headers))

(defun download-to-current-buffer (with-headers url)
  "Downloads URL into current buffer."
  (interactive "P\nsURL: ")
  (download-to-buffer url (current-buffer) with-headers))

(defun advice-list (symbol)
  (let ((list))
    (advice-mapc (lambda (&rest args) (push args x)) symbol)
    list))

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

(defun overlay-get-eslint-message (overlay)
  "Get text message from OVERLAY if it is from `flymake-eslint--checker' backend."
  (when-let* ((diagnostics (and (overlayp overlay)
                                (overlay-get overlay 'flymake-diagnostic))))
    (when (eq (flymake--diag-backend diagnostics) 'flymake-eslint--checker)
      (flymake--diag-message diagnostics))))

(ert-deftest overlay-get-eslint-message ()
  "Tests basics of `overlay-get-eslint-message'."
  (should (null (overlay-get-eslint-message nil)))
  (should (null (overlay-get-eslint-message [])))
  (should (null (overlay-get-eslint-message (make-overlay 0 0)))))

(defun eslint-message-get-rule (text)
  "Extract rule name from eslint message."
  (string-trim (car (s-match "[[@0-9a-z/-]+]$" text)) "\\[" "\\]"))

(ert-deftest eslint-message-get-rule ()
  "Tests basics of `eslint-message-get-rule'."
  (should (string= "react-hooks/exhaustive-deps"
                   (eslint-message-get-rule "warning: React Hook useEffect has a missing dependency: 'init'. Either include it or remove the dependency array [react-hooks/exhaustive-deps]")))
  (should (string= "jsx-a11y/anchor-has-content"
                   (eslint-message-get-rule "Anchor must have content [jsx-a11y/anchor-has-content]")))
  (should (string= "@typescript-eslint/rule"
                   (eslint-message-get-rule "Rule in scoped package [@typescript-eslint/rule]"))))

(defun overlay-get-current-eslint-messages ()
  (let ((messages (seq-map #'overlay-get-eslint-message (overlays-at (point)))))
    (seq-remove #'null messages)))

(defun eslint-ignore-rules ()
  "Insert eslint-disable-next-line rule pragma for overlay warning on current point."
  (interactive)
  (when-let* ((rules (seq-map #'eslint-message-get-rule
                              (overlay-get-current-eslint-messages))))
    (save-excursion
      (beginning-of-line)
      (newline-and-indent)
      (previous-line)
      (call-interactively #'comment-dwim)
      (insert "eslint-disable-next-line " (mapconcat #'identity rules " ")))))

(defun eslint-extract-missing-dependencies (message)
  (let (dependencies)
    (with-temp-buffer
      (insert message)
      (goto-char 0)
      (when (search-forward "missing dependencies: " nil t)
        (while (re-search-forward "'[^']+'" nil t)
          (push (string-trim (match-string-no-properties 0) "'" "'") dependencies))))
    dependencies))

(defun eslint-react-fix-missing-dependencies ()
  (interactive)
  (when-let* ((dependencies (mapcan #'eslint-extract-missing-dependencies (overlay-get-current-eslint-messages))))
    (save-excursion
      (search-forward "]")
      (backward-char 2)
      (let ((empty (looking-at "\\[")))
        (forward-char)
        (unless empty
          (insert ", ")))
      (insert (string-join dependencies ", ")))))

(defun dark-mode ()
  "This seems to be good enough."
  (interactive)
  (set-face-attribute 'default nil
                      :foreground (face-attribute 'default :background)
                      :background (face-attribute 'default :foreground))
  (dolist (face (face-list))
    (face-spec-recalc face nil)))

(defun uuid ()
  (interactive)
  (insert
   (if (fboundp 'org-id-uuid)
       (org-id-uuid)
     (string-trim (shell-command-to-string "uuid")))))

(defun uuid0 ()
  (interactive)
  (insert "00000000-0000-0000-0000-000000000000"))

(defun read-string-at-point (&optional prompt)
  "Read string or thing at point.

If PROMPT is specified, use it to prompt to edit the value before
returning."
  (if (region-active-p)
      (buffer-substring (region-beginning)
                        (region-end))
    (let ((string-at-point (string-trim
                            (or (ignore-errors (thing-at-point 'string))
                                (thing-at-point 'symbol)
                                "")
                            "['\" \t\n\r]+"
                            "['\" \t\n\r]+")))
      (if prompt
          (read-string prompt string-at-point)
        string-at-point))))

(defvar github-search-default-params nil)

(defun github-search (query)
  (interactive (list (read-string-at-point "Query: ")))
  (browse-url (format "https://github.com/search?q=%s&type=code"
                      (let ((parameters
                             (seq-mapcat (lambda (pair) (format "%s:%s " (car pair) (cdr pair)))
                                         github-search-default-params
                                         'string)))
                        (url-hexify-string (concat parameters query))))))

(defun git-show (commit)
  (interactive (list (read-string-at-point)))
  (let ((directory default-directory))
    (with-current-buffer (get-buffer-create "*diff*")
      (unless (eq major-mode 'diff-mode) (diff-mode))
      (read-only-mode -1)
      (erase-buffer)
      (insert (let ((default-directory directory))
                (shell-command-to-string (format "git show %s" commit))))
      (read-only-mode t)
      (display-buffer "*diff*"))))

(provide 'my-commands)
