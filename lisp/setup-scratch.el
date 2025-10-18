;; setup-scratch.el  -*- lexical-binding: t; -*-

(require 'cl-seq)

;; TODO make a minor mode

(defcustom scratch-file-directory "~/.emacs.d/scratch"
  "Directory to place and open scratch files."
  :type 'string
  :risky t)

(defun get-mode-scratch-buffer-create (mode &optional name)
  (let* ((mode-name
          (string-replace "-mode" "" (symbol-name mode)))
         (buffer-name
          (format "*scratch %s%s*"
                  (if name (format "%s " name) "")
                  mode-name))
         (buffer
          (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (eq mode major-mode)
        (funcall mode))
      (pop-to-buffer buffer))))

(defun list-unique-modes ()
  (cl-remove-duplicates (mapcar #'cdr auto-mode-alist)))

(defun scratch-file (mode-name)
  (interactive (list (completing-read "Mode: " (list-unique-modes) nil t)))
  (make-directory scratch-file-directory t)
  (with-current-buffer (find-file
                        (expand-file-name (concat "scratch." (string-replace "-mode" "" mode-name))
                                          scratch-file-directory))
    (let ((mode (intern mode-name)))
      (unless (eq major-mode mode)
        (funcall mode)))))

(defun create-scratch-buffer (mode &optional project)
  (interactive (list (completing-read "Mode: " (list-unique-modes)  nil t)
                     (project-current)))
  (let ((default-directory (if project (project-root project) default-directory))
        (name (and project (project-name project))))
    (get-mode-scratch-buffer-create (intern mode) name)))

(defun create-scratch-org-buffer (&optional name)
  (interactive (list (project-current-name)))
  (get-mode-scratch-buffer-create 'org-mode name))

(keymap-global-set "C-c s f" #'scratch-file)
(keymap-global-set "C-c s b" #'create-scratch-buffer)
(keymap-global-set "C-c s o" #'create-scratch-org-buffer)

(provide 'setup-scratch)
