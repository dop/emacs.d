;; -*- lexical-binding: t; -*-

(defvar zoom-previous-frame-configurations
  (make-hash-table)
  "A frame->frameset map.")

(defun window-side-p (&optional window)
  (window-parameter (window-normalize-window window) 'window-side))

(defun zoom-in ()
  "Save current frameset configuration and delete other windows."
  (interactive)
  (let* ((frame (window-frame))
         (filters (append '((left . :never) (top . :never)) frameset-filter-alist))
         (config (frameset-save (list frame) :filters filters))
         (buf (when (window-side-p) (window-buffer))))
    (setf (gethash frame zoom-previous-frame-configurations) config)
    (ignore-errors (window-toggle-side-windows))
    (delete-other-windows)
    (when buf (switch-to-buffer buf))))

(defun zoom-out ()
  "Pop last pushed frameset configuration if any and restore it."
  (interactive)
  (let* ((frame (window-frame))
         (config (gethash frame zoom-previous-frame-configurations)))
    (if config
        (progn
          (frameset-restore config :reuse-frames t)
          (setf (gethash frame zoom-previous-frame-configurations) nil))
      (message "No configuration saved for current frame."))))

(defun zoom-dwim ()
  (interactive)
  (if (window-buffer (frame-root-window))
      (zoom-out)
    (zoom-in)))

(provide 'zoom)
