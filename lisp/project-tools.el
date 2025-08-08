(require 'project)
(require 'my-library)

(defun project-current-root ()
  (when-let ((project (project-current)))
    (project-root project)))

(defun project-current-name ()
  (when-let ((project (project-current)))
    (project-name project)))

(defun project-has-file-p (project filepath)
  (let* ((directory (project-root project))
         (expanded (expand-file-name filepath directory)))
    (and (file-exists-p expanded) expanded)))

(defun project-has-node-script-p (project script)
  (project-has-file-p project (concat "node_modules/.bin/" script)))

(defun project-npx (project script)
  (when (project-has-node-script-p project script)
    (concat "npx --no-install " script)))

(defun project-forget-node_module-projects ()
  "Forget all known projects that reside inside node_modules."
  (interactive)
  (dolist (proj (project-known-project-roots))
    (when (string-match-p "/node_modules/" proj)
      (project-forget-project proj))))

(defun project-forget-tmp-projects ()
  "Forget all known projects that reside inside node_modules."
  (interactive)
  (dolist (proj (project-known-project-roots))
    (when (string-match-p "^~/tmp/" proj)
      (project-forget-project proj))))

(provide 'project-tools)
