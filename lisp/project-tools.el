(require 'project)
(require 'my-library)

(defun project-current-root ()
  (project-root (project-current)))

(defun project-has-file-p (project filepath)
  (let* ((directory (project-root project))
         (expanded (expand-file-name filepath directory)))
    (and (file-exists-p expanded) expanded)))

(defun project-has-node-script-p (project script)
  (project-has-file-p project (concat "node_modules/.bin/" script)))

(defun project-npx (project script)
  (when (project-has-node-script-p project script)
    (concat "npx --no-install " script)))

(provide 'project-tools)
