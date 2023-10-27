(require 'project)
(require 'my-library)

(defun project-current-root ()
  (when-let ((project (project-current)))
    (project-root project)))

(defun project-has-file-p (project filepath)
  (let* ((directory (project-root project))
         (expanded (expand-file-name filepath directory)))
    (and (file-exists-p expanded) expanded)))

(defun project-has-node-script-p (project script)
  (project-has-file-p project (concat "node_modules/.bin/" script)))

(defun project-npx (project script)
  (when (project-has-node-script-p project script)
    (concat "npx --no-install " script)))

(if (fboundp 'project-name)
    (warn "`project-name' is available, no need to re-define.")
  (cl-defgeneric project-name (project)
    "A human-readable name for the project.
Nominally unique, but not enforced."
    (file-name-nondirectory (directory-file-name (project-root project)))))

(provide 'project-tools)
