(require 'project)

(defun project-npm-p (&optional project)
  (let ((p (or project (project-current))))
    (and (eq 'npm (car p)) p)))

(defun project-has-file-p (filepath &optional project)
  (pcase (or project (project-current))
    (`(,_ . ,directory)
     (let ((expanded (expand-file-name filepath directory)))
       (and (file-exists-p expanded) expanded)))))

(defun project-has-node-script-p (filepath &optional project)
  (project-has-file-p (concat "node_modules/.bin/" filepath)))

(provide 'project-tools)
