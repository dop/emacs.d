(require 'project)

(defun project-has-file-p (project filepath)
  (pcase (or project (project-current))
    (`(,_ . ,directory)
     (let ((expanded (expand-file-name filepath directory)))
       (and (file-exists-p expanded) expanded)))))

(defun project-has-node-script-p (project filepath)
  (project-has-file-p project (concat "node_modules/.bin/" filepath)))

(provide 'project-tools)
