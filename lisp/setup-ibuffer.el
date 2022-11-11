(with-eval-after-load 'ibuffer
  (require 'project-tools)

  (define-ibuffer-sorter project
    "Sort buffers by project or `default-directory'."
    (:description "project")
    (let ((a-dir (with-current-buffer (car a)
                   (or (project-current-root) default-directory)))
          (b-dir (with-current-buffer (car b)
                   (or (project-current-root) default-directory))))
      (string< a-dir b-dir)))

  (define-key ibuffer-mode-map (kbd "s p") #'ibuffer-do-sort-by-project)

  (require 'ibuf-ext)
  (define-ibuffer-filter project "Filter by project."
    (:description "project" :reader (project-current t))
    (seq-contains-p (project-buffers qualifier) buf)))

(add-hook 'ibuffer-mode-hook #'hl-line-mode)

(provide 'setup-ibuffer)
