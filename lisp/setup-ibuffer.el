(with-eval-after-load 'ibuffer
  (define-ibuffer-sorter project
    "Sort buffers by project or `default-directory'."
    (:description "project")
    (let ((a-dir (with-current-buffer (car a)
                   (or (cdr (project-current)) default-directory)))
          (b-dir (with-current-buffer (car b)
                   (or (cdr (project-current)) default-directory))))
      (string< a-dir b-dir)))

  (define-key ibuffer-mode-map (kbd "s p") #'ibuffer-do-sort-by-project)

  (require 'ibuf-ext)
  (define-ibuffer-filter project "Filter by project."
    (:description "project" :reader (project-current t))
    (seq-contains-p (project-buffers qualifier) buf)))

(add-hook 'ibuffer-mode-hook #'hl-line-mode)

(provide 'setup-ibuffer)
