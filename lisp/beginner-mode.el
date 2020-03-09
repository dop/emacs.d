(defun beginner-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(define-minor-mode beginner-mode
  "Beginner friendly mode. Defines some well known keys."
  :lighter " Noob"
  :version "0"
  :keymap '(([(meta ?c)] . copy-region-as-kill)
            ([(meta ?x)] . kill-region)
            ([(meta ?v)] . yank)
            ([(meta ?s)] . save-buffer)
            ([(meta ?f)] . isearch-forward)
            ([(meta ?u)] . undo)
            ([(meta ?i)] . beginner-indent-buffer)
            ([M-up] . drag-stuff-up)
            ([M-down] . drag-stuff-down)
            ([(control ?c) (control ?m)] . smex)
            ([right] . forward-char)
            ([left] . backward-char)
            ([up] . previous-line)
            ([down] . next-line)))

(provide 'beginner-mode)
