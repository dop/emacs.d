(define-minor-mode beginner-mode
  "Beginner friendly mode. Defines some well known keys."
  :lighter " Noob"
  :version "0"
  :keymap '(("\M-c" . copy-region-as-kill)
            ("\M-x" . kill-region)
            ("\M-v" . yank)
            ("\M-s" . save-buffer)
            ("\M-f" . isearch-forward)
            ([right] . forward-char)
            ([left] . backward-char)
            ([up] . previous-line)
            ([down] . next-line)))

(provide 'beginner-mode)
