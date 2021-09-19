(global-set-key "\C-w" #'backward-kill-word)

(global-set-key (kbd "M-DEL") #'kill-whole-line)
(global-set-key "\C-x\C-k" #'kill-region)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x k") #'kill-buffer)
(global-set-key (kbd "C-k") #'kill-line)

(global-set-key (kbd "C-x a r") #'align-regexp)

(global-set-key (kbd "C-.") #'repeat)
(global-set-key (kbd "RET") #'newline-and-indent)

(global-set-key (kbd "A-h") 'mark-paragraph)
(global-set-key (kbd "M-z") #'dp/zap-to-char)
(global-set-key (kbd "M-Z") #'zap-to-char)

(global-set-key (kbd "M-/") #'hippie-expand)

(global-set-key (kbd "C-c C-m") #'execute-extended-command)

(global-set-key (kbd "C-x n i") #'narrow-to-region-indirect)

(global-set-key (kbd "C-c M-c") #'dp/convert-camelcase-to-constant)
(global-set-key (kbd "C-c M-k") #'dp/convert-constant-to-camelcase)

(global-set-key (kbd "M-1") #'dp/jump-to-window)
(global-set-key (kbd "M-2") #'dp/jump-to-window)
(global-set-key (kbd "M-3") #'dp/jump-to-window)
(global-set-key (kbd "M-4") #'dp/jump-to-window)
(global-set-key (kbd "M-5") #'dp/jump-to-window)
(global-set-key (kbd "M-6") #'dp/jump-to-window)
(global-set-key (kbd "M-7") #'dp/jump-to-window)
(global-set-key (kbd "M-8") #'dp/jump-to-window)
(global-set-key (kbd "M-9") #'dp/jump-to-window)
(global-set-key (kbd "M-0") #'dp/delete-window)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-x o") #'other-frame)

(global-set-key (kbd "<f6>") #'profiler-start)
(global-set-key (kbd "<f7>") #'profiler-report)
(global-set-key (kbd "<f8>") #'profiler-stop)

(global-set-key (kbd "M-l") #'downcase-dwim)

(global-set-key (kbd "C-x M-d") #'delete-this-buffer-and-file)

(global-set-key (kbd "<f5>") #'dp/compile)
(global-set-key (kbd "<f12>") #'projectile-magit-status)

(global-unset-key [right])
(global-unset-key [left])
(global-unset-key [up])
(global-unset-key [down])

(provide 'dp-key-bindings)
