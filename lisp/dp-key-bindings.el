(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key [f5] 'whitespace-mode)
(global-set-key [f7] 'cua-mode)

(global-set-key (kbd "M-z") 'dp/zap-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)

(global-set-key (kbd "C-c M-c") 'dp/convert-camelcase-to-constant)
(global-set-key (kbd "C-c M-k") 'dp/convert-constant-to-camelcase)

(global-set-key (kbd "C-x o") nil) ;; force myself to use M-o

(global-set-key (kbd "M-1") 'dp/jump-to-window)
(global-set-key (kbd "M-2") 'dp/jump-to-window)
(global-set-key (kbd "M-3") 'dp/jump-to-window)
(global-set-key (kbd "M-4") 'dp/jump-to-window)
(global-set-key (kbd "M-5") 'dp/jump-to-window)
(global-set-key (kbd "M-6") 'dp/jump-to-window)
(global-set-key (kbd "M-7") 'dp/jump-to-window)
(global-set-key (kbd "M-8") 'dp/jump-to-window)
(global-set-key (kbd "M-9") 'dp/command-keys-vector)
(global-set-key (kbd "M-0") 'dp/delete-window)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x M-d") 'delete-this-buffer-and-file)

(defun dp/command-keys-vector ()
  (interactive)
  (message (format "%S" (this-command-keys-vector))))

(provide 'dp-key-bindings)
