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

(provide 'dp-key-bindings)
