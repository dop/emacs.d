;; -*- lexical-binding: t; -*-

(require 'zoom)
(require 'my-commands)

(global-set-key (kbd "C-z") nil)

;; For terminal
(global-set-key (kbd "C-c i") #'completion-at-point)
(global-set-key (kbd "C-c C-m") #'execute-extended-command)

(global-set-key (kbd "C-w") #'kill-region-or-backward-word)
(global-set-key (kbd "C-k") #'kill-region-or-line)

(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)

(global-set-key (kbd "C-x M-d") #'delete-this-buffer-and-file)
(global-set-key (kbd "C-x M-r") #'rename-buffer-and-file)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x w z") #'zoom-dwim)

(global-set-key (kbd "C-x O") #'other-frame)
(global-set-key (kbd "M-`") #'other-frame)

(define-key project-prefix-map "C" #'project-compile-file)

(require 'mark-and-copy)
(global-set-key (kbd "M-w") #'mark-and-copy)
(global-set-key (kbd "M-W") #'mark-and-copy-mark)

(provide 'setup-key-bindings)
