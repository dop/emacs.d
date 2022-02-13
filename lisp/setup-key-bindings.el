;; -*- lexical-binding: t; -*-

(require 'misc)

(global-set-key (kbd "C-c C-m") #'execute-extended-command)

(global-set-key (kbd "C-w") #'kill-region-or-backward-word)
(global-set-key (kbd "C-k") #'kill-region-or-line)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "C-x M-d") #'delete-this-buffer-and-file)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(define-key project-prefix-map "C" #'project-compile-file)

(require 'mark-and-copy)
(global-set-key (kbd "M-w") #'mark-and-copy)
(global-set-key (kbd "M-W") #'mark-and-copy-mark)

(provide 'setup-key-bindings)
