;; -*- lexical-binding: t; -*-

(require 'misc)

(global-set-key (kbd "C-w") #'kill-region-or-backward-word)
(global-set-key (kbd "C-k") #'kill-region-or-line)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "C-x M-d") #'delete-this-buffer-and-file)

(require 'mark-and-copy)
(global-set-key (kbd "M-w") #'mark-and-copy)
(global-set-key (kbd "M-W") #'mark-and-copy-mark)

(comment
 (global-set-key (kbd "M-1") #'dp/jump-to-window)
 (global-set-key (kbd "M-2") #'dp/jump-to-window)
 (global-set-key (kbd "M-3") #'dp/jump-to-window)
 (global-set-key (kbd "M-4") #'dp/jump-to-window)
 (global-set-key (kbd "M-5") #'dp/jump-to-window)
 (global-set-key (kbd "M-6") #'dp/jump-to-window)
 (global-set-key (kbd "M-7") #'dp/jump-to-window)
 (global-set-key (kbd "M-8") #'dp/jump-to-window)
 (global-set-key (kbd "M-9") #'dp/jump-to-window)
 (global-set-key (kbd "M-0") #'dp/delete-window))

(comment
 (global-set-key (kbd "<f6>") #'profiler-start)
 (global-set-key (kbd "<f7>") #'profiler-report)
 (global-set-key (kbd "<f8>") #'profiler-stop))

(comment
 (global-set-key (kbd "<f5>") #'dp/compile)
 (global-set-key (kbd "<f12>") #'project-magit-status)
 (global-set-key (kbd "<f9>") #'open-terminal-app))

(provide 'key-bindings)
