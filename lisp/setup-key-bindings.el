;; -*- lexical-binding: t; -*-

(require 'zoom)
(require 'my-commands)

(keymap-global-set "C-z" nil)
(keymap-global-set "C-x 1" #'delete-other-windows)

;; For terminal
(keymap-global-set "C-c i" #'completion-at-point)
(keymap-global-set "C-c C-m" #'execute-extended-command)

(keymap-global-set "C-w" #'kill-region-or-backward-word)
(keymap-global-set "C-k" #'kill-region-or-line)

(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-u" #'upcase-dwim)

(keymap-global-set "C-x M-d" #'delete-this-buffer-and-file)
(keymap-global-set "C-x M-r" #'rename-buffer-and-file)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-c C-d" #'duplicate-dwim)
(keymap-global-set "C-c C-k" #'kill-whole-line)

(keymap-global-set "C-x O" #'other-frame)
(keymap-global-set "M-`" #'other-frame)

(keymap-global-set "M-/" #'hippie-expand)

(keymap-global-set "<f5>" #'recompile)

(require 'mark-and-copy)
(keymap-global-set "M-w" #'mark-and-copy)
(keymap-global-set "M-W" #'mark-and-copy-mark)

(provide 'setup-key-bindings)
