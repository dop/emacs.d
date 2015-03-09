;;; Yasnippet

(require 'dropdown-list)

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-completing-prompt
                             yas-ido-prompt))


(define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)
