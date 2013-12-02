;; Emacs customization file
;; Time-stamp: <2013-11-18 20:53:31 donatas>
;; custom.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(backup-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/backups")))))
 '(battery-mode-line-format "[%b%p%% %t]")
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "xdg-open")
 '(c-block-comment-prefix "* ")
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(delete-selection-mode nil)
 '(display-battery-mode -1)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(eshell-directory-name "~/.emacs.d/data/eshell/")
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(frame-title-format "emacs [%b %*%+ %f]" t)
 '(geben-dbgp-default-proxy (quote ("127.0.0.1" 9001 "emacs" nil t)))
 '(haskell-indent-after-keywords (quote (("where" 2 0) ("of" 2) ("do" 4) ("mdo" 4) ("rec" 4) ("in" 4 0) ("{" 2) ("if" 4) "then" "else" ("let" 4))))
 '(haskell-indent-thenelse 1)
 '(icon-title-format "emacs [%b]" t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-curly-indent-offset 0)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline t)
 '(mark-even-if-inactive t)
 '(max-lisp-eval-depth 4096)
 '(max-specpdl-size 8192)
 '(mouse-sel-retain-highlight t)
 '(org-agenda-files (quote ("~/Projects/mmshost/org/MMShost.org" "~/Projects/saludmedica/SaludMedica.org" "~/Projects/kodbyen/Kodbyen.org" "~/Projects/realty/org/Realty.org" "~/Projects/ilovethisshop/org/Project.org" "/home/donatas/Org/cdlc.org" "/home/donatas/Org/contacts.org" "/home/donatas/Org/journal.org" "/home/donatas/Org/notes.org")))
 '(org-export-htmlize-output-type (quote css))
 '(org-startup-folded t)
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(search-highlight t)
 '(sqlplus-format-output-tables-flag t)
 '(tramp-auto-save-directory "~/.emacs.d/data/tramp-auto-save")
 '(tramp-persistency-file-name "/home/donatas/.emacs.d/data/tramp")
 '(transient-mark-mode 1)
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(warning-suppress-types (quote ((undo discard-info)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#dcdccc"))))
 '(hl-line ((t (:background "#eeeeec"))))
 '(magit-branch ((t (:weight bold))))
 '(magit-diff-add ((t (:inherit diff-added))))
 '(magit-diff-del ((t (:inherit diff-removed))))
 '(magit-section-title ((t (:weight bold)))))
