;; Emacs customization file
;; Time-stamp: <2013-11-18 20:53:31 donatas>
;; custom.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(backup-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/backups")))))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "open")
 '(c-block-comment-prefix "* ")
 '(compilation-message-face (quote default))
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode nil nil (cua-base))
 '(delete-selection-mode nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eshell-directory-name "~/.emacs.d/data/eshell/")
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(frame-title-format "emacs [%b %*%+ %f]" t)
 '(geben-dbgp-default-proxy (quote ("127.0.0.1" 9001 "emacs" nil t)))
 '(global-column-enforce-mode t)
 '(haskell-indent-after-keywords
   (quote
    (("where" 2 0)
     ("of" 2)
     ("do" 4)
     ("mdo" 4)
     ("rec" 4)
     ("in" 4 0)
     ("{" 2)
     ("if" 4)
     "then" "else"
     ("let" 4))))
 '(haskell-indent-thenelse 1)
 '(helm-autoresize-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indentation-offset 2)
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(icon-title-format "emacs [%b]" t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-curly-indent-offset 0)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(mark-even-if-inactive t)
 '(max-lisp-eval-depth 4096)
 '(max-specpdl-size 8192)
 '(mouse-sel-retain-highlight t)
 '(org-agenda-files nil)
 '(org-export-htmlize-output-type (quote css))
 '(org-startup-folded t)
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(search-highlight t)
 '(speedbar-use-images t)
 '(sqlplus-format-output-tables-flag t)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(tramp-auto-save-directory "~/.emacs.d/data/tramp-auto-save")
 '(tramp-persistency-file-name "/home/donatas/.emacs.d/data/tramp")
 '(transient-mark-mode 1)
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#b58900")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#859900")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#2aa198")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(highlight-indentation-current-column-face ((t (:inherit highlight))) t)
;;  '(highlight-indentation-face ((t (:inherit highlight))) t)
;;  '(hl-line ((t (:inherit highlight))) t)
;;  '(magit-branch ((t (:weight bold))))
;;  '(magit-diff-add ((t (:inherit diff-added))))
;;  '(magit-diff-del ((t (:inherit diff-removed))))
;;  '(magit-section-title ((t (:weight bold))))
;;  '(web-mode-current-element-highlight-face ((t (:inherit highlight))) t)
;;  '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face))) t)
;;  '(web-mode-html-tag-custom-face ((t (:inherit font-lock-warning-face))) t)
;;  '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
