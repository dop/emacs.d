;; Emacs customization file
;; Time-stamp: <2013-11-18 20:53:31 donatas>
;; custom.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(backup-directory-alist (\` (("." \, (expand-file-name "~/.emacs.d/backups")))))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "open")
 '(c-block-comment-prefix "* ")
 '(compilation-message-face (quote default))
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode nil nil (cua-base))
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(delete-selection-mode nil)
 '(dired-listing-switches "-alF")
 '(display-time-24hr-format t t)
 '(display-time-day-and-date t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eshell-directory-name "~/.emacs.d/data/eshell/")
 '(explicit-shell-file-name "/bin/bash")
 '(fill-column 80)
 '(frame-title-format "emacs [%b %*%+ %f]" t)
 '(geben-dbgp-default-proxy (quote ("127.0.0.1" 9001 "emacs" nil t)))
 '(global-column-enforce-mode t)
 '(haskell-indent-after-keywords
   (quote
    (("where" 2 0)
     ("of" 2)
     ("do" 2)
     ("mdo" 2)
     ("rec" 2)
     ("in" 2 0)
     ("{" 2)
     ("if" 2)
     "then" "else"
     ("let" 2))))
 '(haskell-indent-thenelse 1)
 '(helm-autoresize-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indentation-offset 2)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
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
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
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
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files nil)
 '(org-export-htmlize-output-type (quote css))
 '(org-startup-folded t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((js2-missing-semi-one-line-override . t)
     (js2-strict-missing-semi-warning))))
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(search-highlight t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#839496" 0.2))
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
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tramp-auto-save-directory "~/.emacs.d/data/tramp-auto-save")
 '(tramp-persistency-file-name "/home/donatas/.emacs.d/data/tramp")
 '(transient-mark-mode 1)
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(warning-suppress-types (quote ((undo discard-info))))
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red")))))
