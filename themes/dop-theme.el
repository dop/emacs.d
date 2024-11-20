(deftheme dop)

(require 'color)

(let* ((acme-cyan            "#007777")
       (acme-cyan-light      "#A8EFEB")
       (acme-red             "#880000")
       (acme-red-light       "#F8E8E8")
       (acme-yellow          "#888838")
       (acme-yellow-light    "#F8FCE8")
       (acme-green           "#005500")
       (acme-green-alt       "#006600")
       (acme-green-light     "#E8FCE8")
       (acme-blue            "#1054AF")
       (acme-blue-light      "#E1FAFF")
       (acme-purple          "#555599")
       (acme-purple-light    "#FFEAFF")
       (dark-bg (color-darken-name acme-purple 50)))

  (custom-theme-set-faces
   'dop
   `(default
     ((((background light)) :foreground "black" :background ,acme-yellow-light)
      (((background dark)) :foreground ,acme-yellow-light :background ,dark-bg)))
   '(cursor
     ((((background light)) :background "red")
      (((background dark)) :background "green")))
   '(fringe
     ((((background light)) :inherit default :foreground "grey70")
      (((background dark)) :inherit default :foreground "grey40")))
   `(vertical-border
     ((((background light)) :foreground ,acme-yellow)
      (((background dark)) :foreground ,acme-blue)))
   '(eglot-highlight-symbol-face
     ((t :inherit highlight)))
   `(hl-line
     ((((background light)) :background ,(color-darken-name acme-yellow-light 20))
      (((background dark)) :background ,(color-lighten-name dark-bg 50))))
   `(mode-line
     ((((background light))
       :inherit variable-pitch
       :foreground "black"
       :background ,acme-cyan-light
       :box (:line-width 2 :style flat-button :color ,acme-cyan-light))
      (((background dark))
       :inherit variable-pitch
       :foreground ,acme-purple-light
       :background ,(color-lighten-name acme-purple 10)
       :box (:line-width 2 :style flat-button :color ,(color-lighten-name acme-purple 10)))))
   `(mode-line-inactive
     ((((background light))
       :inherit mode-line
       :foreground "grey30"
       :background "grey90"
       :box (:line-width 2 :style flat-button :color "grey90"))
      (((background dark))
       :inherit mode-line
       :foreground "grey70"
       :background "grey40"
       :box (:line-width 2 :style flat-button :color "grey40"))))
   '(eglot-mode-line
     ((t :weight bold)))
   '(diff-hl-change
     ((((background light)) :background "light blue")
      (((background dark)) :background "royal blue")))
   '(diff-hl-delete
     ((t :background "tomato")))
   '(diff-hl-insert
     ((t :background "DarkOliveGreen2")))
   `(diff-header
     ((t (:inherit shadow :background unspecified))))
   `(diff-file-header
     ((((background light))
       (:inherit shadow :foreground "black" :weight bold))
      (((background dark))
       (:inherit shadow :foreground "white" :weight bold))))
   '(fixed-pitch
     ((t :inherit default)))
   '(trailing-whitespace
     ((((background light)) :background "RosyBrown1")
      (((background dark)) :background "brown4")))
   '(font-lock-comment-face
     ((t :inherit shadow :foreground unspecified :slant italic)))
   '(eshell-prompt
     ((((background light)) :foreground "red" :overline "grey80")
      (((background dark)) :foreground "pink" :overline "grey40")))
   '(dictionary-reference-face ((t (:inherit variable-pitch :foreground "blue"))))
   '(dictionary-word-entry-face ((((type x)) (:italic t))
                                 (((type tty) (class color)) (:foreground "green"))
                                 (t (:inherit variable-pitch
                                              :inverse t))))))

(provide-theme 'dop)
