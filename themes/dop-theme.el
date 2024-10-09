(deftheme dop)

(custom-theme-set-faces
 'dop
 '(default
   ((((background light)) :background "white")
    (((background dark)) :background "#222")))
 '(cursor
   ((((background light)) :background "red")
    (((background dark)) :background "green")))
 '(fringe ((((background light)) :foreground "grey70" :background "transparent")
           (((background dark)) :foreground "grey40" :background "transparent")))
 '(vertical-border ((((background light)) :foreground "grey80")
                    (((background dark)) :foreground "grey40")))
 '(eglot-highlight-symbol-face ((t :inherit highlight)))
 '(mode-line ((t :inherit variable-pitch :background "grey75" :foreground "black" :box (:line-width 4 :style flat-button :color "grey75") :height 1.0)))
 '(mode-line-inactive ((t :inherit mode-line :background "grey75" :foreground "grey50")))
 '(diff-hl-change ((((background light)) :background "light blue")
                   (((background dark)) :background "royal blue")))
 '(diff-hl-delete ((t :background "tomato")))
 '(diff-hl-insert ((t :background "DarkOliveGreen2")))
 '(fixed-pitch ((t :inherit default)))
 '(trailing-whitespace
   ((((background light)) :background "RosyBrown1")
    (((background dark)) :background "brown4")))
 '(font-lock-comment-face
   ((((background light)) :foreground "Firebrick" :slant italic)
    (((background dark)) :foreground "chocolate1" :slant italic))))

(provide-theme 'dop)
