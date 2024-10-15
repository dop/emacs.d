(deftheme dop)

(custom-theme-set-faces
 'dop
 '(default
   ((((background light)) :background "white")
    (((background dark)) :background "#292b29")))
 '(cursor
   ((((background light)) :background "red")
    (((background dark)) :background "green")))
 '(fringe
   ((((background light)) :inherit default :foreground "grey70")
    (((background dark)) :inherit default :foreground "grey40")))
 '(vertical-border
   ((((background light)) :foreground "grey80")
    (((background dark)) :foreground "grey40")))
 '(eglot-highlight-symbol-face
   ((t :inherit highlight)))
 '(mode-line
   ((t :inherit variable-pitch
       :background "LightSteelBlue2" :foreground "black"
       :box (:line-width 3 :style flat-button :color "LightSteelBlue2")
       :height 1.0)))
 '(mode-line-inactive
   ((((background light))
     :inherit mode-line :background "LightSteelBlue1" :foreground "grey50"
     :box (:line-width 3 :style flat-button :color "LightSteelBlue1"))
    (((background dark))
     :inherit mode-line :background "LightSteelBlue4" :foreground "grey20"
     :box (:line-width 3 :style flat-button :color "LightSteelBlue4"))))
 '(diff-hl-change
   ((((background light)) :background "light blue")
    (((background dark)) :background "royal blue")))
 '(diff-hl-delete
   ((t :background "tomato")))
 '(diff-hl-insert
   ((t :background "DarkOliveGreen2")))
 '(fixed-pitch
   ((t :inherit default)))
 '(trailing-whitespace
   ((((background light)) :background "RosyBrown1")
    (((background dark)) :background "brown4")))
 '(font-lock-comment-face
   ((((background light)) :foreground "Firebrick" :slant italic)
    (((background dark)) :foreground "chocolate1" :slant italic)))
 '(eshell-prompt
   ((((background light)) :background "grey95" :foreground "red" :overline "grey80")
    (((background dark)) :background "grey25" :foreground "pink" :overline "grey40"))))

(provide-theme 'dop)
