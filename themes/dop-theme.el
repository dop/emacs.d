(deftheme dop)

(custom-theme-set-faces
 'dop
 '(default
   ((((background light)) :foreground "black" :background "ivory")
    (((background dark)) :foreground "ivory" :background "#15273b")))
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
   ((t
     :inherit variable-pitch
     :foreground "black"
     :background "grey"
     :box (:line-width 3 :style flat-button :color "grey"))))
 '(mode-line-inactive
   ((t
     :inherit mode-line
     :foreground "grey30")))
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
    (((background dark)) :background "grey25" :foreground "pink" :overline "grey40")))
 '(dictionary-reference-face ((t (:inherit variable-pitch :foreground "blue"))))
 '(dictionary-word-entry-face ((((type x)) (:italic t))
                               (((type tty) (class color)) (:foreground "green"))
                               (t (:inherit variable-pitch :inverse t)))))

(provide-theme 'dop)
