(deftheme dop)

(custom-theme-set-faces
 'dop
 '(cursor
   ((((background light)) :background "red")
    (((background dark)) :background "green")))
 '(eglot-highlight-symbol-face ((t (:inherit highlight))))
 '(mode-line ((t :inherit variable-pitch :background "grey75" :foreground "black" :box (:line-width (2 . 2) :style flat-button :color "grey75") :height 1.0)))
 '(mode-line-inactive ((t :inherit mode-line :background "grey75" :foreground "grey50")))
 '(diff-hl-change ((((background light)) :background "light blue")
                   (((background dark)) :background "royal blue")))
 '(diff-hl-delete ((t :background "tomato")))
 '(diff-hl-insert ((t :background "DarkOliveGreen2")))
 '(fixed-pitch ((t :inherit default)))
 '(trailing-whitespace
   ((((background light)) :background "RosyBrown1")
    (((background dark)) :background "brown4")))

 '(org-level-1 ((t :inherit outline-1 :height 1.3)))
 '(org-level-2 ((t :inherit outline-2 :height 1.2)))
 '(org-level-3 ((t :inherit outline-3 :height 1.1))))

(provide-theme 'dop)
