(deftheme dp "My Color Theme")

(let ((reversed nil))
  (cl-flet ((color (value &optional reversed-value)
                   (if reversed (or reversed-value value) value)))
    (let ((bg              (color "#d7dfd7" "#202420"))
          (fg              (color "#202420" "white"))
          (white           (color "white" "black"))
          (black           (color "black" "white"))
          (gray            (color "gray" "gray40"))
          (dark-gray       (color "gray50" "gray50"))

          (pink            (color "hot pink" "pink"))
          (blue            (color "deep sky blue"))
          (green           (color "yellow green"))
          (red             (color "tomato"))
          (lightpink       (color "misty rose" "pink"))

          (light-green     (color "#c8ffcf" "#1f4c1f"))
          (light-blue      (color "PaleTurquoise1" "blue"))

          (very-light-gray (color "#f5f5f5" "#202420")))
      (custom-theme-set-faces
       'dp
       `(default ((t (:foreground ,fg :background ,bg))))
       `(cursor ((t (:foreground ,black :background "red"))))
       `(fringe ((t (:background ,bg))))
       `(vertical-border ((t (:foreground ,gray))))

       `(highlight ((t (:background ,light-green))))
       `(region ((t (:background ,light-blue))))
       `(trailing-whitespace ((t (:background "misty rose"))))
       `(parenthesis ((t (:foreground ,(if reversed (color-lighten-name bg 30) (color-darken-name bg 30))))))

       `(font-lock-comment-face ((t (:foreground ,dark-gray :slant italic))))
       `(font-lock-constant-face ((t (:foreground ,pink))))

       `(mmm-default-submode-face ((t (:background ,very-light-gray))))

       `(org-level-1 ((t (:height 160 :weight bold :inherit outline-1))))
       `(org-level-2 ((t (:height 140 :weight bold :inherit outline-2))))
       `(org-level-3 ((t (:height 120 :weight bold :inherit outline-3))))

       `(git-gutter:modified ((t (:foreground ,blue))))
       `(git-gutter:added ((t (:foreground ,green))))
       `(git-gutter:deleted ((t (:foreground ,red))))

       `(js2-highlight-vars-face ((t (:inherit highlight))))
       `(js2-highlight-vars-second-face ((t (:inherit highlight))))

       `(mode-line
         ((t (:overline ,gray :underline nil :foreground ,black :background ,very-light-gray
                        :box (:line-width 3 :color ,very-light-gray :style unspecified)))))
       `(mode-line-inactive
         ((t (:overline ,gray :underline nil :foreground ,gray :background ,very-light-gray
                        :box (:line-width 3 :color ,very-light-gray :style unspecified)))))

       `(hl-tags-face ((t (:inherit highlight))))

       `(warning ((t (:background "light yellow" :underline (:color "orange" :style wave)))))
       `(error ((t (:foreground "misty rose" :background ,red))))
       ))))

(provide-theme 'dp)
