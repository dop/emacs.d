;; -*- lexical-binding: t; -*-

(deftheme dop)

(require 'color)

(let* ((acme-cyan            "#9eeeee")
       (acme-cyan-light      "#e9f6f6")
       (acme-red             "#880000")
       (acme-red-light       "#F8E8E8")
       (acme-yellow          "#eeee9e")
       (acme-yellow-light    "#ffffea")
       (acme-green           "#005500")
       (acme-green-alt       "#006600")
       (acme-green-light     "#E8FCE8")
       (acme-blue            "#1054AF")
       (acme-blue-light      "#E1FAFF")
       (acme-purple          "#555599")
       (acme-purple-light    "#FFEAFF"))
  (custom-theme-set-faces
   'dop
   `(default
     ((((background light)) :foreground "black" :background  ,(color-desaturate-name acme-yellow-light 20))
      (((background dark)) :foreground "white" :background ,(color-darken-name acme-blue 50))))
   `(cursor
     ((((background dark)) :background "magenta")
      (((background light)) :background "red")))
   `(fringe
     ((((background light))
       (:inherit default :foreground "grey70"))
      (((background dark))
       (:inherit default :foreground "grey40"))))
   `(vertical-border
     ((((background light))
       (:foreground ,acme-yellow))
      (((background dark))
       (:foreground ,acme-blue))))
   `(hl-line
     ((((background light)) :background ,(color-darken-name acme-yellow-light 8))
      (((background dark)) :background ,(color-darken-name acme-blue 30))))
   '(mode-line
     ((t (:inherit variable-pitch :width condensed :background "grey75" :foreground "black" :box (:line-width (2 . 2) :style released-button)))))
   `(mode-line-inactive
     ((((background light))
       (:inherit mode-line :foreground "grey30" :background "grey90" :box (:line-width 2 :style flat-button :color "grey90")))
      (((background dark))
       (:inherit mode-line :foreground "grey70" :background "grey40" :box (:line-width 2 :style flat-button :color "grey40")))))
   `(eglot-mode-line
     ((t (:weight bold))))
   `(eglot-diagnostic-tag-unnecessary-face
     ((((background light)) (:inherit shadow :background ,acme-red-light))
      (((background dark)) (:inherit shadow :background ,acme-red))))
   `(eglot-highlight-symbol-face
     ((t (:inherit match))))
   `(diff-hl-change
     ((((background light)) (:background "light blue"))
      (((background dark)) (:background "royal blue"))))
   `(diff-hl-delete
     ((((background light)) (:background "tomato"))))
   `(diff-hl-insert
     ((((background light)) (:background "DarkOliveGreen2"))))
   `(diff-header
     ((t (:inherit shadow :background unspecified))))
   `(diff-file-header
     ((((background light))
       (:inherit shadow :foreground "black" :weight bold))
      (((background dark))
       (:inherit shadow :foreground "white" :weight bold))))
   `(fixed-pitch
     ((t (:inherit default))))
   `(trailing-whitespace
     ((((background light)) (:background "RosyBrown1"))
      (((background dark)) (:background "brown4"))))
   `(error
     ((((background light)) (:foreground "red" :weight bold))
      (((background dark)) (:foreground "red" :weight bold))))
   `(warning
     ((((background light)) (:foreground "orange" :weight bold))
      (((background dark)) (:foreground "orange" :weight bold))))
   `(font-lock-comment-delimiter-face
     ((t (:foreground "grey"))))
   `(font-lock-comment-face
     ((((background light)) (:foreground ,acme-blue :slant italic))
      (((background dark)) (:foreground ,acme-cyan-light :slant italic))))
   `(font-lock-function-call-face ((t )))
   `(font-lock-variable-name-face
     ((((background light)) (:foreground ,acme-purple))
      (((background dark)) (:foreground ,(color-darken-name acme-purple-light 5)))))
   `(font-lock-function-name-face
     ((((background light)) (:foreground ,(color-darken-name acme-cyan 50)))
      (((background dark)) (:foreground ,(color-lighten-name "orange" 60)))))
   `(font-lock-keyword-face ((t )))
   `(font-lock-type-face ((t )))
   `(font-lock-variable-use-face ((t )))
   `(font-lock-constant-face ((t )))
   `(font-lock-builtin-face ((t (:inherit shadow))))
   `(font-lock-property-name-face ((t )))
   `(font-lock-property-use-face ((t )))
   `(font-lock-doc-face
     ((((background light)) (:foreground ,acme-green-alt :background ,acme-green-light))
      (((background dark)) (:foreground ,(color-darken-name acme-green-light 20)))))
   `(font-lock-string-face
     ((((background light)) (:foreground ,acme-green-alt))
      (((background dark)) (:foreground ,(color-darken-name acme-green-light 10)))))
   `(font-lock-number-face
     ((((background light)) (:foreground ,acme-green-alt))
      (((background dark)) (:foreground ,(color-darken-name acme-green-light 5)))))
   `(font-lock-bracket-face ((t (:inherit shadow))))
   `(font-lock-punctuation-face ((t (:inherit shadow))))
   `(typescript-ts-jsx-attribute-face ((t )))
   `(eshell-prompt
     ((((background light)) (:inherit shadow :overline "grey80"))
      (((background dark)) (:inherit shadow :overline "grey40"))))
   `(dictionary-reference-face
     ((((background light)) (:inherit variable-pitch :foreground "blue"))
      (((background dark)) (:inherit variable-pitch :foreground "sky blue"))))
   `(dictionary-word-entry-face
     ((((type x)) (:italic t))
      (((type tty) (class color)) (:foreground "green"))
      (t (:inherit variable-pitch :inverse t))))
   `(shr-code ((t (:inherit fixed-pitch :box (:line-width (1 . 1) :color "grey50")))))
   `(sly-reader-conditional-face ((t (:inherit shadow))))
   `(treesit-fold-replacement-face ((t (:inherit shadow))))
   `(treesit-fold-replacement-mouse-face ((t (:inherit shadow))))))

(provide-theme 'dop)
