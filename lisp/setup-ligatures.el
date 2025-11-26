;; -*- lexical-binding: t; -*-

(defun pipe-ligature-p (lig)
  (string-prefix-p "|" lig))

(defun ligature-adjust-pragmatapro-pipes (mode ligatures)
  "https://github.com/fabrizioschiavi/pragmatapro/issues/293"
  (let ((font (face-attribute 'default :font nil t)))
    ;; We run only in our specific case:
    ;;   - using PragamatPro Mono Liga
    ;;   - concrete mode
    ;;   - all ligatures are strings
    ;;   - some ligatures start with pipe
    ;;   - auto-compoisition-mode is enabled.
    (when (and auto-composition-mode
               (eq (font-get font :family) 'PragmataPro\ Mono\ Liga)
               (symbolp mode)
               (seq-every-p #'stringp ligatures)
               (seq-some #'pipe-ligature-p ligatures))
      (let ((pipe-ligatures (seq-filter #'pipe-ligature-p ligatures)))
        (dolist (lig pipe-ligatures)
          (let ((gs (composition-get-gstring 0 (length lig) font lig)))
            ;; Glyph was not shaped yet if ID is nil
            (unless (aref gs 1)
              (with-temp-buffer
                (funcall mode)
                ;; Run through all ligatures, just in case.
                (mapc (lambda (l) (insert l "\n")) pipe-ligatures)
                ;; These 2 lines force glyphs to be shaped
                (switch-to-buffer (current-buffer) 'norecord)
                (redisplay)
                (setq gs (composition-get-gstring 0 (length lig) font lig))))
            (dotimes (i (lgstring-glyph-len gs))
              (aset (lgstring-glyph gs i) 7 1)
              (aset (lgstring-glyph gs i) 8 1))))))))

(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
  (advice-add 'ligature-set-ligatures :after #'ligature-adjust-pragmatapro-pipes)

  (ligature-set-ligatures
   'fundamental-mode
   '("=>" "->" "==>" "-->" "www"))

  (ligature-set-ligatures
   'lisp-data-mode
   '("=>" "->" "==>" "-->" "--" "#{" "#[" "#(" "#_" ";;" ";;;" ";;;;" "##"))

  (ligature-set-ligatures 'sgml-mode '("<!-- -->"))

  (let ((js-ligatures
         '("=>" "===" "==" "!==" "!=" "->" "-->" "--" "++"
           "<=" ">="
           "+=" "|=" "*=" "&=" "?=" "^=" "%="
           "?." "<>" "</>" "/>"
           "/**" "//"
           "**" "||" "&&")))
    (ligature-set-ligatures 'js-base-mode js-ligatures)
    (ligature-set-ligatures 'typescript-ts-base-mode js-ligatures)))

(provide 'setup-ligatures)
