(require 'f)
(require 'cl)

(defun jar? (filepath)
  "Return t if FILEPATH looks like JAR file."
  (not (null (and
              (s-match "\\.jar$" filepath)
              (s-match "Zip archive data"
                       (shell-command-to-string (format "file %s" filepath)))))))

(defun dp/jar-to-source-jar (filepath)
  (replace-regexp-in-string "\\.jar$" "-sources.jar" filepath))

(defun dp/ensime-project-each (ensime-project-definition fn)
  (-each (plist-get ensime-project-definition :subprojects) fn)
  ensime-project-definition)

(defun dp/ensime-project-add-sources (project)
  "For each sub-project in PROJECT set :reference-sources-roots."
  (cl-flet ((jar-exists? (filepath) (and (f-exists? filepath) (jar? filepath))))
    (let ((sources (-map #'dp/jar-to-source-jar (plist-get project :compile-deps))))
      (plist-put project :reference-source-roots (-filter #'jar-exists? sources)))))

(defun dp/ensime-project-add-source-roots (project)
  (let* ((source-roots (plist-get project :source-roots))
         (scala-main (-find (curry #'s-match "src/main/scala") source-roots))
         (new-roots (cons (s-replace "src/main/scala" "src/it/scala" scala-main) source-roots)))
    (plist-put project :source-roots (-filter #'f-exists? (delete-dups new-roots)))))

(defun dp/ensime-project-add-resources-to-runtime-deps (project)
  (let* ((source-root (nth 0 (plist-get project :source-roots)))
         (root (replace-regexp-in-string "/src/\\(it\\|main\\|test\\)/\\(scala\\|java\\)/?$" "" source-root))
         (resources (f-glob "src/*/resources" root))
         (deps (plist-get project :runtime-deps)))
    (plist-put project :runtime-deps (append deps resources))))

(defun dp/fix-ensime-project (project)
  (dp/ensime-project-each
   project
   (lambda (subproject)
     (dp/ensime-project-add-sources subproject)
     (dp/ensime-project-add-source-roots subproject)
     (dp/ensime-project-add-resources-to-runtime-deps subproject))))

;; (with-output-to-temp-buffer "*ensime config*"
;;   (print
;;    (dp/fix-ensime-project
;;     (nth 0 (read-from-string (ensime-read-from-file "~/Projects/hotels/hotels-world-server/.ensime"))))))

(provide 'dp-ensime)
