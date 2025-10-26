;; -*- lexical-binding: t; -*-

(use-package eglot
  :commands eglot
  :config (setq eglot-stay-out-of '(flymake-diagnostic-functions))

  :init
  (defun eglot-rename (newname)
    "Rename the current symbol to NEWNAME."
    (interactive
     (list (read-from-minibuffer
            (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                           "unknown symbol"))
            (thing-at-point 'symbol t)
            nil nil nil
            (symbol-name (symbol-at-point)))))
    (eglot--server-capable-or-lose :renameProvider)
    (eglot--apply-workspace-edit
     (jsonrpc-request (eglot--current-server-or-lose)
                      :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                             :newName ,newname))
     current-prefix-arg))

  (defun eglot-hover-eldoc-function-callback (contents range)
    (let* ((info (unless (seq-empty-p contents)
                   (eglot--hover-info contents range))))
      (list info :echo (and info
                            (let ((newline-pos (string-match "\n" info )))
                              (if (string-prefix-p "```" info)
                                  (string-match "\n" info (1+ newline-pos))
                                newline-pos))))))

  (defun eglot-hover-eldoc-function (cb &rest _ignored)
    "A member of `eldoc-documentation-functions', for hover."
    (when (eglot-server-capable :hoverProvider)
      (let ((buf (current-buffer)))
        (eglot--async-request
         (eglot--current-server-or-lose)
         :textDocument/hover (eglot--TextDocumentPositionParams)
         :success-fn (eglot--lambda ((Hover) contents range)
                       (eglot--when-buffer-window buf
                         (apply cb (eglot-hover-eldoc-function-callback contents range))))
         :hint :textDocument/hover))
      t)))

(provide 'setup-eglot)
