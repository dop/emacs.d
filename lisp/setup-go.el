;;; setup-go.el --- go configuration                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Donatas Petrauskas

;; Author: Donatas Petrauskas
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(defun go-run (edit-args)
  "Run current file with 'go run'."
  (interactive "P")
  (let ((async-shell-command-buffer 'confirm-kill-process)
        (cmd (format "go run -race %s" (buffer-file-name))))
    (when edit-args
      (setq cmd (read-from-minibuffer "Command: " cmd)))
    (basic-save-buffer)
    (async-shell-command cmd)))

(defun go-format-make-sentinel (buf stdout stderr)
  (lambda (proc type)
    (when (eq 'exit (process-status proc))
      (if (eq 0 (process-exit-status proc))
          (let* ((formatted (with-current-buffer stdout (buffer-string))))
            (with-current-buffer buf
              (when-let* ((current (buffer-string))
                          (_ (not (equal (secure-hash 'sha1 current)
                                         (secure-hash 'sha1 formatted))))
                          (p (point)))
                (erase-buffer)
                (insert formatted)
                (goto-char p))))
        (with-current-buffer stderr
          (message (car (string-lines (buffer-string)))))))))

(defun go-format-buffer (&rest command)
  "Run COMMAND on a current buffer (as standard input) and replace buffer's
content with the output."
  (let* ((command-name (car command))
         (buf (current-buffer))
         (stdout (get-buffer-create (concat " *" command-name " out*")))
         (stderr (get-buffer-create (concat " *" command-name " err*"))))
    (with-current-buffer stderr (erase-buffer))
    (with-current-buffer stdout (erase-buffer))
    (let ((proc (make-process :name command-name
                              :buffer stdout
                              :stderr stderr
                              :command command
                              :sentinel (go-format-make-sentinel buf stdout stderr))))
      (process-send-string proc (buffer-string))
      (process-send-eof proc))))

(defun gofmt ()
  "Format current buffer using gofmt."
  (interactive)
  (go-format-buffer "gofmt"))

(defun goimports ()
  "Format current buffer using goimports."
  (interactive)
  (go-format-buffer "goimports"))

(defvar go-treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'go :feature 'keyword :override 'append
   '((["continue" "break" "return" "defer"] @control-flow-keyword-face))
   :language 'go :feature 'builtin :override 'append
   '((call_expression function: (identifier) @control-flow-dangerous-keyword-face
                      (:eq? @control-flow-dangerous-keyword-face "panic")))))

(defun setup-go-mode ()
  (setq tab-width 4)
  (treesit-add-font-lock-settings go-treesit-font-lock-settings))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :config
  (keymap-unset go-ts-mode-map "C-c C-d")
  (keymap-set go-ts-mode-map "C-c C-c" #'go-run)
  (add-hook 'go-ts-mode-hook #'setup-go-mode))

(provide 'setup-go)
;;; setup-go.el ends here
