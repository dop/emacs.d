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

(defun go-run ()
  "Run current file with 'go run'."
  (interactive)
  (basic-save-buffer)
  (async-shell-command (format "go run -race %s" (buffer-file-name))))

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
                              :sentinel (lambda (proc type)
                                          (when (eq 'exit (process-status proc))
                                            (if (eq 0 (process-exit-status proc))
                                                (let ((formatted (with-current-buffer stdout
                                                                   (buffer-string))))
                                                  (with-current-buffer buf
                                                    (let ((p (point)))
                                                      (erase-buffer)
                                                      (insert formatted)
                                                      (goto-char p))))
                                              (with-current-buffer stderr
                                                (message (car (string-lines (buffer-string)))))))))))
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

(use-package go-ts-mode
  :mode "\\.go\\'"
  :config
  (keymap-unset go-ts-mode-map "C-c C-d")
  (keymap-set go-ts-mode-map "C-c C-c" #'go-run))

(provide 'setup-go)
;;; setup-go.el ends here
