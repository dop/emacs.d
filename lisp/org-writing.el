;; -*- lexical-binding: t; -*-

(defvar org-writing-mode-to-enable
  '(olivetti-mode variable-pitch-mode flyspell-mode org-modern-mode)
  "Modes to enable when entering writing mode.")

(defvar org-writing-mode-to-disable
  '(org-indent-mode)
  "Modes to disable when entering writing mode.")

(defvar-local org-writing-mode--enabled-modes nil
  "Local variable to keep enabled modes that should be turned off on
mode exit.")

(defvar-local org-writing-mode--disabled-modes nil
  "Local variable to keep disabled modes that should be turned on on
mode exit.")

(define-minor-mode org-writing-mode
  "Collection of settings for my writing needs."
  :global nil
  (cond
   (org-writing-mode
    (setq-local face-remapping-alist '((default (:height 1.4))
                                       (outline-1 (:height 1.2) outline-1)
                                       (outline-2 (:height 1.1) outline-2)
                                       (fixed-pitch (:height 0.8) fixed-pitch))
                org-hide-leading-stars t
                olivetti-body-width 90)
    (dolist (submode org-writing-mode-to-enable)
      (unless (ignore-errors (symbol-value submode))
        (push submode org-writing-mode--enabled-modes)
        (funcall (symbol-function submode) t)))
    (dolist (submode org-writing-mode-to-disable)
      (unless (ignore-errors (symbol-value submode))
        (push submode org-writing-mode--disabled-modes)
        (funcall (symbol-function submode) -1))))
   (t
    (dolist (enabled-submode org-writing-mode--enabled-modes)
      (funcall (symbol-function enabled-submode) -1))
    (dolist (enabled-submode org-writing-mode--disabled-modes)
      (funcall (symbol-function enabled-submode) t))
    (kill-local-variable 'olivetti-body-width)
    (kill-local-variable 'face-remapping-alist)
    (kill-local-variable 'org-hide-leading-stars))))

(provide 'org-writing)
