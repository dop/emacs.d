;;; SQL

(add-hook 'sql-mode-hook
          (lambda nil
            (sql-highlight-ansi-keywords)
            (sql-highlight-mysql-keywords)))

;; (autoload 'plsql-mode "plsql.el" nil t)
;; (setq auto-mode-alist
;;       (append '(("\\.pk[sb]$" . plsql-mode)
;;              ("\\.plsql" . plsql-mode)
;;              ("\\.prc$" . plsql-mode))
;;            auto-mode-alist))

;; (eval-after-load "plsql" '(progn (unless (featurep 'sqlplus) (require 'sqlplus))))

;; (autoload 'sqlplus-mode "sqlplus.el" nil t)
;; (add-to-list 'auto-mode-alist '("\\.sqp$" . init-sqlplus))
;; (defun init-sqlplus ()
;;   "load plsql-mode before sqlplus"
;;   (unless (featurep 'plsql)
;;     (require 'plsql)
;;     (plsql-mode))
;;   (sqlplus-mode)
;;   (setq sqlplus-html-output-sql t)
;;   (setq sqlplus-html-output-encoding 'utf-8)
;;   (sql-highlight-ansi-keywords)
;;   (sql-highlight-oracle-keywords))
