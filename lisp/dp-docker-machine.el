(require 's)
(require 'dash)
(require 'pcase)

(defun dp/docker-machines-get ()
  (->> (-drop 1 (s-lines (shell-command-to-string "docker-machine ls")))
       (-map (lambda (s) (s-split " " s t)))
       (-filter #'identity)
       (-map #'dp/machine-line-to-plist)))

(defun dp/docker-machine-line-to-plist (line)
  (pcase line
    (`(,name ,active ,driver ,state ,url . ,_)
     (list :name name
           :active (equal active "*")
           :diver driver
           :state (pcase state
                    ("Running"  'running)
                    ("Paused"   'paused)
                    ("Saved"    'saved)
                    ("Stopped"  'stopped)
                    ("Stopping" 'stopping)
                    ("Starting" 'starting)
                    ("Error"    'error))
           :url url))))

(defun dp/docker-machine-env (name)
  (->> (s-lines (shell-command-to-string (format "docker-machine env %s" name)))
       (-map (curry #'s-match "export \\([a-zA-Z_]+\\)=\"\\([^\"]+\\)\""))
       (-filter #'identity)
       (-map (lambda (matches) (list (nth 1 matches) (nth 2 matches))))))

(defun dp/docker-machine-select (&optional name)
  (interactive (list (ido-completing-read
                      "Docker machine"
                      (-map (lambda (m) (plist-get m :name)) (dp/get-docker-machines)))))
  (let ((config (dp/docker-machine-env name)))
    (-map (curry #'apply #'setenv) config)
    (message (s-join "\n" (-map (curry #'s-join "=") config)))))

(provide 'dp-docker-machine)
