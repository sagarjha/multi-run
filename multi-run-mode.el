(defvar multi-run-mode-hook nil)

(defun multi-run-cmd ()
  (interactive)
  (insert "\n")
  (run (buffer-substring-no-properties cmd-start (1- (point))))
  (let ((prompt "multi-run \$"))
    (insert (propertize prompt 'font-lock-face '(:foreground "red")))
    (insert (propertize " " 'font-lock-face '(:foreground "green"))))
  (set-marker cmd-start (point)))
(defun configure-multi-run ()
  (interactive)
  (configure-eshell-terminals (read-number "Enter the number of terminals: ")))
(defun set-eshell-list ()
  (interactive)
  (setq eshell-list (mapcar 'string-to-number (split-string (read-string "Enter eshell-list as space separated numbers: "))))
  (print (concat "eshell-list set to " (prin1-to-string eshell-list))))
(defun multi-run-ssh ()
  (interactive)
  (run-ssh))

(defvar multi-run-map
  (let ((multi-run-map (make-keymap)))
    (define-key multi-run-map "\C-m" 'multi-run-cmd)
    (define-key multi-run-map (kbd "C-c C-c") 'configure-multi-run)
    (define-key multi-run-map (kbd "C-c C-l") 'set-eshell-list)
    (define-key multi-run-map (kbd "C-c C-s") 'multi-run-ssh)
    multi-run-map)
  "Keymap for multi-run mode")

(define-derived-mode multi-run-mode fundamental-mode "multi-run"
  (interactive)
  (use-local-map multi-run-map)
  (setq mode-name "multi-run")
  (run-hooks 'multi-run-mode-hook)
  (let ((prompt "multi-run \$"))
    (insert (propertize prompt 'font-lock-face '(:foreground "red")))
    (insert (propertize " " 'font-lock-face '(:foreground "green"))))
  (set (make-local-variable 'cmd-start) (point-marker)))

(defun multi-run (&optional arg)
  (interactive "P")
  (setq multi-run-buffer-name "multi-run")
  (let ((buf (cond ((numberp arg)
		    (get-buffer-create (format "%s<%d>"
					       multi-run-buffer-name
					       arg)))
		   (arg
		    (generate-new-buffer multi-run-buffer-name))
		   (t
		    (get-buffer-create multi-run-buffer-name)))))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'multi-run-mode)
      (multi-run-mode))
    buf))

(provide 'multi-run-mode)
