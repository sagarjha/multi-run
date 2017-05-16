(defvar multi-run-mode-hook nil)
(defun multi-run ()
  (interactive)
  (insert "\n")
  (run (buffer-substring-no-properties start (1- (point))))
  (let ((prompt "multi-run \$"))
    (insert (propertize prompt 'font-lock-face '(:foreground "red")))
    (insert (propertize " " 'font-lock-face '(:foreground "green"))))
  (set-marker start (point)))

(defvar multi-run-map
  (let ((multi-run-map (make-keymap)))
    (define-key multi-run-map "\C-j" 'multi-run)
    multi-run-map)
  "Keymap for multi-run mode")

(define-derived-mode multi-run-mode fundamental-mode "multi-run"
  (interactive)
  (use-local-map multi-run-map)
  (setq mode-name "multi-run")
  (set (make-local-variable 'start) (point-marker))
  (run-hooks 'multi-run-mode-hook))

(provide 'multi-run-mode)
