(make-variable-buffer-local
 (defvar eshell-buffer-list (list 1)
   "List of eshell buffers currently being managed"))

;; run a command on multiple eshell terminals
(defun run-on-multiple-eshell-terminals (&optional USE-REGION QUEUE-P NO-NEWLINE)
  "Run a command on multiple eshell terminals"
  (progn
    (message "here before the while loop")
    (let ((buffer-list eshell-buffer-list))
      (while buffer-list
	(progn
	  (message "here in run-on-...")
	  (set-buffer (concat "*eshell*<" (number-to-string (car buffer-list)) ">"))
	  (eshell-send-input USE-REGION QUEUE-P NO-NEWLINE)
	  (setq buffer-list (cdr buffer-list)))))))

(defun eshell-send (&optional USE-REGION QUEUE-P NO-NEWLINE)
  (interactive)
  (message "here")
  (run-on-multiple-eshell-terminals USE-REGION QUEUE-P NO-NEWLINE))

(define-minor-mode multi-run-mode
  "A minor mode for managing multiple eshell terminals"
  :lighter " multi-run"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-j C-j") 'eshell-send)
            map))

(provide 'multi-run)
