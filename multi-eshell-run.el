(provide 'multi-eshell-run)

;; run a command on a single eshell terminal
(defun run-on-eshell-terminal (command name)
  (set-buffer (concat "*eshell*<" (number-to-string name) ">"))
  (goto-char (point-max))
  (insert command)
  (eshell-send-input))

;; run a command on multiple eshell terminals - useful for ssh
(defun run-on-multiple-eshell-terminals (command names &optional delay)
  "Run a command on multiple eshell terminals - useful for ssh"
  (when (not delay)
    (setq delay 0))
  (setq cnt 0)
  (while names
    (run-at-time (concat (number-to-string (* cnt delay)) " sec") nil 
		 'run-on-eshell-terminal command (car names))
    (setq names (cdr names))
    (setq cnt (+ 1 cnt))))

(defun run (cmd)
  (run-on-multiple-eshell-terminals cmd eshell-list))

(defun run-with-delay (delay cmd)
  (run-on-multiple-eshell-terminals cmd eshell-list delay))

(provide 'multi-eshell-run)
