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
  (setq delay-cnt 0)
  (setq loop-cnt 1)
  (setq evaled-command command)
  (while names
    (when (functionp command)
      (setq evaled-command (funcall command loop-cnt)))
    (run-at-time (concat (number-to-string (* delay-cnt delay)) " sec") nil 
		 'run-on-eshell-terminal evaled-command (car names))
    (setq names (cdr names))
    (setq delay-cnt (+ 1 delay-cnt))
    (setq loop-cnt (+ 1 loop-cnt))))

(setq eshell-window-batch 5)

(defun open-eshell-terminals (num-terminals)
  (delete-other-windows)
  (dotimes (i (+ 1 (/ (+ -1 num-terminals) eshell-window-batch)))
    (split-window-horizontally)
    (balance-windows))
  (dotimes (i num-terminals)
    (if (= (% i eshell-window-batch) 0)
	(other-window 1)
      (progn (split-window-vertically)
	     (balance-windows)
	     (other-window 1)))
    (eshell (+ i 1)))
  (other-window 1))

(defun run (&rest cmd)
  (when (not (boundp (quote eshell-list)))
    (error "Define the variable eshell-list on which you want to run the command on. e.g. (list 1 2 3)"))
  (mapc
   (lambda (command) (run-on-multiple-eshell-terminals command eshell-list))
   cmd)
  nil)

(defun run-with-delay (delay cmd)
  (when (not (boundp (quote eshell-list)))
    (error "Define the variable eshell-list on which you want to run the command on. e.g. (list 1 2 3)"))
  (run-on-multiple-eshell-terminals cmd eshell-list delay))

(provide 'multi-eshell-run)
