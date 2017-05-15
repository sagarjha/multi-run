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
  (setq evaled-command command)
  (while names
    (when (functionp command)
      (setq evaled-command (funcall command (car names))))
    (run-at-time (concat (number-to-string (* delay-cnt delay)) " sec") nil 
		 'run-on-eshell-terminal evaled-command (car names))
    (setq names (cdr names))
    (setq delay-cnt (+ 1 delay-cnt))))

(setq eshell-window-batch 5)

(defun configure-eshell-terminals (num-terminals)
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
  (other-window 1)
  (setq eshell-list (number-sequence 1 num-terminals))
  (concat "Preemptively setting the eshell-list to " (prin1-to-string eshell-list)))

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

;; convenience function for ssh'ing to the terminals
(defun run-ssh (&optional eshell-num)
  (when (not (boundp (quote eshell-list)))
    (error "Define the variable eshell-list on which you want to run the command on. e.g. (list 1 2 3)"))
  (when (not (boundp (quote nodes-list)))
    (error "Define the variable nodes-list to be the list of ip-addrs e.g. (list \"128.84.139.10\" \"128.84.139.11\" \"128.84.139.12\")"))
  (if eshell-num
      (run-on-multiple-eshell-terminals (lambda (x) (concat "ssh " (elt nodes-list (- x 1)))) (list eshell-num))
      (run-on-multiple-eshell-terminals (lambda (x) (concat "ssh " (elt nodes-list (- x 1)))) eshell-list)))

(provide 'multi-eshell-run)
