(provide 'multi-eshell-run)

(setq multi-run-timers (list))

;; run a command on a single eshell terminal
(defun mr-run-on-single-eshell-terminal (command name)
  (set-buffer (concat "*eshell*<" (number-to-string name) ">"))
  (goto-char (point-max))
  (insert command)
  (eshell-send-input))

;; run a command on multiple eshell terminals - useful for ssh
(defun mr-run-on-eshell-terminals (command names &optional delay)
  "Run a command on multiple eshell terminals - useful for ssh"
  (when (not delay)
    (setq delay 0))
  (setq delay-cnt 0)
  (setq evaled-command command)
  (while names
    (when (functionp command)
      (setq evaled-command (funcall command (car names))))
    (setq multi-run-timers (cons (run-at-time (concat (number-to-string (* delay-cnt delay)) " sec") nil 
					      'mr-run-on-single-eshell-terminal evaled-command (car names)) multi-run-timers))
    (setq names (cdr names))
    (setq delay-cnt (1+ delay-cnt))))

(defun mr-configure-eshell-terminals (num-terminals &optional eshell-window-batch)
  (when (not eshell-window-batch)
    (setq eshell-window-batch 5))
  (delete-other-windows)
  (dotimes (i (1+ (/ (+ -1 num-terminals) eshell-window-batch)))
    (split-window-horizontally)
    (balance-windows))
  (dotimes (i num-terminals)
    (if (= (% i eshell-window-batch) 0)
	(other-window 1)
      (progn (split-window-vertically)
	     (balance-windows)
	     (other-window 1)))
    (eshell (1+ i)))
  (other-window 1)
  (setq eshell-list (number-sequence 1 num-terminals))
  (concat "Preemptively setting the eshell-list to " (prin1-to-string eshell-list)))

(defun multi-run (&rest cmd)
  (interactive)
  (when (not (boundp (quote eshell-list)))
    (error "Define the variable eshell-list on which you want to run the command on. e.g. (list 1 2 3)"))
  (mapc
   (lambda (command) (mr-run-on-eshell-terminals command eshell-list))
   cmd)
  cmd)

(defun multi-run-with-delay (delay cmd)
  (when (not (boundp (quote eshell-list)))
    (error "Define the variable eshell-list on which you want to run the command on. e.g. (list 1 2 3)"))
  (mr-run-on-eshell-terminals cmd eshell-list delay))

(defun multi-run-loop (cmd &optional times delay)
  (when (not times)
    (setq times 1))
  (when (not delay)
    (setq delay 0))
  (setq delay-cnt 0)
  (dotimes (i times)
    (progn 
      (setq multi-run-timers (cons (run-at-time (concat (number-to-string (* delay-cnt delay)) " sec")
						nil '(lambda (cmd) (multi-run cmd)) cmd) multi-run-timers))
      (setq delay-cnt (1+ delay-cnt)))))

;; convenience function for ssh'ing to the terminals
(defun multi-run-ssh (&optional eshell-num)
  (when (not (boundp (quote eshell-list)))
    (error "Define the variable eshell-list on which you want to run the command on. e.g. (list 1 2 3)"))
  (when (not (boundp (quote nodes-list)))
    (error "Define the variable nodes-list to be the list of ip-addrs e.g. (list \"128.84.139.10\" \"128.84.139.11\" \"128.84.139.12\")"))
  (mr-run-on-eshell-terminals (lambda (x) (concat "ssh " (if (boundp (quote ssh-username)) (concat ssh-username "@") "") (elt nodes-list (- x 1)))) (if eshell-num (list eshell-num) eshell-list)))

(defun multi-run-kill-terminals (&optional eshell-num)
  (when (not (boundp (quote eshell-list)))
    (error "Define the variable eshell-list on which you want to run the command on. e.g. (list 1 2 3)"))
  (if eshell-num
      (kill-buffer (concat "*eshell*<" (number-to-string eshell-num) ">"))
    (mapc (lambda (name) (kill-buffer (concat "*eshell*<" (number-to-string name) ">"))) eshell-list))
  nil)

(defun multi-run-kill-all-timers ()
  (mapc (lambda (timer) (cancel-timer timer)) multi-run-timers)
  "All timers canceled")

(provide 'multi-eshell-run)
