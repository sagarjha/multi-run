(provide 'multi-run)

(require 'window-layout)

(defvar mr-term "eshell"
  "Terminal type to run the commands on")
;; (setq mr-term "eshell")
;; (setq mr-term "shell")
;; (setq mr-term "ansi-term")
;; (setq mr-term "term")
;; (setq mr-term "multi-term")
(setq multi-run-timers (list))

(defun mr-get-buffer-name (term-num)
  "Returns the name of the buffer for a given terminal number"
  (cond ((string= mr-term "eshell") (concat "eshell<" (number-to-string term-num) ">"))
	((string= mr-term "shell") (concat "shell<" (number-to-string term-num) ">"))
	((string= mr-term "ansi-term") (concat "ansi-term<" (number-to-string term-num) ">"))
	((string= mr-term "term") (concat "term<" (number-to-string term-num) ">"))
	((string= mr-term "multi-term") (concat "terminal<" (number-to-string term-num) ">"))))

(defun mr-get-input-function ()
  "Returns the name of the function that runs the input on the terminal"
  (cond ((string= mr-term "eshell") 'eshell-send-input)
	((string= mr-term "shell") 'comint-send-input)
	((string= mr-term "ansi-term") 'term-send-input)
	((string= mr-term "term") 'term-send-input)
	((string= mr-term "multi-term") 'term-send-input)))

(defun mr-get-new-input-point ()
  "Moves point to the latest prompt in the terminal buffer"
  (cond ((string= mr-term "eshell") eshell-last-output-end)
	((string= mr-term "shell") (process-mark (get-buffer-process (current-buffer))))
	((string= mr-term "ansi-term") (process-mark (get-buffer-process (current-buffer))))
	((string= mr-term "term") (process-mark (get-buffer-process (current-buffer))))
	((string= mr-term "multi-term") (process-mark (get-buffer-process (current-buffer))))))

(defun mr-open-terminal (term-num)
  "Opens terminal number term-num in a buffer if it's not already open. In any case, switches to it"
  (when (not (get-buffer (mr-get-buffer-name term-num)))
    (progn
      (cond ((string= mr-term "eshell") (eshell term-num))
	    ((string= mr-term "shell") (shell))
	    ((string= mr-term "ansi-term") (ansi-term "/bin/bash"))
	    ((string= mr-term "term") (term "/bin/bash"))
	    ((string= mr-term "multi-term") (multi-term)))
      (rename-buffer (mr-get-buffer-name term-num))))
  (switch-to-buffer (mr-get-buffer-name term-num)))

;; run a command on a single terminal
(defun mr-run-on-single-terminal (command term-num)
  "Runs the command on a single terminal"
  (set-buffer (mr-get-buffer-name term-num))
  (goto-char (mr-get-new-input-point))
  (insert command)
  (funcall (mr-get-input-function)))

;; run a command on multiple terminals
(defun mr-run-on-terminals (command term-nums &optional delay)
  "Runs the command on multiple terminals with an optional delay between running on successive terminals"
  (when (not delay)
    (setq delay 0))
  (setq delay-cnt 0)
  (setq evaled-command command)
  (while term-nums
    (when (functionp command)
      (setq evaled-command (funcall command (car term-nums))))
    (setq multi-run-timers (cons (run-at-time (concat (number-to-string (* delay-cnt delay)) " sec") nil 
					      'mr-run-on-single-terminal evaled-command (car term-nums)) multi-run-timers))
    (setq term-nums (cdr term-nums))
    (setq delay-cnt (1+ delay-cnt))))

(defun mr-create-terminals (num-terminals)
  "Creates num-terminals number of terminals"
  (dotimes (i num-terminals)
    (mr-open-terminal (1+ i))))

(defun mr-configure-terminals (num-terminals &optional window-batch)
  "Lays out the terminals on the screen"
  (when (not window-batch)
    (setq window-batch 5))
  (setq master-buffer-name (buffer-name))
  (mr-create-terminals num-terminals)

  (defun mr-make-symbols (num-terminals) 
    (defun mr-make-symbols-helper (cnt)
      (when (<= cnt num-terminals)
	(vconcat (vector (make-symbol (concat "term" (number-to-string cnt)))) (mr-make-symbols-helper (1+ cnt)))))
    (mr-make-symbols-helper 0))

  (defun mr-make-dict (num-terminals)
    (defun mr-make-dict-helper (cnt)
      (when (<= cnt num-terminals)
	(cons (list :name (aref sym-vec cnt)
		    :buffer (mr-get-buffer-name cnt))
	      (mr-make-dict-helper (1+ cnt)))))
    (mr-make-dict-helper 1))

  (setq sym-vec (mr-make-symbols num-terminals))
  (setq dict (cons (list :name (aref sym-vec 0)
			 :buffer master-buffer-name)
		   (mr-make-dict num-terminals)))
  
  (defun make-vertical-or-horizontal-pane (num-terminals offset choice)
    (if (= num-terminals 1) (aref sym-vec offset)
      (list (if (= choice 0) '| '-) `(,(if (= choice 0) :left-size-ratio :upper-size-ratio)
				      ,(/ (- num-terminals 1.0) num-terminals))
	    (make-vertical-or-horizontal-pane (1- num-terminals) (1- offset) choice) (aref sym-vec offset))))

  (defun make-internal-recipe (num-terminals window-batch)
    (setq num-panes (if (= (% num-terminals window-batch) 0)
			(/ num-terminals window-batch) (1+ (/ num-terminals window-batch))))
    (if (<= num-terminals window-batch)
	(make-vertical-or-horizontal-pane num-terminals num-terminals 1)
      (list '| `(:left-size-ratio ,(/ (- num-panes 1.0) num-panes))
	    (make-internal-recipe (- num-terminals (if (= (% num-terminals window-batch) 0)
						       window-batch
						     (% num-terminals window-batch)))
				  window-batch)
	    (make-vertical-or-horizontal-pane (if (= (% num-terminals window-batch) 0) window-batch
						(% num-terminals window-batch))
					      num-terminals 1))))

  (setq internal-recipe (make-internal-recipe num-terminals window-batch))
  (setq overall-recipe `(- (:upper-size-ratio 0.9)
			   ,internal-recipe ,(aref sym-vec 0)))
  (wlf:layout
   overall-recipe
   dict)
  (select-window (get-buffer-window master-buffer-name))
  (setq mr-terminals (number-sequence 1 num-terminals))
  (concat "Preemptively setting mr-terminals to " (prin1-to-string mr-terminals)))

(defun multi-run-with-delay (delay &rest cmd)
  "Runs one or more commands on multiple terminals with the provided delay"
  (when (not (boundp (quote mr-terminals)))
    (error "Define the variable mr-terminals on which you want to run the command on. e.g. (list 1 2 3)"))
  (setq delay-now 0)
  (dolist (command cmd)
    (setq multi-run-timers (cons (run-at-time (concat (number-to-string delay-now) " sec") nil 'mr-run-on-terminals command mr-terminals delay) multi-run-timers))
    (setq delay-now (+ delay-now (* (length mr-terminals) delay))))
  nil)

(defun multi-run-with-delay2 (delay &rest cmd)
  "Runs one or more commands on multiple terminals with the provided delay - but the delay is between different command invocations at the terminals, as opposed to delay between running commands on different terminals"
  (when (not (boundp (quote mr-terminals)))
    (error "Define the variable mr-terminals on which you want to run the command on. e.g. (list 1 2 3)"))
  (setq delay-now 0)
  (dolist (command cmd)
    (setq multi-run-timers (cons (run-at-time (concat (number-to-string delay-now) " sec") nil 'mr-run-on-terminals command mr-terminals) multi-run-timers))
    (setq delay-now (+ delay-now delay)))
  nil)

(defun multi-run (&rest cmd)
  "Runs one or more commands on multiple terminals" 
  (apply #'multi-run-with-delay 0 cmd))

(defun multi-run-loop (cmd &optional times delay)
  "Loops the command given number of times with delay between successive runs"
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
(defun multi-run-ssh (&optional terminal-num)
  "Establishes ssh connections in the terminals with the help of user-defined variables"
  (when (not (boundp (quote mr-terminals)))
    (error "Define the variable mr-terminals on which you want to run the command on. e.g. (list 1 2 3)"))
  (when (not (boundp (quote mr-hostnames-list)))
    (error "Define the variable mr-hostnames-list to be the list of ip-addrs e.g. (list \"128.84.139.10\" \"128.84.139.11\" \"128.84.139.12\")"))
  (mr-run-on-terminals (lambda (x) (concat "ssh " (if (boundp (quote ssh-username)) (concat ssh-username "@") "") (elt mr-hostnames-list (- x 1)))) (if terminal-num (list terminal-num) mr-terminals)))

(defun mr-kill-terminals (&optional terminal-num)
  "Kills terminals"
  (when (not (boundp (quote mr-terminals)))
    (error "Define the variable mr-terminals on which you want to run the command on. e.g. (list 1 2 3)"))
  (if terminal-num
      (kill-buffer (mr-get-buffer-name terminal-num))
    (mapc (lambda (terminal-num) (kill-buffer (mr-get-buffer-name terminal-num))) mr-terminals))
  nil)

(defun mr-kill-all-timers ()
  "Cancels commands running on a loop or via delay functions"
  (mapc (lambda (timer) (cancel-timer timer)) multi-run-timers)
  "All timers canceled")

(provide 'multi-run)
