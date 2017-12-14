A minor mode for managing multiple emacs eshell terminals and running commands on them

# Functions

(defvar mr-term ...
  &quot;Terminal type to run the commands on&quot;)

(defun mr-get-buffer-name (term-num)  
  &quot;Returns the name of the buffer for a given terminal number&quot;  
  ...)

(defun mr-get-input-function ()  
  &quot;Returns the name of the function that runs the input on the terminal&quot;  
  ...)

(defun mr-get-new-input-point ()  
  &quot;Moves point to the latest prompt in the terminal buffer&quot;  
  ...)

(defun mr-open-terminal (term-num)  
  &quot;Opens terminal number term-num in a buffer if it's not already open. In any case, switches to   it&quot;  
  ...)

(defun mr-run-on-single-terminal (command term-num)  
  &quot;Runs the command on a single terminal&quot;  
  ...)

(defun mr-run-on-terminals (command term-nums &optional delay)  
  &quot;Runs the command on multiple terminals with an optional delay between running on successive terminals&quot;  
  ...)

(defun mr-create-terminals (num-terminals)  
  &quot;Creates num-terminals number of terminals&quot;  
  ...)

(defun mr-configure-terminals (num-terminals &optional window-batch)  
  &quot;Lays out the terminals on the screen&quot;  
  ...)

(defun multi-run-with-delay (delay &rest cmd)  
  &quot;Runs one or more commands on multiple terminals with the provided delay&quot;  
  ...)

(defun multi-run-with-delay2 (delay &rest cmd)  
  &quot;Runs one or more commands on multiple terminals with the provided delay - but the delay is between different command invocations at the terminals, as opposed to delay between running commands on different terminals&quot;  
  ...)

(defun multi-run (&rest cmd)  
  &quot;Runs one or more commands on multiple terminals&quot;   
  ...)

(defun multi-run-loop (cmd &optional times delay)  
  &quot;Loops the command given number of times with delay between successive runs&quot;  
  ...)

(defun multi-run-ssh (&optional terminal-num)  
  &quot;Establishes ssh connections in the terminals with the help of user-defined variables&quot;  
  ...)

(defun mr-kill-terminals (&optional terminal-num)  
  &quot;Kills terminals&quot;  
  ...)

(defun mr-kill-all-timers ()  
  &quot;Cancels commands running on a loop or via delay functions&quot;  
  ...)


In addition, you can set the variable `mr-terminals` to manually change the list of terminals on which you want to run the command.

# Suggested customizations in `.emacs`

* With many eshell buffers on the screen with multi-run, texts may not completely show on the screen. Using visual-line mode, emacs will break up lines visually at word boundaries:  
    `(add-hook 'eshell-mode-hook 'visual-line-mode)`

  ![Alt text](docs/before_and_after_visual_line_mode.png?raw=true "Before and after visual line mode - without line wrapping, entire directory contents are not visible")

  Before and after visual line mode - without line wrapping, entire directory contents are not visible

* Emacs will not automatically scroll output of eshell and terminal buffers. Add this:

    >(custom-set-variables  
    >  &#39;(comint-move-point-for-output (quote all))  
    >  &#39;(comint-scroll-to-bottom-on-output (quote all))  
    >  &#39;(eshell-scroll-to-bottom-on-output (quote all)))

* SSH setup

  Provide the list of IP-addreses/hostnames for run-ssh to work. For this, define the variable `mr-hostnames-list`.  
    `(setq mr-hostnames-list (list "ip_addr1" "ip_addr2" ...))`  
  The `ith` element of the list will be used for SSHing in the `ith` terminal. I don't know what will happen if you run `run-ssh` with insufficient number of IPs in `mr-hostnames-list`.  

  Define the variable `ssh-username` if you need to provide a username. The same username is used for all SSHs.

* All the functions and variables have `mr-` in their beginning to avoid name collisions. You can define shorthands (if they are safe). For example:

   > (defalias &#39;run &#39;multi-run)  
   > (defalias &#39;configure-terminals &#39;mr-configure-terminals)  
   > (defalias &#39;run-ssh &#39;multi-run-ssh)  
   > (defalias &#39;kill-timers &#39;mr-kill-all-timers)  
   > (defalias &#39;kill-terminals &#39;mr-kill-terminals)  
   > (defalias &#39;run-loop &#39;multi-run-loop)  
   > (defalias &#39;run-with-delay &#39;multi-run-with-delay)  
   > (defalias &#39;run-with-delay2 &#39;multi-run-with-delay2)

