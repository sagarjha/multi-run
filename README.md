A minor mode for managing multiple emacs eshell terminals and running commands on them

# Getting Started

To be able to access the most basic functionality, run `multi-run-configure-terminals` with two arguments, the number of terminals you want to create and the number of terminals to stack in a single vertical window. For example, running `multi-run-configure-terminals 8 3` in an eshell buffer will create 3 vertical panes, with the first two panes containing 3 terminals each and the last pane containing the remaining two terminals. After you have created the terminals, run `multi-run command` to run the desired command from the same eshell buffer. See my blog (to be linked here) for a more detailed guide on how to use the library effectively.

# Functions

Following is a list of important variables and functions for a user of the library. 

(defvar multi-run-term-type ...  
  &quot;Terminal type to run the commands on&quot;)

(defvar multi-run-terminals-list ...  
  &quot;List of terminals to run the command on.&quot;)

(defvar multi-run-hostnames-list ...  
  &quot;List of hostnames for multi-run-ssh.&quot;)

(defvar multi-run-ssh-username ...  
  &quot;SSH username for multi-run-ssh.&quot;)

(defun multi-run-configure-terminals (num-terminals &optional window-batch)  
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

(defun multi-run-kill-terminals (&optional terminal-num)  
  &quot;Kills terminals&quot;  
  ...)

(defun multi-run-kill-all-timers ()  
  &quot;Cancels commands running on a loop or via delay functions&quot;  
  ...)


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

  Provide the list of IP-addreses/hostnames for run-ssh to work. For this, define the variable `multi-run-hostnames-list`.  
    `(setq multi-run-hostnames-list (list "ip_addr1" "ip_addr2" ...))`  
  The `ith` element of the list will be used for SSHing in the `ith` terminal. I don't know what will happen if you run `run-ssh` with insufficient number of IPs in `multi-run-hostnames-list`.  

  Define the variable `multi-run-ssh-username` if you need to provide a username. The same username is used for all SSHs.

* All the functions and variables have `multi-run-` in their beginning to avoid name collisions. You can define shorthands (if they are safe). For example:

   > (defalias &#39;run &#39;multi-run)  
   > (defalias &#39;configure-terminals &#39;multi-run-configure-terminals)  
   > (defalias &#39;run-ssh &#39;multi-run-ssh)  
   > (defalias &#39;kill-timers &#39;multi-run-kill-all-timers)  
   > (defalias &#39;kill-terminals &#39;multi-run-kill-terminals)  
   > (defalias &#39;run-loop &#39;multi-run-loop)  
   > (defalias &#39;run-with-delay &#39;multi-run-with-delay)  
   > (defalias &#39;run-with-delay2 &#39;multi-run-with-delay2)

