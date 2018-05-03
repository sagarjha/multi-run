<strong><font color="blueviolet" size="6">UNDER CONSTRUCTION!! CHECK BACK LATER!!!</font></strong>
# Introduction #
_Emacs_ supports multiple terminal types including bash emulators (term, ansi-term), shell and eshell. Using terminals inside Emacs helps one avoid the need to switch environments for running shell commands. Refer to [mastering emacs](https://www.masteringemacs.org/article/running-shells-in-emacs-overview) for an excellent overview of using terminals in Emacs.

While this is great, Emacs lacks support for managing multiple terminals. I often need to work with many remote nodes at the same time. My example use-cases include:
1. <span style="color:orange">Synchronizing state:</span> Updating repos, building binaries, cd'ing to the right directories etc. on the remote nodes.
2. <span style="color:orange">Running distributed programs:</span> Specially for testing. These programs often have similar commands, arguments and input etc. across all the nodes.
3. <span style="color:orange">Running interactive commands:</span> These include debugging tools such as gdb.

While shell scripts can possibly help with synchronizing state and running distributed programs, they are a poor fit because:
1. <span style="color:orange"> Scripts are inflexible: </span> People often need to write different scripts for different use-cases. Any small change to the script involves changing the script file which interferes with the workflow.
2. <span style="color:orange"> Scripts are hard to debug</span>

Additionally, they do not support interactive commands. Thus, what often works best in practice is to have multiple terminals visiting each node. With _multi-run_, one doesn't have to type the same command on each terminal (inefficient) or use copy-and-paste (cumbersome).

_multi-run_ provides the following functionality:
1. <span style="color:orange"> Create and destroy multiple terminals:</span> Spawning multiple terminals or changing the working set of active terminals is as easy as running a command. Terminals are displayed in an Emacs frame for better visual connection, but can also be in the background if desired.
2. <span style="color:orange"> Run same (or similar) commands on multiple terminals:</span> Multiple such commands can be run in a single invocation. In a lot of cases, the commands are similar, but not identical. For example, the input or command line arguments may slightly be different. With _multi-run_, one can run emacs-lisp lambda functions that produce a command when invoked with a terminal number.
3. <span style="color:orange"> Run commands with delays:</span> Some distributed programs need to run at the nodes with some in-between delay (e.g. server needs to start before the client, clients must start one at a time). One can run such commands with _multi-run_.
4. <span style="color:orange"> Run loops:</span> The same command can be run on all the terminals in a loop. This can help with running performance tests and finding/fixing non-deterministic bugs, as we will see later.

The salient feature with all _multi-run_ functions is that they are invoked as commands on a special terminal. This ensures that _multi-run_ integrates nicely with the workflow: combining running _multi-run_ commands with single terminal commands is totally smooth because the terminal running the multi-run commands can also run normal commands.

The rest of this blog goes in depth on how to use _multi-run_, with embedded screencasts to help understand the usage better. Later sections include instructions on how to install the package and customization tips for a better experience.

<br>
<hr>
<br>

# Features #
_multi-run_, written in _emacs-lisp_, is a thin wrapper around Emacs terminals. Its functions can be run as commands in an _eshell_ buffer (it's better to designate a terminal for this, called the <span style="color:teal"> _master_ _terminal_ </span> from now on). For a primer on how to use eshell effectively, see 
[this](https://www.masteringemacs.org/article/complete-guide-mastering-eshell).

## Creating terminals ##
To create _n_ terminals and display _k_ of them in a single vertical slot, run _multi-run-configure-terminals n k_ from the master terminal.

``` emacs-lisp
(defun multi-run-configure-terminals (num-terminals &optional window-batch)
  "Lay out NUM-TERMINALS number of terminals on the Emacs frame with
  WINDOW-BATCH number of them in one single vertical slot." ...)
```

configure-terminals names the terminal buffers as _eshell<1>_, _eshell<2>_ etc. (or _term<1>_, _<term<2>_ etc., if you are using term). If the buffers have already been created, configure-terminals can be used to display them back on the screen. This is convenient if they have been replaced by other Emacs buffers.

The following video shows how this works:

[![multi-run-configure-terminals](https://img.youtube.com/vi/VSpy815oJPQ/0.jpg)](https://www.youtube.com/watch?v=VSpy815oJPQ)

## Running commands ##
Having created terminals, run _multi-run cmd_ to run cmd on all the terminals.

``` emacs-lisp
(defun multi-run (&rest cmd)
  "Run one or more commands CMD on multiple terminals." ...)
```

Multiple such commands enclosed in strings can be run this way. The commands will run concurrently in the active terminals and sequentially in each terminal. Managing set of _active_ terminals is explained later.

The following video shows examples of updating local repos at remote nodes and compiling binaries using _multi-run_:

[![multi-run](https://img.youtube.com/vi/wps_xdbqWzo/0.jpg)](https://www.youtube.com/watch?v=wps_xdbqWzo)

Each command can be a string or a <span style="color:teal"> _lambda_ </span> function that outputs a string when invoked with the terminal number. As an example of running a function, consider nodes on a shared file system. We want to run a distributed program with input file input_i for node i. The multi-run command for this is:
``` emacs-lisp
(multi-run (lambda (num) (concat "./distributed_program < input_" (number-to-string num))))
```

If the active terminals are from 1 to 4, this will run _./distributed_program < input_1_ in terminal 1, _./distributed_program < input_2_ in terminal 2 and so on. Notice the parenthesis enclosing the entire command. This is necessary with lambda functions.

multi-run effectively copies the commands to the terminals and inserts enter. This means that it can be abused to send inputs to the commands running in the terminals.

A nice application of lambda functions is with SSH. After creating terminals with configure-terminals, we want to ssh into a different node in every terminal. Since the hostnames, usernames etc. are different for each SSH, a lambda function can conveniently be used to get the right command for each SSH. However, this is still tedious and since SSH is very common anticipated use case, multi-run provides the function _multi-run-ssh_:
``` emacs-lisp
(defun multi-run-ssh (&optional terminal-num)
  "Establish ssh connections in the terminals (or in terminal number TERMINAL-NUM)
  with the help of user-defined variables." ...)
```

The function expects the user to have set the variable _multi-run-hostnames-list_ to the list of hostnames (or IP addresses) and _multi-run-ssh-username_ to the ssh username. The hostname for SSH in terminal _i_ is the i^th entry in the list. The username is optional (as with SSH) and is the same for all the commands. It is best to edit .ssh/config for more configurability. Finally, multi-run-ssh takes an optional argument, terminal-num, if SSH to only one terminal is desired (instead of to all active terminals).

``` emacs-lisp
(defvar multi-run-hostnames-list nil
  "List of hostnames for multi-run-ssh.")

(defvar multi-run-ssh-username nil
  "SSH username for multi-run-ssh.")
```

It is convenient to set these in your _.emacs_ if you often work with the same set of nodes:

``` emacs-lisp
(setq multi-run-hostnames-list (list "<hostname1>" "<hostname2>" ...)
```

## Active terminals ##
The list of active terminals specifies the target for the multi-run functions. Calling configure-terminals automatically sets this list to the terminals displayed. If your list of terminals are fixed for a given session, then you do not need to set it manually. However, in some cases, manipulating it is really handy. One common pattern is SSHing to some terminals, running few commands and then expanding the terminals to include more nodes (successfully tested on 2 nodes, now want to test on 4...).

``` emacs-lisp
(defvar multi-run-terminals-list nil
  "List of terminals to run the command on.")
```

_multi-run-terminals-list_ can be set using emacs-lisp _setq_ which can again be conveniently run in the master terminal.

The following video shows examples of SSHing to nodes and changing active terminals:

[![multi-run-ssh-and-terminals-list](https://img.youtube.com/vi/KuyNbuuKlj4/0.jpg)](https://www.youtube.com/watch?v=KuyNbuuKlj4)

## Aborting commands ##
Consistent with our philosophy, we absolutely don't want to visit every terminal to abort commands (C-c C-c in eshell, similar to sending SIGINT by _Ctrl-C_). So, how to pass control characters such as _^C, ^D_ to the terminals? Fortunately, Emacs provides the function _quoted-insert_ (bound to _C-q_ by default). So the command_C-q C-c_ will insert _^C_ in any emacs buffer. Accordingly, the command

``` emacs-lisp
multi-run ^C
```

will abort any command run using multi-run.

This is amazing IMO!

Similarly, inserting _^D_ can be useful when EOF needs to be inserted. Example: adding SSH public key to multiple nodes:

``` emacs-lisp
multi-run-configure-terminals n
multi-run-ssh
multi-run "cat >> .ssh/authorized_keys"
multi-run "<SSH key>"
multi-run ^D
```

<strong><font color="blueviolet" size="6">UNDER CONSTRUCTION!! CHECK BACK LATER!!!</font></strong>
