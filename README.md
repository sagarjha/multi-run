<strong><font color="blueviolet" size="6">UNDER CONSTRUCTION!! CHECK BACK LATER!!!</font></strong>
# Introduction #
**Emacs** supports multiple terminal types including bash emulators (term, ansi-term), shell and eshell. Using terminals inside Emacs is appealing if you mix using Emacs with running terminal commands, because then you don't have to switch between environments. Refer to [mastering emacs](https://www.masteringemacs.org/article/running-shells-in-emacs-overview){:target="_blank"} for an excellent overview of running shells in Emacs.

While this is great, Emacs lacks support for managing multiple terminals. I often need to work with many remote nodes at the same time. My example use-cases include:
1. <span style="color:orange">Synchronizing state:</span> Updating repos, building binaries, cd'ing to the right directories etc. on the remote nodes.
2. <span style="color:orange">Running distributed programs:</span> Specially for testing. These programs often have similar commands, arguments and input etc. across all the nodes.
3. <span style="color:orange">Running interactive commands:</span> This includes debugging tools such as gdb.

While shell scripts can possibly help with synchronizing state and running distributed programs, they are a poor fit because:
1. <span style="color:orange"> Scripts are inflexible: </span> People often need to write different scripts for different use-cases. Moreover, any small change to the script involves changing the script file which interferes with the workflow.
2. <span style="color:orange"> Scripts are hard to debug</span>
3. <span style="color:orange"> Scripts do not support interactive commands:</span> For example, I haven't heard of any tool that will allow debugging distributed programs with gdb using a script.

Thus, what often works best in practice is to have multiple terminals visiting each node. With _multi-run_, one doesn't have to type the same command on each terminal (inefficient) or use copy-and-paste (cumbersome).

`multi-run` provides the following functionality:
1. <span style="color:orange"> Create and destroy multiple terminals:</span> Spawning multiple terminals or changing the working set of active terminals is as easy as running a command. Terminals are displayed in an Emacs frame for better visual connection, but can also be in the background if desired.
2. <span style="color:orange"> Run same (or similar) commands on multiple terminals:</span> Multiple such commands can be run in a single invocation. In a lot of cases, the commands are similar, but not identical. For example, the input or command line arguments may slightly be different. With _multi-run_, one can run emacs-lisp lambda functions that produce a command when invoked with a terminal number.
3. <span style="color:orange"> Run commands with delays:</span> Some distributed programs need to run at the remote nodes with some in-between delay (e.g. server needs to start before the client, clients must start one at a time). One can run such commands with _multi-run_.
4. <span style="color:orange"> Run loops:</span> The same command can be run on all the terminals in a loop. This can help with running performance tests and finding/fixing non-deterministic bugs, as we will see later.

The salient feature with all _multi-run_ functions is that they are invoked as commands on a special terminal. This ensures that _multi-run_ integrates nicely with the workflow: combining running _multi-run_ commands with single terminal commands is totally smooth because the terminal running the multi-run commands can also run normal commands.

The rest of this blog goes in depth on how to use _multi-run_, with embedded screencasts to help understand the usage better. Later sections include instructions on how to install the package and customization tips for a better experience.

<br>
<hr>
<br>

# Features #
_multi-run_, written in `emacs-lisp`, is a thin wrapper around Emacs terminals. Its functions can be run as commands in an **_eshell_** buffer (it's better to designate a terminal for this, called the <span style="color:teal"> _master_ _terminal_ </span> from now on). For a primer on how to use **eshell** effectively, see 
[this](https://www.masteringemacs.org/article/complete-guide-mastering-eshell){:target="_blank"}.

## Creating terminals ##
To create `n` terminals and display `k` of them in a single vertical slot, run `multi-run-configure-terminals n k` from the master terminal.

``` emacs-lisp
(defun multi-run-configure-terminals (num-terminals &optional window-batch)
  "Lay out NUM-TERMINALS number of terminals on the Emacs frame with
  WINDOW-BATCH number of them in one single vertical slot." ...)
```

configure-terminals names the terminal buffers as _eshell<1>_, _eshell<2>_ etc. (or _term<1>_, _<term<2>_ etc., if you are using term). If the buffers have already been created, configure-terminals can be used to display them back on the screen. This is convenient if they have been replaced by other Emacs buffers.

The following video shows how this works:

[![multi-run-configure-terminals](https://img.youtube.com/vi/VSpy815oJPQ/0.jpg)](https://www.youtube.com/watch?v=VSpy815oJPQ){:target="_blank"}

## Running commands ##
Having created terminals, run `multi-run cmd` to run `cmd` on all the terminals.

``` emacs-lisp
(defun multi-run (&rest cmd)
  "Run one or more commands CMD on multiple terminals." ...)
```

Multiple such commands enclosed in strings can be run this way. The commands will run concurrently in the active terminals and sequentially in each terminal. Managing set of _active_ terminals is explained later.

The following video shows examples of updating local repos at remote nodes and compiling binaries using _multi-run_:

[![multi-run](https://img.youtube.com/vi/wps_xdbqWzo/0.jpg)](https://www.youtube.com/watch?v=wps_xdbqWzo){:target="_blank"}

Each command can be a string or a <span style="color:teal"> _lambda_ </span> function that outputs a string when invoked with the terminal number. For example, consider nodes on a shared file system. We want to run a distributed program with input file input_i for node i. The multi-run command for this is:
``` emacs-lisp
(multi-run (lambda (num) (concat "./distributed_program < input_" (number-to-string num))))
```

If the active terminals are from 1 to 4, this will run `./distributed_program < input_1` in terminal 1, `./distributed_program < input_2` in terminal 2 and so on. Notice the parenthesis enclosing the entire command. This is necessary when using lambda functions.

multi-run effectively copies the commands to the terminals and inserts enter. As a result, it can also be abused to send inputs to the commands running in the terminals.

A nice application of lambda functions is with SSH. After creating terminals with configure-terminals, we want to ssh into a different node in every terminal. Since the hostnames, usernames etc. are different for each SSH, a lambda function can conveniently be used to get the right command for each SSH. However, this is still tedious and since SSH is very common anticipated use case, multi-run provides the function `multi-run-ssh`:
``` emacs-lisp
(defun multi-run-ssh (&optional terminal-num)
  "Establish ssh connections in the terminals (or in terminal number TERMINAL-NUM)
  with the help of user-defined variables." ...)
```

The function relies on the uesr to set the variable `multi-run-hostnames-list` to the list of hostnames (or IP addresses) and `multi-run-ssh-username` to the ssh username. The hostname for SSH in terminal `i` is the i^th entry in the list. The command ran is thus, `ssh username@hostname`. Currently, the same username is used for all SSH. If this variable is not set, then `ssh hostname` is run instead. For more control, I suggest configuring your `.ssh/config`, so that `ssh hostname` will automatically work. Finally, multi-run-ssh takes an optional argument, terminal-num, if SSH to only one terminal is desired (instead of to all active terminals).

``` emacs-lisp
(defvar multi-run-terminals-list nil
  "List of terminals to run the command on.")

(defvar multi-run-timers-list nil
  "Internal list of timers to cancel when multi-run-kill-all-timers is called.")
```

It is convenient to define these variables in the .emacs file if your target list does not change often:

``` emacs-lisp
(setq multi-run-hostnames-list (list "<hostname1>" "<hostname2>" ...)
```

## Active terminals ##
The set of active terminals specifies the target for the multi-run functions. Most often than not, your list of terminals are fixed for a given session, so you can forget that this exists. Whenever you create or redisplay the terminals using configure-terminals, the list is automatically updated to the list of terminals created/displayed. However, in some cases, manipulating this list is really handy. One common pattern is SSHing to some terminals, running few commands and then expanding the terminals to include more nodes (successfully tested on 2 nodes, now want to test on 4. For the record, one way to do this is to exit SSH - by `multi-run exit`, kill the terminals by running the same command again and then calling configure-terminals and run-ssh again).

The variable storing the list of terminals is:

``` emacs-lisp
(defvar multi-run-terminals-list nil
  "List of terminals to run the command on.")
```

Its value can be changed using emacs-lisp `setq` which can again be conveniently run in the master terminal itself. The following video shows how this is done:

[![multi-run-ssh-and-terminals-list](https://img.youtube.com/vi/KuyNbuuKlj4/0.jpg)](https://www.youtube.com/watch?v=KuyNbuuKlj4){:target="_blank"}

## Aborting commands ##
Consistent with our philosophy, we absolutely don't want to visit every terminal to abort commands (C-c C-c in eshell, similar to sending SIGINT by `Ctrl-C`). So, how to pass control characters such as `^C, ^D` to the terminals? Fortunately, Emacs provides the function `quoted-insert` (bound to `C-q` by default). Thus, `C-q C-c` will insert `^C` in any emacs buffer. Accordingly, the command

``` emacs-lisp
multi-run ^C
```

will abort any command run using multi-run.

Amazing!

Similarly, inserting `^D` can be useful when EOF needs to be inserted. Example: adding SSH public key to multiple nodes:

``` emacs-lisp
multi-run-configure-terminals n
multi-run-ssh
multi-run "cat >> .ssh/authorized_keys"
multi-run "<SSH key>"
multi-run ^D
```

<strong><font color="blueviolet" size="6">UNDER CONSTRUCTION!! CHECK BACK LATER!!!</font></strong>
