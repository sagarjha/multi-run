# Introduction #
_Emacs_ supports multiple terminal types including bash emulators (term, ansi-term), shell and eshell. Using terminals inside Emacs helps one avoid the need to switch environments for running shell commands. Refer to [mastering emacs](https://www.masteringemacs.org/article/running-shells-in-emacs-overview) for an excellent overview of using terminals in Emacs.

While this is great, Emacs lacks support for managing multiple terminals. I often need to work with many remote nodes at the same time. My example use-cases include:
1. <span style="color:orange">Synchronizing state:</span> Updating repos, building binaries, cd'ing to the right directories etc. on the remote nodes.
2. <span style="color:orange">Running distributed programs:</span> Specially for testing. These programs often have similar commands, arguments and input etc. across all the nodes.
3. <span style="color:orange">Running interactive commands:</span> These include debugging tools such as gdb.

While shell scripts can possibly help with synchronizing state and running distributed programs, they are a poor fit because:
1. <span style="color:orange"> Scripts are inflexible: </span> People often need to write different scripts for different use-cases. Any small change to the script involves changing the script file which interferes with the workflow.
2. <span style="color:orange"> Scripts are hard to debug</span>

Additionally, they do not support interactive commands. Thus, what often works best in practice is to have multiple terminals visiting each node. With _multi-run_, one doesn't have to type the same command in each terminal (inefficient) or use copy-and-paste (cumbersome).

_multi-run_ provides the following functionality:
1. <span style="color:orange"> Create and destroy multiple terminals:</span> Spawning multiple terminals or changing the working set of active terminals is as easy as running a command. Terminals are displayed in an Emacs frame for better visual connection, but can also be in the background if desired.
2. <span style="color:orange"> Run same (or similar) commands in multiple terminals:</span> Multiple such commands can be run in a single invocation. In a lot of cases, the commands are similar, but not identical. For example, the input or command line arguments may slightly be different. With multi-run, one can run emacs-lisp lambda functions that produce a command when invoked with a terminal number.
3. <span style="color:orange"> Run commands with delays:</span> Some distributed programs need to run at the nodes with some in-between delay (e.g. server needs to start before the client, clients must start one at a time). One can run such commands with multi-run.
4. <span style="color:orange"> Run loops:</span> The same command can be run in all the terminals in a loop. This can help with running performance tests and finding non-deterministic bugs.

The salient feature with all multi-run functions is that they are invoked as commands in a special terminal. This ensures that multi-run integrates nicely with the workflow: combining running multi-run commands with single terminal commands is totally smooth because the terminal running the multi-run commands can also run normal commands.

The rest of this blog goes in depth on how to use the tool, with embedded screencasts to help understand the usage better. Later sections include instructions on how to install the package and customization tips for a better experience.

<br>
<hr>
<br>

# Features #
multi-run, written in _emacs-lisp_, is a thin wrapper around Emacs terminals. Its functions can be run as commands in an _eshell_ buffer (it's better to designate a terminal for this, called the <span style="color:teal"> _master_ _terminal_ </span> from now on). For a primer on how to use eshell effectively, see 
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
Having created terminals, run _multi-run cmd_ to run cmd in all the terminals.

``` emacs-lisp
(defun multi-run (&rest cmd)
  "Run one or more commands CMD in multiple terminals." ...)
```

Multiple such commands enclosed in strings can be run this way. The commands will run concurrently in the active terminals and sequentially in each terminal. Managing set of _active_ terminals is explained later.

The following video shows examples of updating local repos at remote nodes and compiling binaries using multi-run:

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
Consistent with our philosophy, we absolutely don't want to visit every terminal to abort commands (C-c C-c in eshell, similar to sending SIGINT by _Ctrl-C_). So, how to pass control characters such as _^C, ^D_ to the terminals? Fortunately, Emacs provides the function _quoted-insert_ (bound to _C-q_ by default). So the command_C-q C-c_ will insert ^C in any emacs buffer. Accordingly, the command

``` emacs-lisp
multi-run ^C
```

will abort any command run using multi-run.

This is simply amazing in my opinion!

Similarly, inserting _^D_ can be useful when EOF needs to be inserted. Example: adding SSH public key to multiple nodes:

``` emacs-lisp
multi-run-configure-terminals n
multi-run-ssh
multi-run "cat >> .ssh/authorized_keys"
multi-run "<SSH key>"
multi-run ^D
```

## Commands with delay ##
multi-run provides the function _multi-run-wth-delay_, that introduces a delay between executing a command at different terminals.

```emacs-lisp
(defun multi-run-with-delay (delay &rest cmd)
  "With the provided DELAY, run one or more commands CMD in multiple terminals
  - the delay is between running commands in different terminals." ...)
```

For example, if you run _multi-run-with-delay 0.3 cmd_, it will run _cmd_ in terminal 1 immediately, in terminal 2 at 0.3 seconds, in terminal 3 at 0.6 seconds and so on. If multiple commands are provided as arguments, the second command will run after the first command has been run at all the terminals.

There's a variant of this called _multi-run-with-delay2_ (for lack of a better name). It has the same arguments, but runs each command in every terminal at the same time. The delay is only applied between command executions.

## Loops ##
A command can be run multiple times in a loop with the function _multi-run-loop_:

```emacs-lisp
(defun multi-run-loop (cmd &optional times delay)
  "Loop CMD given number of TIMES with DELAY between successive run(s)." ...)
```

In every invocation of the loop, _cmd_ will run in all terminals (equivalent to _multi-run cmd_). The delay is applied between different loop invocations.

The reason we need to provide a delay is that in general, it is not possible to know from emacs when the commands have finished executing. This limitation forces the user to provide an upper-bounds on the execution times of the commands. In many cases, even running commands without delay is not an issue if the commands are successfully queued up and run later by the terminal (depends on the terminal type you're working with). Loops can help in finding non-deterministic bugs and running performance/correctness tests, something we will see later in more detail.

Just like in _multi-run_, the command can be a string or a lambda function. The lambda function is called with the terminal number to obtain the command string. With some familiarity with emacs-lisp, it is possible to write more sophisticated functions that manage state internally to make use of other parameters such as the iteration number.

So how does one abort loops? With the delay and loop functions, multi-run registers multiple timers with emacs that run the commands in the future. The timer references are stored internally by multi-run so that it can later ask emacs to cancel them. Consequently, the function _multi-run-kill-all-timers_ aborts all commands that were scheduled to run in the future. This is specially handy when you start a wrong command in a loop.

```emacs-lisp
(defun multi-run-kill-all-timers ()
  "Cancel commands scheduled by multi-run-loop or multi-run-with-delay." ...)
```

The following video shows the basic features of running a simple command in a loop, running a lambda function, and aborting loops.

[![multi-run-loop](https://img.youtube.com/vi/lnIQst7yqKM/0.jpg)](https://www.youtube.com/watch?v=lnIQst7yqKM)

## Terminal types ##
multi-run is just a wrapper around Emacs supported terminals. And Emacs supports multiple terminal types including bash emulators (term, multi-term, ansi-term) and emacs shells (shell, eshell). The terminal running the multi-run commands must be _eshell_ (or any elisp shell), but the target terminals can be any of the above types. I highly recommend using the default type eshell, however, you might want to use a bash emulator because:
1. eshell has its own idiomatic way of using it that can take time getting used to. For example, its tab completion, command search in history etc. work differently.
2. eshell is not an emulation of bash. "Most" bash commands are supported and will work as expected. Furthermore, eshell does not support input redirection.
<br>Both these points aren't much relevant if you are working on remote nodes, because then you will be using SSH which will run bash (or equivalent) inside the eshell terminal.
3. eshell integration with shell processes is not satisfactory. You will not be able to access command history or use tab-completion in SSH.

The following image contrasts syntax coloring in eshell (left) vs term (right):

![]({{ site.baseurl }}/docs/mr_syntax_coloring.png?raw=true)
*Syntax coloring - eshell vs term*

multi-run supports _term_, _multi-term_, _ansi-term_ and _shell_ apart from _eshell_. To change the terminal type, set the variable _multi-run-term-type_.

```emacs-lisp
(defcustom multi-run-term-type 'eshell
  "Terminal type to run the commands on.")
```

After changing the terminal type, you can start from multi-run-configure-terminals to spawn terminals of the set terminal type. All multi-run functions will work automatically.

<br>
<hr>
<br>

# Installation #
multi-run is released under GNU GENERAL PUBLIC LICENSE Version 3.

It is part of Emacs' package manager _MELPA_. Installing it from MELPA is the easiest option.

Alternatively, you can download the [v1-release](https://github.com/sagarjha/multi-run/releases/tag/v1) from github. You just need the file _multi-run.el_. Store it in a directory included in Emacs' load-path and load it by adding
```emacs-lisp
(require 'multi-run)
```
to the _.emacs_ file. If you are installing manually this way, you will also have to install [window-layout](https://github.com/kiwanami/emacs-window-layout) that multi-run depends on.

<br>
<hr>
<br>

# Useful Emacs customization for multi-run #
1. After you run a command, you will want the buffers showing the terminals to automatically scroll past the output to the next prompt. Emacs will not automatically scroll to the bottom of the output unless you set the variables _eshell-scroll-to-bottom-on-output_ (for eshell), _comint-scroll-to-bottom-on-output_ (for shell) and _term-scroll-to-bottom-on-output_ (for term). A convenient way to set these is by adding the following lines in your _.emacs_:
```emacs-lisp
(custom-set-variables
  '(comint-scroll-to-bottom-on-output (quote all))
  '(eshell-scroll-to-bottom-on-output (quote all))
  '(term-scroll-to-bottom-on-output (quote all)))
```
2. Every one of multi-run functions has a prefix of multi-run- in its name. This branding is necessary to avoid name collisions as emacs-lisp does not support namespaces. You can define shorthands to these functions, for example, _run_ for _multi-run_, _configure-terminals_ for _multi-run-configure-terminals_ etc. provided the shorthands do not collide with existing names. You can add these in your _.emacs_ file. For example,
```emacs-lisp
(defalias 'run 'multi-run)
(defalias 'configure-terminals 'mr-configure-terminals)
(defalias 'run-ssh 'multi-run-ssh)
(defalias 'kill-timers 'multi-run-kill-all-timers)
(defalias 'kill-terminals 'multi-run-kill-terminals)
(defalias 'run-loop 'multi-run-loop)
(defalias 'run-with-delay 'multi-run-with-delay)
```
3. Since you are likely going to work with many windows (buffers on the screen), turning on _visual-line-mode_ is a good idea. It will wrap words around the window edges so that you can look at the entire output.\\
The following image shows the difference. In the left half, entire directory contents are not visible because of no line wrapping.

![]({{ site.baseurl }}/docs/before_and_after_visual_line_mode.png?raw=true)
*Before and after visual line mode - without line wrapping, entire directory contents are not visible*

If you notice no wrapping, add this to your _.emacs_:
```
(add-hook 'eshell-mode-hook 'visual-line-mode)
```
4. In some cases, you might want to jump to a specific terminal buffer. If you have many buffers on the screen, using _other-window_ (bound to _C-x o_) is inefficient. You can install packages [window-number](https://github.com/nikolas/window-number) or [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode/) to efficiently move between windows. To be able to undo changes in window configuration, enable _winner-mode_.

5. TRAMP (Transparent Remote (file) Access, Multiple Protocol) mode in Emacs provides features for working with files stored on remote nodes.

<br>
<hr>
<br>

# Advanced examples #
## gdb ##
For this example, suppose our distributed program has a rare non-deterministic bug. In the buggy execution, all nodes hang while in the correct execution, they all exit cleanly before 20 seconds. We don't want to manually run each execution inside gdb using multi-run in hopes of hitting a buggy run. Instead, we can use multi-run-loop to start a loop running the program in gdb. The good thing is that when the program hits the bug, successive loop iterations have no effect since the previous iteration is not complete. This means that you can leaeve the loop running in the background and check periodically if the bug has been triggered.

The following video shows exactly this. I start a loop running 200 iterations with a 25 second delay. The buggy execution is triggered in the 85^th iteration of the loop, after more than 35 minutes. I then debug in gdb after aborting the loop and interrupting the program.

[![multi-run-loop](https://img.youtube.com/vi/t5SYBEicqsw/0.jpg)](https://www.youtube.com/watch?v=t5SYBEicqsw)

Similarly, programs can be run in a loop (with or without gdb) to loosely verify that they are working every time. But, what if the programs do not exit in a correct execution, but enter an infinite loop instead? You can interleave a loop that runs ^C so that the next run can start!  The ability to write a multi-run script expressing these timing constraints (for multiple loops) will be very helpful here. Eshell provides a scripting functionality; I am going to look into how multi-run can integrate nicely with it.

## Performance tests ##
In pretty much the same way, you can write functions that cycle through the parameters and produce commands for each set of parameters. Then you can run those functions with _multi-run-loop_. Scripts can do the same thing, but with multi-run you will have more information if something goes wrong. You can also combine running performance tests with processing data in emacs-lisp and plotting the results.

## Miscellaneous ##
I created some unconventional (sometimes amusing, somewhat creative) examples of using multi-run. These are most likely not going to be relevant to you. You can find them <a href="misc.html" target="_self">here</a>.

<br>
<hr>
<br>

# Conclusion #
Development of multi-run has been guided by my use cases. I hope you like the package. If you have ideas on how to improve it, send me an email at [srj57@cornell.edu](mailto:srj57@cornell.edu).

I wrote the first draft back in April 2016 with just one function multi-run. I added the delay commands in April 2017. I created the videos and applied for adding the package to MELPA in December 2017. The package was added to MELPA in January 2018 after some code cleaning/refactoring (thanks to Steve Purcell from MELPA). I released v1 right after in January. I wrote this blog and added the videos to YouTube during April and May 2018. You can find the full playlist of multi-run videos [here](https://www.youtube.com/playlist?list=PLuorXJMjI9I34tfnJaIV9IalE0UBXB0VF). After having used Emacs for more than 6 years, I am pleased to have contributed something back to it.

Some people have argued against using emacs shells, specially eshell, for some justifiable reasons. eshell is not very well documented and many of its features can be improved. Hopefully, this article makes a case for using eshell by showing how easily it integrates with Emacs. multi-run is just 245 lines of emacs-lisp including comments and newlines.

If you use Emacs for basic editing (and/or for writing code, latex, emails etc.), you should consider switching to using terminals inside Emacs. I believe this will integrate better with your workflow, specially if you mix writing code and running shell commands.
