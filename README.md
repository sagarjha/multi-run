# Introduction #
**Emacs** supports multiple terminal types including bash emulators (term, ansi-term), shell and eshell. Refer to [masteringemacs](https://www.masteringemacs.org/article/running-shells-in-emacs-overview){:target="_blank"} for an excellent overview of running shells in **Emacs**.

While this is great, **Emacs** lacks support for managing multiple terminals. I often need to work with multiple remote nodes at the same time. My example use-cases include:
1. <span style="color:orange">Synchronizing state:</span> Updating repos, building binaries, cd'ing to the right directories etc. on the remote nodes.
2. <span style="color:orange">Running distributed programs:</span> Specially for testing. These programs often have similar commands, arguments, input across all the nodes.
3. <span style="color:orange">Running interactive commands:</span> This includes debugging tools such as gdb.

While shell scripts can help support some of these tasks, specially synchronizing state and running distributed programs, they are a poor fit because:
1. <span style="color:orange"> Scripts are inflexible: </span> People often need to write different scripts for different use-cases. Moreover, any small change to the script involves changing the script file which interferes with the workflow.
2. <span style="color:orange"> Debugging scripts is messy</span>
3. <span style="color:orange"> Scripts do not support interactive commands:</span> For example, I haven't heard of any tool that will allow debugging distributed programs with gdb using a script.

Thus, what often works best in practice is to have multiple terminals visiting each node. With _multi-run_, one doesn't have to type the same command on each terminal (inefficient) or use copy and paste (cumbersome).

`multi-run` provides the following functionality:
1. <span style="color:orange"> Create and destroy multiple terminals:</span> Spawning multiple terminals or changing the working set of active terminals is as easy as running a command. Terminals are displayed in an **Emacs** frame for better visual connection, but can also be in the background if desired.
2. <span style="color:orange"> Run same (or similar) commands on multiple terminals:</span> Multiple such commands can be run in single invocation. In a lot of cases, the commands are similar, but not identical. For example, the input or command line arguments may slightly be different. With _multi-run_, one can run emacs-lisp lambda functions that produce a command when invoked with a terminal number.
3. <span style="color:orange"> Run commands with delays:</span> Some distributed programs need to run at the remote nodes with some in-between delay. One can run such commands with _multi-run_.
4. <span style="color:orange"> Run loops:</span> The same command can be run on all the terminals in a loop.

The salient feature with all _multi-run_ functions is that they are invoked as commands on a special terminal. This means that using _multi-run_ integrates nicely with the workflow and combining running _multi-run_ commands with single terminal commands is totally smooth.

The rest of this blog goes in depth on how to use _multi-run_, with embedded screencasts to help understand the usage better. Later sections include instructions on how to install the package and customization tips for a better experience.

# Features #
_multi-run_, written in `emacs-lisp`, is a thin wrapper around **Emacs** terminals. Its functions can be run as commands in an **_eshell_** buffer. For a primer on how to use **eshell** effectively, see 
[this](https://www.masteringemacs.org/article/complete-guide-mastering-eshell){:target="_blank"}.

## Creating terminals ##

``` emacs-lisp
(defun multi-run-configure-terminals (num-terminals &optional window-batch)
  "Lay out NUM-TERMINALS number of terminals on the Emacs frame with
  WINDOW-BATCH number of them in one single vertical slot." ...)
```

[//]: <> [![IMAGE ALT TEXT HERE](https://img.youtube.com/vi/VSpy815oJPQ/0.jpg)](https://www.youtube.com/watch?v=VSpy815oJPQ){:target="_blank"}
