multi-run is an Emacs package that manages multiple terminals and runs commands in them. This blog explains its basic features.

# Introduction
I started using Emacs for basic editing, but over time my workflow evolved to be centered around Emacs. Emacs provides awesome integration for using git ([Magit](https://magit.vc/){:target="_blank"}), latex ([Auctex](https://www.gnu.org/software/auctex/){:target="_blank"}) and for organizing notes ([org-mode](https://orgmode.org/){:target="_blank"}). The list goes on and on. Similarly, Emacs has great support for shells/terminals.

Emacs supports multiple terminal types including bash emulators (term, ansi-term), shell and eshell. Mickey Petersen provided an excellent overview of running shells in emacs at [https://www.masteringemacs.org/article/running-shells-in-emacs-overview](https://www.masteringemacs.org/article/running-shells-in-emacs-overview){:target="_blank"}. In particular, I would like to single out eshell. Eshell can be used on any platform that can run Emacs, including Wiindows, and comes with native support for running emacs-lisp. Again, refer to masteringemacs, [here](https://www.masteringemacs.org/article/complete-guide-mastering-eshell){:target="_blank"}, for a primer on how to use eshell effectively. multi-run is a wrapper around different emacs terminal types that enables running commands simultaneously on multiple such terminals.

I come from a distributed systems background and as such, I need to manage multiple remote nodes at the same time. My example use-cases include:
1. Synchronizing state: Updating repos, building binaries, cd'ing to the right directories etc. on the remote nodes.
2. Running distributed programs: Specially for testing. These programs often have similar commands, arguments, input across all the nodes.
3. Running interactive commands: This includes debugging tools such as gdb.

While shell scripts can help support some of these tasks, specially synchronizing state and running distributed programs, they are a poor fit because:
1. Scripts are inflexible: People often need to write different scripts for different use-cases. Moreover, any small change to the script involves changing the script file which interferes with the workflow.
2. Messy error handling: It is hard to debug shell scripts.
3. Scripts do not support interactive commands: For example, I haven't heard of any tool that will allow debugging distributed programs with gdb using a script.

A proper solution for managing multiple remote nodes for the above use-cases is to have multiple terminals visiting each one of them. Since I don't want to type the same command on each terminal (inefficient) or use copy and paste (cumbersome), I wrote multi-run to do that for me. An alternative for multi-run is [tmux's synchronization feature](https://sanctum.geek.nz/arabesque/sync-tmux-panes/){:target="_blank"}, but that is very primitive and does not support more advanced features I need. Moreover, it is not for Emacs.

multi-run provides the following functionality:
1. Create and destroy multiple terminals: Spawning multiple terminals or changing the working set of active terminals is as easy as running a command. Terminals are displayed in an emacs frame for better visual connection, but can also be in the background if desired.
2. Run same (or similar) commands on multiple terminals: Multiple such commands can be run in single invocation. In a lot of cases, the commands are similar, but not identical. For example, the input or command line arguments may slightly be different. With multi-run, one can run emacs-lisp's lambda functions that produce a command when invoked with a terminal number.
3. Run commands with delays: Some distributed programs need to run at the remote nodes with some in-between delay. One can run such commands with multi-run.
4. Run loops: The same command can be run on all the terminals in a loop.

The salient feature with all multi-run functions is that they are invoked as commands on a special terminal. This means that using multi-run integrates nicely with the workflow and combining running multi-run commands with single terminal commands is totally smooth.

The rest of this blog goes in depth on how to use multi-run, with embedded screencasts to help understand the usage better. Later sections include instructions on how to install multi-run and explanation of emacs customization tips for a better experience using the package.
