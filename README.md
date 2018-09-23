See [https://sagarjha.github.io/multi-run](https://sagarjha.github.io/multi-run) for a full set of features. Below you will find a brief summary of features that are new:

## New Features ##

multi-run now supports opening multiple remote files with TRAMP:

``` emacs-lisp
(defun multi-run-find-remote-files (file-path &optional window-batch)
  "Open file specified by FILE-PATH for all terminals and display them on the screen with WINDOW-BATCH number of them in one single vertical slot.")	

(defun multi-run-find-remote-files-sudo (file-path &optional window-batch non-root)
  "Open file specified by FILE-PATH for all terminals and display them on the screen with WINDOW-BATCH number of them in one single vertical slot.  Open with sudo if NON-ROOT is false.")
```
