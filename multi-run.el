;;; multi-run.el --- Manage multiple terminals and run commands on them

;; Copyright (C) 2015-2018  Sagar Jha

;; Author: Sagar Jha
;; URL: https://www.github.com/sagarjha/multi-run
;; Package-Requires: ((emacs "24") (window-layout "1.4"))
;; Version: 1.0
;; Keywords: tools, terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The functions below are self-explanatory with the documentation string.

;; See the full documentation on https://www.github.com/sagarjha/multi-run.

;;; Code:

(require 'window-layout)
(require 'multi-run-helpers)

(defgroup multi-run nil
  "Run commands in multiple terminal windows."
  :group 'terminals)

(defvar multi-run-terminals-list nil
  "List of terminals to run the command on.")

(defvar multi-run-hostnames-list nil
  "List of hostnames for multi-run-ssh.")

(defvar multi-run-ssh-username nil
  "SSH username for multi-run-ssh.")

(defun multi-run-configure-terminals (&optional num-terminals window-batch)
  "Display NUM-TERMINALS number of terminals given by multi-run-terminals-list on the screen with WINDOW-BATCH number of them in one single vertical slot."
  (let* ((num-terminals (if num-terminals (progn (setq multi-run-terminals-list (number-sequence 1 num-terminals)) num-terminals) (length multi-run-terminals-list)))
	 (window-batch (if window-batch window-batch (calculate-window-batch num-terminals)))
	 (master-buffer-name (buffer-name))
	 (master-buffer-symbol (make-symbol "master"))
	 (sym-list (multi-run-make-symbols "term"))
	 (buffer-dict (cons (list :name master-buffer-symbol
				  :buffer master-buffer-name)
			    (multi-run-make-dict 'multi-run-get-buffer-name sym-list)))
	 (internal-recipe (multi-run-make-internal-recipe num-terminals window-batch (vconcat sym-list)))
	 (overall-recipe `(- (:upper-size-ratio 0.9)
			     ,internal-recipe ,master-buffer-symbol)))
    (multi-run-create-terminals)
    (wlf:layout
     overall-recipe
     buffer-dict)
    (select-window (get-buffer-window master-buffer-name))
    (concat "multi-run-terminals-list is " (prin1-to-string multi-run-terminals-list))))

(defun multi-run-with-delay (delay &rest cmd)
  "With the provided DELAY, run one or more commands CMD on multiple terminals - the delay is between running commands on different terminals."
  (let ((delay-now 0))
    (dolist (command cmd)
      (setq multi-run-timers-list (cons (run-at-time (concat (number-to-string delay-now) " sec") nil 'multi-run-on-terminals command multi-run-terminals-list delay) multi-run-timers-list))
      (setq delay-now (+ delay-now (* (length multi-run-terminals-list) delay)))))
  nil)

(defun multi-run-with-delay2 (delay &rest cmd)
  "With the provided DELAY, run one or more commands CMD on multiple terminals - but the delay is between different command invocations at the terminals."
  (let ((delay-now 0))
    (dolist (command cmd)
      (setq multi-run-timers-list (cons (run-at-time (concat (number-to-string delay-now) " sec") nil 'multi-run-on-terminals command multi-run-terminals-list) multi-run-timers-list))
      (setq delay-now (+ delay-now delay))))
  nil)

(defun multi-run (&rest cmd)
  "Run one or more commands CMD on multiple terminals."
  (apply #'multi-run-with-delay 0 cmd))

(defun multi-run-loop (cmd &optional times delay)
  "Loop CMD given number of TIMES with DELAY between successive run(s)."
  (unless times
    (setq times 1))
  (unless delay
    (setq delay 0))
  (let ((delay-cnt 0))
    (dotimes (i times)
      (progn
        (setq multi-run-timers-list (cons (run-at-time (concat (number-to-string (* delay-cnt delay)) " sec")
                                                       nil '(lambda (cmd) (multi-run cmd)) cmd) multi-run-timers-list))
        (setq delay-cnt (1+ delay-cnt))))))

(defun multi-run-ssh ()
  "Establish ssh connections in the terminals with the help of user-defined variables."
  (multi-run-on-terminals (lambda (x) (concat "ssh " (if multi-run-ssh-username
							 (concat multi-run-ssh-username "@") "")
					      (elt multi-run-hostnames-list (- x 1)))) multi-run-terminals-list))

(defun multi-run-find-remote-files-sudo (file-path &optional window-batch non-root)
  "Open file specified by FILE-PATH for all terminals and display them on the screen with WINDOW-BATCH number of them in one single vertical slot.  Open with sudo if NON-ROOT is false."
  (let* ((non-root (if non-root non-root nil))
	 (window-batch (if window-batch window-batch 5))
	 (master-buffer-name (buffer-name))
	 (master-buffer-symbol (make-symbol "master"))
	 (buffer-vector (vconcat (mapcar
				  (lambda (x) (find-file (concat "/ssh:"
								 (when multi-run-ssh-username
								   (concat multi-run-ssh-username "@"))
								 (elt multi-run-hostnames-list (- x 1))
								 (when (not non-root) (concat "|sudo:" (elt multi-run-hostnames-list (- x 1))))
								 ":" file-path)))
				  multi-run-terminals-list)))
	 (num-terminals (length multi-run-terminals-list))
	 (sym-list (multi-run-make-symbols "file"))
	 (buffer-dict (cons (list :name master-buffer-symbol
				  :buffer master-buffer-name)
			    (multi-run-make-dict (lambda (cnt) (aref buffer-vector (1- cnt))) sym-list)))
	 (internal-recipe (multi-run-make-internal-recipe num-terminals window-batch (vconcat sym-list)))
	 (overall-recipe `(- (:upper-size-ratio 0.9)
			     ,internal-recipe ,master-buffer-symbol)))
    (wlf:layout
     overall-recipe
     buffer-dict)
    (select-window (get-buffer-window master-buffer-name)))
  nil)

(defun multi-run-find-remote-files (file-path &optional window-batch)
  "Open file specified by FILE-PATH for all terminals and display them on the screen with WINDOW-BATCH number of them in one single vertical slot."
  (multi-run-find-remote-files-sudo file-path window-batch 't))

(defun multi-run-copy-sudo (file1 file-or-directory1 &rest files)
  "Copy FILE1 to FILE-OR-DIRECTORY1.  With multiple arguments, the last element FILES is the destination directory, the rest are source files to copy."
  (apply 'multi-run-copy-generic 'multi-run-copy-one-file-sudo file1 file-or-directory1 files))

(defun multi-run-copy (file1 file-or-directory1 &rest files)
  "Copy FILE1 to FILE-OR-DIRECTORY1.  With multiple arguments, the last element FILES is the destination directory, the rest are source files to copy."
  (apply 'multi-run-copy-generic 'multi-run-copy-one-file file1 file-or-directory1 files))

(defun multi-run-kill-terminals ()
  "Kill active terminals."
  (mapc (lambda (terminal-num) (kill-buffer (multi-run-get-buffer-name terminal-num))) multi-run-terminals-list)
  nil)

(defun multi-run-kill-all-timers ()
  "Cancel commands running on a loop or via delay functions."
  (mapc 'cancel-timer multi-run-timers-list)
  "All timers canceled")


(provide 'multi-run)
;;; multi-run.el ends here
