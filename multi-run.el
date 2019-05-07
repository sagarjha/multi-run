;;; multi-run.el --- Efficiently manage multiple remote nodes  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019  Sagar Jha

;; Author: Sagar Jha
;; URL: https://www.github.com/sagarjha/multi-run
;; Package-Requires: ((emacs "24") (window-layout "1.4"))
;; Version: 1.0
;; Keywords: multiple shells, multi-run, remote nodes

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

;; See the full documentation at https://www.github.com/sagarjha/multi-run.

;;; Code:

(require 'window-layout)
(require 'multi-run-vars)
(require 'multi-run-helpers)

(defgroup multi-run nil
  "Run commands in multiple terminal windows."
  :group 'terminals)

(defun multi-run-configure-terminals (&optional num-terminals window-batch)
  "Display NUM-TERMINALS number of terminals given by multi-run-terminals-list on the screen with WINDOW-BATCH number of them in one single vertical slot."
  (let* ((master-buffer-name (buffer-name))
	 (num-terminals (if num-terminals
			    (progn (setq multi-run-terminals-list (number-sequence 1 num-terminals)) num-terminals)
			  (length multi-run-terminals-list)))
	 (window-batch (if window-batch window-batch (calculate-window-batch num-terminals))))
    (multi-run-create-terminals)
    (multi-run-display-buffers master-buffer-name num-terminals window-batch "term" 'multi-run-get-buffer-name)
    (concat "multi-run-terminals-list is " (prin1-to-string multi-run-terminals-list))))

(defun multi-run-with-delay (delay &rest cmd)
  "With the provided DELAY, run one or more commands CMD on multiple terminals - the delay is between running commands on different terminals."
  (let ((delay-now 0))
    (dolist (command cmd)
      (setq multi-run-timers-list (cons (run-at-time (concat (number-to-string delay-now) " sec") nil 'multi-run-on-terminals command multi-run-terminals-list delay) multi-run-timers-list))
      (setq delay-now (+ delay-now (* (length multi-run-terminals-list) delay))))
    nil))

(defun multi-run-with-delay2 (delay &rest cmd)
  "With the provided DELAY, run one or more commands CMD on multiple terminals - but the delay is between different command invocations at the terminals."
  (let ((delay-now 0))
    (dolist (command cmd)
      (setq multi-run-timers-list (cons (run-at-time (concat (number-to-string delay-now) " sec") nil 'multi-run-on-terminals command multi-run-terminals-list) multi-run-timers-list))
      (setq delay-now (+ delay-now delay)))
    nil))

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
  (multi-run-on-terminals (lambda (term-num) (concat "ssh " (if multi-run-ssh-username
								(concat multi-run-ssh-username "@") "")
						     (elt multi-run-hostnames-list (1- term-num)))) multi-run-terminals-list))

(defun multi-run-find-remote-files-sudo (file-path &optional window-batch non-root)
  "Open file specified by FILE-PATH for all terminals and display them on the screen with WINDOW-BATCH number of them in one single vertical slot.  Open with sudo if NON-ROOT is false."
  (let* ((master-buffer-name (buffer-name))
	 (non-root (if non-root non-root nil))
	 (num-terminals (length multi-run-terminals-list))
	 (window-batch (if window-batch window-batch (calculate-window-batch num-terminals)))
	 (term-num-buffer-list (mapcar
				(lambda (term-num) (cons term-num (find-file (multi-run-get-full-remote-path file-path term-num (not non-root)))))
				multi-run-terminals-list)))
    (multi-run-display-buffers master-buffer-name num-terminals window-batch "file" (lambda (term-num) (cdr (assoc term-num term-num-buffer-list))))
    (setq multi-run-buffers-assoc-list term-num-buffer-list)
    nil))

(defun multi-run-find-remote-files (file-path &optional window-batch)
  "Open file specified by FILE-PATH for all terminals and display them on the screen with WINDOW-BATCH number of them in one single vertical slot."
  (multi-run-find-remote-files-sudo file-path window-batch 't))

(defun multi-run-execute-on-single-buffer (buffer)
  "Run user command on BUFFER."
  (set-buffer buffer)
  (ignore-errors (call-interactively this-original-command))
  (set-window-point (get-buffer-window buffer) (point)))

(defun multi-run-execute-command ()
  "Execute the command on all the files."
  (when multi-run-start
    (unless (or (eq this-original-command 'multi-run-edit-files)
		(eq this-original-command 'eshell-send-input))
      (if (eq this-original-command 'keyboard-quit)
	  (multi-run-edit-files-quit)
	(mapc (lambda (term-num) (multi-run-execute-on-single-buffer (cdr (assoc term-num multi-run-buffers-assoc-list)))) (cdr multi-run-terminals-list))))))

(defun multi-run-edit-files (&optional window-batch)
  "Enable editing files opened by `multi-run-find-remote-files'.  Display the files on the frame with WINDOW-BATCH number of them in one single vertical slot."
  (interactive)
  (let* ((master-buffer-name (buffer-name))
	 (num-terminals (length multi-run-terminals-list))
	 (window-batch (if window-batch window-batch (calculate-window-batch num-terminals))))
    (multi-run-display-buffers master-buffer-name num-terminals window-batch "file" (lambda (term-num) (cdr (assoc term-num multi-run-buffers-assoc-list))))
    (select-window (get-buffer-window (cdr (assoc (car multi-run-terminals-list) multi-run-buffers-assoc-list))))
    (setq multi-run-start nil)
    (add-hook 'post-command-hook 'multi-run-execute-command t nil)
    (setq multi-run-start 't)
    "Editing multiple files. Press C-g to quit editing and return to this buffer"))

(defun multi-run-edit-files-quit ()
  "Disable editing files."
  (remove-hook 'post-command-hook 'multi-run-execute-command nil)
  (other-window -1))

(defun multi-run-execute-function-on-single-buffer (buffer fun)
  "Run on BUFFER the function FUN."
  (set-buffer buffer)
  (funcall fun)
  (set-window-point (get-buffer-window buffer) (point)))

(defun multi-run-execute-function-on-files (fun)
  "Execute FUN on the buffers opened by `multi-run-find-remote-files'."
  (mapc (lambda (term-num) (multi-run-execute-function-on-single-buffer (cdr (assoc term-num multi-run-buffers-assoc-list)) fun)) multi-run-terminals-list))

(defun multi-run-copy-sudo (file1 file-or-directory1 &rest files)
  "Copy FILE1 to FILE-OR-DIRECTORY1.  With multiple arguments, the last element FILES is the destination directory, the rest are source files to copy."
  (apply 'multi-run-copy-generic 'multi-run-copy-one-file-sudo file1 file-or-directory1 files))

(defun multi-run-copy (file1 file-or-directory1 &rest files)
  "Copy FILE1 to FILE-OR-DIRECTORY1.  With multiple arguments, the last element FILES is the destination directory, the rest are source files to copy."
  (apply 'multi-run-copy-generic 'multi-run-copy-one-file file1 file-or-directory1 files))

(defun multi-run-kill-terminals ()
  "Kill active terminals."
  (mapc (lambda (term-num) (kill-buffer (multi-run-get-buffer-name term-num))) multi-run-terminals-list)
  nil)

(defun multi-run-kill-all-timers ()
  "Cancel commands running on a loop or via delay functions."
  (mapc 'cancel-timer multi-run-timers-list)
  "All timers canceled")

(provide 'multi-run)
;;; multi-run.el ends here
