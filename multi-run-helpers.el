;;; multi-run-helpers.el --- Helper functions for multi-run.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019  Sagar Jha

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This contains helper functions for multi-run.el.  See that file for the user interface.

;; See the full documentation on https://www.github.com/sagarjha/multi-run.

;;; Code:

(require 'multi-run-vars)

(defgroup multi-run nil
  "Run commands in multiple terminal windows."
  :group 'terminals)

(defun multi-run-get-buffer-name (term-num)
  "Return the name of the buffer for a given terminal number TERM-NUM."
  (concat "eshell<" (number-to-string term-num) ">"))

(defun multi-run-get-working-directory ()
  "Return the working directory relative to home.  Useful for multi-copy."
  (let* ((working-directory (eshell/pwd))
	 (dir-names (split-string working-directory "/" 't)))
    (if (not (string-equal (car dir-names) "home")) (error "Not a child of home directory")
      (seq-reduce (lambda (res dir-name) (concat res dir-name "/")) (cons "." (cdr (cdr dir-names))) ""))))

(defun multi-run-get-full-remote-path (file-path term-num root)
  "Return the full path on the remote node for FILE-PATH specified by TERM-NUM.  Add sudo if ROOT is t."
  (concat "/ssh:" (when multi-run-ssh-username (concat multi-run-ssh-username "@"))
	  (elt multi-run-hostnames-list (1- term-num))
	  (when root (concat "|sudo:" (elt multi-run-hostnames-list (1- term-num))))
	  ":" file-path))

(defun multi-run-open-terminal (term-num)
  "Open terminal number TERM-NUM in a buffer if it's not already open.  In any case, switch to it."
  (unless (get-buffer (multi-run-get-buffer-name term-num))
    (progn
      (eshell term-num)
      (rename-buffer (multi-run-get-buffer-name term-num)))))

(defun multi-run-on-single-terminal (command term-num)
  "Run the command COMMAND on a single terminal with number TERM-NUM."
  (set-buffer (multi-run-get-buffer-name term-num))
  (goto-char eshell-last-output-end)
  (insert command)
  (eshell-send-input))

(defun multi-run-on-terminals (command term-nums &optional delay)
  "Run the COMMAND on terminals in TERM-NUMS with an optional DELAY between running on successive terminals."
  (let ((delay (if delay delay 0))
	(delay-cnt 0))
    (while term-nums
      (let ((evaled-command (if (functionp command)
                                (funcall command (car term-nums)) command)))
        (setq multi-run-timers-list
              (cons (run-at-time (concat (number-to-string (* delay-cnt delay)) " sec")
                                 nil
                                 'multi-run-on-single-terminal evaled-command (car term-nums))
                    multi-run-timers-list))
        (setq term-nums (cdr term-nums))
        (setq delay-cnt (1+ delay-cnt))))))

(defun multi-run-create-terminals ()
  "Create terminals given by multi-run-terminals-list."
  (mapc 'multi-run-open-terminal multi-run-terminals-list))

(defun calculate-window-batch (num-terminals)
  "Calculate the window batch parameters for displaying NUM-TERMINALS."
  (round (sqrt num-terminals))
  )

(defun multi-run-make-vertical-or-horizontal-pane (num-terminals offset sym-vec)
  "Helper function for multi-run-configure-terminals.  Create NUM-TERMINALS number of windows with buffer names given by OFFSET into SYM-VEC in a single vertical pane."
  (if (= num-terminals 1) (aref sym-vec (1- offset))
    (list '- `(,:upper-size-ratio
	       ,(/ (- num-terminals 1.0) num-terminals))
	  (multi-run-make-vertical-or-horizontal-pane (1- num-terminals) (1- offset) sym-vec) (aref sym-vec (1- offset)))))

(defun multi-run-make-internal-recipe (num-terminals window-batch sym-vec)
  "Create a recipe for wlf:layout for NUM-TERMINALS terminal buffers with WINDOW-BATCH of them in one vertical pane.  Get symbol names for terminals from SYM-VEC."
  (let* ((num-panes (if (= (% num-terminals window-batch) 0)
			(/ num-terminals window-batch) (1+ (/ num-terminals window-batch)))))
    (if (<= num-terminals window-batch)
	(multi-run-make-vertical-or-horizontal-pane num-terminals num-terminals sym-vec)
      (list '| `(:left-size-ratio ,(/ (- num-panes 1.0) num-panes))
	    (multi-run-make-internal-recipe (- num-terminals (if (= (% num-terminals window-batch) 0)
								 window-batch
							       (% num-terminals window-batch)))
					    window-batch sym-vec)
	    (multi-run-make-vertical-or-horizontal-pane (if (= (% num-terminals window-batch) 0) window-batch
							  (% num-terminals window-batch))
							num-terminals sym-vec)))))

(defun multi-run-make-symbols (hint)
  "Create unique symbols for the terminals with common prefix HINT."
  (mapcar (lambda (num) (make-symbol (concat hint (number-to-string num)))) multi-run-terminals-list))

(defun multi-run-make-dict (hint-fun sym-list)
  "Create a dictionary of terminal symbol names according to HINT-FUN and symbols from SYM-LIST."
  (cl-mapcar (lambda (symbol num) (list :name symbol
					:buffer (funcall hint-fun num)))
	     sym-list multi-run-terminals-list))

(defun multi-run-copy-one-file-sudo (source-file destination-file-or-directory &optional non-root)
  "Copy SOURCE-FILE to DESTINATION-FILE-OR-DIRECTORY at remote nodes for all terminals.  Copy with sudo if NON-ROOT is false."
  (mapc (lambda (x) (condition-case err (copy-file source-file (concat "/ssh:" (when multi-run-ssh-username
										 (concat multi-run-ssh-username "@"))
								       (elt multi-run-hostnames-list (- x 1))
								       (when (not non-root) (concat "|sudo:" (elt multi-run-hostnames-list (- x 1))))
								       ":" destination-file-or-directory) 't)
		      (file-missing (signal (car err) (cdr err)))
		      (file-error (if (string-prefix-p "Couldn’t write region to" (error-message-string err))
				      (message "Either the destination directory does not end with a slash (/), does not exist on remote nodes  or you need sudo permissions")
				    (print (error-message-string err)))
				  (signal (car err) (cdr err)))
		      (error (print err))))
	multi-run-terminals-list))

(defun multi-run-copy-one-file (source-file destination-file-or-directory)
  "Copy SOURCE-FILE to DESTINATION-FILE-OR-DIRECTORY at remote nodes for all terminals."
  (multi-run-copy-one-file-sudo source-file destination-file-or-directory 't))

(defun multi-run-copy-generic (copy-fun &rest files)
  "Internal function.  Call COPY-FUN after parsing FILES."
  (let ((destination-file-or-directory (car (last files)))
	(source-files (flatten-list (reverse (cdr (reverse files))))))
    (mapc (lambda (source-file) (funcall copy-fun source-file destination-file-or-directory)) source-files)))

(provide 'multi-run-helpers)
;;; multi-run-helpers.el ends here
