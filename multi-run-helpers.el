;;; multi-run-helpers.el --- Helper functions for multi-run.el  -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This contains helper functions for multi-run.el. See that file for the user interface.

;; See the full documentation on https://www.github.com/sagarjha/multi-run.

;;; Code:

(defvar multi-run-timers-list nil
  "Internal list of timers to cancel when multi-run-kill-all-timers is called.")

(defun multi-run-match-term-type-p (multi-run-term-type)
  "Return non-nil if MULTI-RUN-TERM-TYPE is one of the supported terminal types."
  (memq multi-run-term-type '(eshell shell ansi-term term multi-term)))

(defun multi-run-get-buffer-name (term-num)
  "Return the name of the buffer for a given terminal number TERM-NUM."
  (let ((term-num-in-str (number-to-string term-num)))
    (pcase multi-run-term-type
      ('eshell (concat "eshell<" term-num-in-str ">"))
      ('shell (concat "shell<" term-num-in-str ">"))
      ('ansi-term (concat "ansi-term<" term-num-in-str ">"))
      ('term (concat "term<" term-num-in-str ">"))
      ('multi-term (concat "terminal<" term-num-in-str ">"))
      (multi-run-term-type (error "Value of multi-run-term-type should be one of the following symbols: eshell, shell, ansi-term, term, multi-term")))))

(defun multi-run-get-input-function ()
  "Return the name of the function that will run the input on the terminal."
  (pcase multi-run-term-type
    ('eshell 'eshell-send-input)
    ('shell 'comint-send-input)
    ((pred multi-run-match-term-type-p) 'term-send-input)
    (multi-run-term-type (error "Value of multi-run-term-type should be one of the following symbols: eshell, shell, ansi-term, term, multi-term"))))

(defun multi-run-get-new-input-point ()
  "Move point to the latest prompt in the terminal buffer."
  (pcase multi-run-term-type
    ('eshell eshell-last-output-end)
    ((pred multi-run-match-term-type-p) (process-mark (get-buffer-process (current-buffer))))
    (multi-run-term-type (error "Value of multi-run-term-type should be one of the following symbols: eshell, shell, ansi-term, term, multi-term"))))

(defun multi-run-open-terminal (term-num)
  "Open terminal number TERM-NUM in a buffer if it's not already open.  In any case, switch to it."
  (unless (get-buffer (multi-run-get-buffer-name term-num))
    (progn
      (pcase multi-run-term-type
        ('eshell (eshell term-num))
        ('shell (shell))
        ('ansi-term (ansi-term "/bin/bash"))
        ('term (term "/bin/bash"))
        ('multi-term (multi-term))
        (multi-run-term-type (error "Value of multi-run-term-type should be one of the following symbols: eshell, shell, ansi-term, term, multi-term")))
      (rename-buffer (multi-run-get-buffer-name term-num)))))

(defun multi-run-on-single-terminal (command term-num)
  "Run the command COMMAND on a single terminal with number TERM-NUM."
  (set-buffer (multi-run-get-buffer-name term-num))
  (goto-char (multi-run-get-new-input-point))
  (insert command)
  (funcall (multi-run-get-input-function)))

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

(defun multi-run-create-terminals (num-terminals)
  "Create NUM-TERMINALS number of terminals."
  (dotimes (i num-terminals)
    (multi-run-open-terminal (1+ i))))

(defun multi-run-make-vertical-or-horizontal-pane (num-terminals offset sym-vec choice)
  "Helper function for multi-run-configure-terminals.  Create NUM-TERMINALS number of windows with buffer names given by OFFSET into SYM-VEC.  The windows are created in a single vertical or horizontal pane determined by CHOICE."
  (if (= num-terminals 1) (aref sym-vec offset)
    (list (if (= choice 0) '| '-) `(,(if (= choice 0) :left-size-ratio :upper-size-ratio)
				    ,(/ (- num-terminals 1.0) num-terminals))
	  (multi-run-make-vertical-or-horizontal-pane (1- num-terminals) (1- offset) sym-vec choice) (aref sym-vec offset))))

(defun multi-run-make-internal-recipe (num-terminals window-batch sym-vec)
  "Helper function for multi-run-configure-terminals.  Create a recipe for wlf:layout for NUM-TERMINALS number of terminal buffers with WINDOW-BATCH of them in one vertical pane.  Get symbol names for terminals from SYM-VEC."
  (let ((num-panes (if (= (% num-terminals window-batch) 0)
		       (/ num-terminals window-batch) (1+ (/ num-terminals window-batch)))))
    (if (<= num-terminals window-batch)
	(multi-run-make-vertical-or-horizontal-pane num-terminals num-terminals sym-vec 1)
      (list '| `(:left-size-ratio ,(/ (- num-panes 1.0) num-panes))
	    (multi-run-make-internal-recipe (- num-terminals (if (= (% num-terminals window-batch) 0)
								 window-batch
							       (% num-terminals window-batch)))
					    window-batch sym-vec)
	    (multi-run-make-vertical-or-horizontal-pane (if (= (% num-terminals window-batch) 0) window-batch
							  (% num-terminals window-batch))
							num-terminals sym-vec 1)))))

(defun multi-run-make-symbols (num-terminals hint &optional cnt)
  "Create unique symbols for NUM-TERMINALS number of terminals with common prefix HINT having created recursively symbols for CNT of them."
  (unless cnt
    (setq cnt 0))
  (when (<= cnt num-terminals)
    (vconcat (vector (make-symbol (concat hint (number-to-string cnt)))) (multi-run-make-symbols num-terminals hint (1+ cnt)))))

(defun multi-run-make-dict (num-terminals hint-fun sym-vec &optional cnt)
  "Create a dictionary of terminal symbol names for NUM-TERMINALS number of terminals with names provided by HINT-FUN and symbols from SYM-VEC, having created recursively entries for CNT of them."
  (unless cnt
    (setq cnt 1))
  (when (<= cnt num-terminals)
    (cons (list :name (aref sym-vec cnt)
		:buffer (funcall hint-fun cnt))
	  (multi-run-make-dict num-terminals hint-fun sym-vec (1+ cnt)))))

(defun multi-run-copy-one-file-sudo (source-file destination-file-or-directory &optional non-root)
  "Copy SOURCE-FILE to DESTINATION-FILE-OR-DIRECTORY at remote nodes for all terminals.  Copy with sudo if NON-ROOT is false."
  (mapc (lambda (x) (copy-file source-file (concat "/ssh:" (when multi-run-ssh-username
							     (concat multi-run-ssh-username "@"))
						   (elt multi-run-hostnames-list (- x 1))
						   (when (not non-root) (concat "|sudo:" (elt multi-run-hostnames-list (- x 1))))
						   ":" destination-file-or-directory) 't))
	multi-run-terminals-list))

(defun multi-run-copy-one-file (source-file destination-file-or-directory)
  "Copy SOURCE-FILE to DESTINATION-FILE-OR-DIRECTORY at remote nodes for all terminals."
  (multi-run-copy-one-file-sudo source-file destination-file-or-directory 't))

(defun multi-run-copy-generic (copy-fun &rest files)
  "Internal function.  Call COPY-FUN after parsing FILES."
  (let ((destination-file-or-directory (car (last files)))
	(source-files (reverse (cdr (reverse files)))))
    (mapc (lambda (source-file) (funcall copy-fun source-file destination-file-or-directory)) source-files)))


(provide 'multi-run-helpers)
;;; multi-run-helpers.el ends here
