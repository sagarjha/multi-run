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

;; This contains helper variables for multi-run.el and multi-run-helpers.el.

;; See the full documentation on https://www.github.com/sagarjha/multi-run.

;;; Code:

(defgroup multi-run nil
  "Run commands in multiple terminal windows."
  :group 'terminals)

(defvar multi-run-terminals-list nil
  "List of terminals to run the command on.")

(defvar multi-run-hostnames-list nil
  "List of hostnames for multi-run-ssh.")

(defvar multi-run-ssh-username nil
  "SSH username for multi-run-ssh.")

(defvar multi-run-timers-list nil
  "Internal list of timers to cancel when multi-run-kill-all-timers is called.")

(provide 'multi-run-vars)
;;; multi-run-vars.el ends here
