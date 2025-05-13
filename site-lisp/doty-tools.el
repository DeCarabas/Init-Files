;;; doty-tools.el --- Define Doty's tools for gptel     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John Doty

;; Author: John Doty <john@d0ty.me>
;; Package-Version: 20250512.0000
;; Package-Revision:
;; Package-Requires: ((gptel "20250512.0000"))
;; Keywords: convenience, tools
;; URL:

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This defines a set of tools that gptel can use to edit buffers and whatnot.
;;
;;; Code:
(require 'gptel)
(require 'url)

(defun doty-tools--describe-function (func-name)
  "Return the help text for function FUNC-NAME."
  (save-window-excursion
    (describe-function (intern func-name))
    (with-current-buffer "*Help*"
      (buffer-substring-no-properties (point-min) (point-max)))))

;; Emacs specific help
(gptel-make-tool
 :name "emacs_describe_function"
 :function #'doty-tools--describe-function
 :description "Get help documentation for an Emacs function. Returns the complete help text including the function signature, description, and any additional information available."
 :args (list '(:name "func-name"
               :type string
               :description "The name of the Emacs function to get help for, e.g., 'find-file' or 'buffer-string'"))
 :category "emacs"
 :confirm nil
 :include t)

(defun doty-tools--describe-variable (variable-name)
  "Return the help text for function VARIABLE-NAME."
  (save-window-excursion
    (describe-variable (intern variable-name))
    (with-current-buffer "*Help*"
      (buffer-substring-no-properties (point-min) (point-max)))))

(gptel-make-tool
 :name "emacs_describe_variable"
  :function #'doty-tools--describe-variable
 :description "Get documentation for an Emacs variable. Returns the complete help text for the specified variable."
 :args '((:name "variable-name"
          :type string
          :description "Name of the Emacs variable to get documentation for"))
 :category "emacs"
 :confirm nil
 :include t)

(defun doty-tools--apropos (pattern)
  "Invoke the help apropos function for PATTERN and return the results as a string."
  (save-window-excursion
    (apropos pattern)
    (with-current-buffer "*Apropos*"
      (buffer-substring-no-properties (point-min) (point-max)))))

(gptel-make-tool
 :name "emacs_help_apropos"
  :function #'doty-tools--describe-variable
 :description "Search for appropriate emacs function and variable documentation. Further information about functions and variables can be retrieved with the `emacs_describe_function` and `emacs_describe_variable` tools."
 :args '((:name "pattern"
          :type string
          :description "The pattern to search for. It can be a word, a list of words (separated by spaces), or a regexp (using some regular expression characters). If it is a word, search for matches for that word as a substring.  If it is a list of words, search for matches for any two (or more) of those words."))
 :category "emacs"
 :confirm nil
 :include t)

(defun doty-tools--open-file (filename)
  "Visit FILENAME and return its contents as a string."
  (with-current-buffer (find-file-noselect filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(gptel-make-tool
 :name "emacs_open_file"
 :function #'doty-tools--open-file
 :description "Opens a file and reads content from a specified file path or displays directory information. This tool accepts relative file paths and returns different outputs based on the path type:
- For files: Returns the complete file contents
- For directories: Returns directory listings in Unix long format with permissions, link count, owner, group, size (bytes), modification date, and filename. First character in permissions indicates file type ('d'=directory, '-'=file).

Example:
```
drwxr-x---  2 john.doty ubuntu   4096 May 13 17:08 .
-rw-r-----  1 john.doty ubuntu   6290 Jan  9 23:20 50-arc.el
```"
 :args '((:name "filename"
          :type string
          :description "The path of the file to read."))
 :category "devtools"
 :confirm nil
 :include t)

(defun doty-tools--run-async-command (callback command)
  "Run COMMAND asynchronously and call CALLBACK with the results as a string."
  (let ((output-buffer (generate-new-buffer " *async-command-output*")))
    (with-current-buffer output-buffer
      ;; Make buffer lightweight - disable undo, make read-only
      (buffer-disable-undo)
      (setq-local inhibit-modification-hooks t)
      (setq-local inhibit-read-only t)) ; Temporarily allow writing by the process

    (set-process-sentinel
     (start-process "gptel-async-command" output-buffer
                    shell-file-name shell-command-switch command)
     (lambda (process event)
       (when (string-match "finished" event)
         (with-current-buffer output-buffer
           (funcall callback (buffer-string)))
         (kill-buffer output-buffer))))))

(gptel-make-tool
 :name "shell_command"
 :function #'doty-tools--run-async-command
 :description "Run a shell command asynchronously and return its output as a string. The command is executed in a subprocess and the standard output and error are captured."
 :args (list '(:name "command"
               :type string
               :description "The shell command to execute"))
 :async t
 :category "system"
 :confirm t  ;; For security, prompt the user before running any shell command
 :include t) ;; Include the command output in the conversation

;;; doty-tools.el ends here
