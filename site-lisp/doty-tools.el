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

;; === Emacs tools

(defun doty-tools--describe-function (func-name)
  "Return the help text for function FUNC-NAME."
  (save-window-excursion
    (describe-function (intern func-name))
    (with-current-buffer "*Help*"
      (buffer-substring-no-properties (point-min) (point-max)))))

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

;; ==== File reading

(defun doty-tools--buffer-or-file (buffer-or-file)
  "Return a buffer visiting a file, if BUFFER-OR-FILE names a file.
Otherwise a buffer with the given name, if it is the only one."
  (if (file-exists-p buffer-or-file)
      (find-file-noselect buffer-or-file)
    (let ((buffers (match-buffers buffer-or-file)))
      (if (length= buffers 1)
          (car buffers)
        nil))))

(defun doty-tools--open-file (filename)
  "Visit FILENAME and return its contents as a string."
  (with-current-buffer (doty-tools--buffer-or-file filename)
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
 :category "reading"
 :confirm nil
 :include t)

(defun doty-tools--read-lines (buffer-or-file start-line &optional end-line include-line-numbers)
  "Get content from specified line range in BUFFER-OR-FILE.

START-LINE is the beginning line number. Optional END-LINE is the ending
line number. If nil, only START-LINE is  returned. Optional
INCLUDE-LINE-NUMBERS, if non-nil, adds line numbers to the output."
  (with-current-buffer (doty-tools--buffer-or-file buffer-or-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- start-line))
        (let ((end-line (or end-line start-line))
              (result ""))
          (dotimes (i (- end-line start-line -1))
            (let ((line-num (+ start-line i))
                  (line-content (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
              (setq result
                    (concat result
                            (if include-line-numbers
                                (format "%d: %s\n" line-num line-content)
                              (format "%s\n" line-content)))))
            (forward-line 1))
          result)))))

(gptel-make-tool
 :name "emacs_read_lines"
 :function #'doty-tools--read-lines
 :description "Get content from specified line range in a file."
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path")
         (:name "start_line"
          :type integer
          :description "Starting line number")
         (:name "end_line"
          :type integer
          :optional t
          :description "Ending line number (optional)")
         (:name "include_line_numbers"
          :type boolean
          :optional t
          :description "Whether to include line numbers in output (optional)"))
 :category "reading"
 :confirm nil
 :include t)

(defun doty-tools--convert-regex (regex)
  "Convert the REGEX in standard syntax to Emacs regex syntax.
Handles common differences like | vs \\|, () vs \\(\\), etc.
Also converts special character classes like \\d to [[:digit:]]."
  (let ((case-fold-search nil)
        (i 0)
        (result "")
        (len (length regex))
        (escaped nil))
    (while (< i len)
      (let ((char (aref regex i)))
        (cond
         ;; Handle escaped characters and special character classes
         (escaped
          (setq escaped nil)
          (cond
           ;; Convert \d (digits) to [[:digit:]]
           ((= char ?d)
            (setq result (concat result "[[:digit:]]")))

           ;; Convert \D (non-digits) to [^[:digit:]]
           ((= char ?D)
            (setq result (concat result "[^[:digit:]]")))

           ;; Convert \w (word chars) to [[:alnum:]_]
           ((= char ?w)
            (setq result (concat result "[[:alnum:]_]")))

           ;; Convert \W (non-word chars) to [^[:alnum:]_]
           ((= char ?W)
            (setq result (concat result "[^[:alnum:]_]")))

           ;; Convert \s (whitespace) to [[:space:]]
           ((= char ?s)
            (setq result (concat result "[[:space:]]")))

           ;; Convert \S (non-whitespace) to [^[:space:]]
           ((= char ?S)
            (setq result (concat result "[^[:space:]]")))

           ;; Pass through other escaped characters
           (t
            (setq result (concat result "\\" (string char))))))

         ;; Handle escape character
         ((= char ?\\)
          (setq escaped t))

         ;; Convert | to \|
         ((= char ?|)
          (setq result (concat result "\\|")))

         ;; Convert ( to \( and ) to \)
         ((= char ?\()
          (setq result (concat result "\\(")))
         ((= char ?\))
          (setq result (concat result "\\)")))

         ;; Convert { to \{ and } to \}
         ((= char ?{)
          (setq result (concat result "\\{")))
         ((= char ?})
          (setq result (concat result "\\}")))

         ;; Pass other characters through
         (t
          (setq result (concat result (string char))))))
      (setq i (1+ i)))
    result))

(defun doty-tools--search-text (buffer-or-file pattern context-lines max-matches use-regex)
  "Search for PATTERN in BUFFER-OR-FILE and return matches with context.

CONTEXT-LINES is the number of lines before and after each match to
include.

MAX-MATCHES is the maximum number of matches to return.

If USE-REGEX is non-nil, treat PATTERN as a regular expression, in
standard syntax. It will be converted into Emacs syntax before being
run."
  (with-current-buffer (doty-tools--buffer-or-file buffer-or-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((count 0)
              (matches "")
              (search-pattern (if use-regex (doty-tools--convert-regex pattern) pattern))
              (search-fn (if use-regex 're-search-forward 'search-forward)))
          (while (and (funcall search-fn search-pattern nil t)
                      (< count max-matches))
            (setq count (1+ count))
            (let* ((match-line (line-number-at-pos))
                   (start-line (max 1 (- match-line context-lines)))
                   (end-line (+ match-line context-lines))
                   (context (doty-tools--read-lines (current-buffer)
                                                    start-line
                                                    end-line
                                                    t)))
              (setq matches (concat matches
                                    (format "Match %d (line %d):\n"
                                           count match-line)
                                    context
                                    "\n"))))
          matches)))))

(gptel-make-tool
 :name "emacs_search_text"
 :function #'doty-tools--search-text
 :description "Find text matching a pattern and return with context. Returns formatted matches with line numbers and surrounding context."
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path")
         (:name "pattern"
          :type string
          :description "Text or regex to search for")
         (:name "context_lines"
          :type integer
          :description "Number of lines before/after to include")
         (:name "max_matches"
          :type integer
          :description "Maximum number of matches to return")
         (:name "use_regex"
          :type boolean
          :description "Whether to use regex matching"))
 :category "reading"
 :confirm nil
 :include t)

;; === Editing tools

(defun doty-tools--insert-at-line (buffer-or-file line-number text &optional at-end)
  "Insert TEXT at LINE-NUMBER in BUFFER-OR-FILE.
If AT-END is non-nil, insert at end of line, otherwise at beginning."
  (with-current-buffer (doty-tools--buffer-or-file buffer-or-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- line-number))
        (if at-end
            (end-of-line)
          (beginning-of-line))
        (insert text)
        (format "Inserted text at %s of line %d in %s"
                (if at-end "end" "beginning")
                line-number
                (if (bufferp buffer-or-file)
                    (buffer-name buffer-or-file)
                  buffer-or-file))))))

(gptel-make-tool
 :name "emacs_insert_at_line"
 :function #'doty-tools--insert-at-line
 :description "Insert text at the beginning or end of specified line."
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path")
         (:name "line_number"
          :type integer
          :description "Line to insert at")
         (:name "text"
          :type string
          :description "Text to insert")
         (:name "at_end"
          :type boolean
          :optional t
          :description "If true, insert at end of line; otherwise at beginning (optional)"))
 :category "editing"
 :confirm t
 :include t)


;; === System tools

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
