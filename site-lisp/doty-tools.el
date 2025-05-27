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
(require 'dash) ;; TODO: Package requires
(require 'ert)
(require 'gptel)
(require 'project)
(require 'treesit)

(defun doty-tools-bool (v)
  "Convert V into a boolean, treating :json-false as nil."
  (and v (not (eq v :json-false))))

(ert-deftest doty-tools--test--bool ()
  (should (doty-tools-bool t))
  (should (not (doty-tools-bool nil)))
  (should (not (doty-tools-bool :json-false))))

;; === Testing Support

(defun doty-tools--test--find-tool (name)
  "Find the registered tool named NAME for testing."
  (alist-get name
             (-flatten (--map (cdr it) gptel--known-tools))
             nil nil #'equal))

(defun doty-tools--test--invoke-tool (name arg-plist)
  "Invoke the tool named NAME and pass in the provided arguments from ARG-PLIST.

This is kinda like what happens inside gptel but that's not accessible."
  (let* ((tool (doty-tools--test--find-tool name))
         ;; Ensure we have the correct JSON encoding.
         (arg-plist (progn
                      ;; (message "ARGS: %S" (gptel--json-encode arg-plist))
                      (gptel--json-read-string (gptel--json-encode arg-plist))))
         (arg-values (-map (lambda (arg)
                             (let ((key (intern (concat ":" (plist-get arg :name)))))
                               (plist-get arg-plist key)))
                           (gptel-tool-args tool))))
    (apply (gptel-tool-function tool) arg-values)))


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

(ert-deftest doty-tools--test--emacs_describe_function ()
  "Tests for the emacs_describe_function tool."
  (should
   (string-match-p "cons is a primitive-function"
                   (doty-tools--test--invoke-tool
                    "emacs_describe_function" '(:func-name "cons")))))



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

(ert-deftest doty-tools--test--emacs_describe_variable ()
  "Tests for doty-tools--describe-variable."
  (should
   (string-match-p "current-load-list is a variable"
                   (doty-tools--test--invoke-tool
                    "emacs_describe_variable" '(:variable-name "current-load-list")))))



(defun doty-tools--apropos (pattern)
  "Invoke the help apropos function for PATTERN and return the results as a string."
  (save-window-excursion
    (apropos pattern)
    (with-current-buffer "*Apropos*"
      (buffer-substring-no-properties (point-min) (point-max)))))

(gptel-make-tool
 :name "emacs_help_apropos"
 :function #'doty-tools--apropos
 :description "Search for appropriate emacs function and variable documentation. Further information about functions and variables can be retrieved with the `emacs_describe_function` and `emacs_describe_variable` tools."
 :args '((:name "pattern"
          :type string
          :description "The pattern to search for. It can be a word, a list of words (separated by spaces), or a regexp (using some regular expression characters). If it is a word, search for matches for that word as a substring.  If it is a list of words, search for matches for any two (or more) of those words."))
 :category "emacs"
 :confirm nil
 :include t)

(ert-deftest doty-tools--test--emacs_help_apropos ()
  "Tests for the emacs_help_apropos tool."
  (should
   (string-match-p "with-current-buffer"
                   (doty-tools--test--invoke-tool
                    "emacs_help_apropos" '(:pattern "buffer")))))

;; === File reading

(defun doty-tools--buffer-or-file (buffer-or-file)
  "Return a buffer for BUFFER-OR-FILE for the well-behaved LLM tool.

If it is a buffer object, just return it. If it names a file, visit the
 file. If there is exactly one buffer that matches that name, return
 that buffer. Otherwise return nil."
  (cond
   ((bufferp buffer-or-file) buffer-or-file)
   ((length= (match-buffers (regexp-quote buffer-or-file)) 1)
    (car (match-buffers (regexp-quote buffer-or-file))))
   (t
    (find-file-noselect (expand-file-name buffer-or-file)))))
;; (t (error "File '%s' doesn't exist and does not name an open buffer"
;;           buffer-or-file))))

;; NOTE: I THINK THIS TOOL ISN'T GREAT.

;; (defun doty-tools--open-file (filename &optional max-chars)
;;   "Visit FILENAME and return up to MAX-CHARS of its contents as a string.

;; If MAX-CHARS is not provided then the entire buffer is returned."
;;   (with-current-buffer (doty-tools--buffer-or-file filename)
;;     (buffer-substring-no-properties (point-min) (min (point-max) (or max-chars 4096)))))

;; (gptel-make-tool
;;  :name "emacs_open_file"
;;  :function #'doty-tools--open-file
;;  :description "Opens a file and reads content from a specified file path or displays directory information. This tool accepts relative file paths and returns different outputs based on the path type:
;; - For files: Returns the complete file contents
;; - For directories: Returns directory listings in Unix long format with permissions, link count, owner, group, size (bytes), modification date, and filename. First character in permissions indicates file type ('d'=directory, '-'=file).

;; Example:
;; ```
;; drwxr-x---  2 john.doty ubuntu   4096 May 13 17:08 .
;; -rw-r-----  1 john.doty ubuntu   6290 Jan  9 23:20 50-arc.el
;; ```
;; "
;;  :args '((:name "filename"
;;           :type string
;;           :description "The path of the file to read.")
;;          (:name "max-chars"
;;           :type integer
;;           :optional t
;;           :description "The maximum number of characters to return. If this is not specified then at most 4096 characters are returned."))
;;  :category "reading"
;;  :confirm nil
;;  :include t)

(defun doty-tools--read-lines (buffer-or-file start-line &optional end-line include-line-numbers no-prologue)
  "Get content from specified line range in BUFFER-OR-FILE.

START-LINE is the beginning line number.

Optional END-LINE is the ending line number. If nil, only START-LINE is
returned.

Optional INCLUDE-LINE-NUMBERS, if non-nil, adds line numbers to the output.

Optional NO-PROLOGUE adds a small buffer summary to the top of the file, if
non-nil."
  (with-current-buffer (doty-tools--buffer-or-file buffer-or-file)
    (unless (buffer-modified-p)
      (condition-case _
          (revert-buffer t t t)
        (t nil)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- start-line))
        (let* ((end-line (or end-line start-line))
               (result (if (not no-prologue)
                           (format "Lines %d-%d of %d:\n"
                                   start-line end-line
                                   (count-lines (point-min) (point-max)))
                         "")))
          (dotimes (i (- end-line start-line -1))
            (let ((line-num (+ start-line i))
                  (line-content (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
              (setq result
                    (concat result
                            (if (doty-tools-bool include-line-numbers)
                                (format "%d: %s\n" line-num line-content)
                              (format "%s\n" line-content)))))
            (forward-line 1))
          result)))))

(gptel-make-tool
 :name "emacs_read_lines"
 :function #'doty-tools--read-lines
 :description "Opens a file and reads content from a specified file path or displays directory information. Lines are returned with trailing blanks, if any. Line 1 is the first line. There is no guarantee that the last line is blank. This tool accepts relative file paths and returns different outputs based on the path type:
- For files: Returns the complete file contents
- For directories: Returns directory listings in Unix long format with permissions, link count, owner, group, size (bytes), modification date, and filename. First character in permissions indicates file type ('d'=directory, '-'=file).

Example:
```
drwxr-x---  2 john.doty ubuntu   4096 May 13 17:08 .
-rw-r-----  1 john.doty ubuntu   6290 Jan  9 23:20 50-arc.el
```

In all cases, the response is prefixed with a single line containing the line range being returned, e.g.:

```
Lines 1-1 of 267:
```

Returning line numbers is required if the file is to be edited."
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path")
         (:name "start_line"
          :type integer
          :description "1-based starting line number")
         (:name "end_line"
          :type integer
          :optional t
          :description "1-based ending line number (inclusive, optional)")
         (:name "include_line_numbers"
          :type boolean
          :optional t
          :description "Whether to include line numbers in output (optional)"))
 :category "reading"
 :confirm nil
 :include t)

(ert-deftest doty-tools--test--emacs_read_lines ()
  "Tests for emacs_read_lines."
  (with-temp-buffer
    (let ((name (buffer-name (current-buffer))))
      (insert "Hello!\n")
      (insert "World!\n")
      (insert "Emacs!\n")

      (should
       (string-equal "Lines 1-1 of 3:\nHello!\n"
                     (doty-tools--test--invoke-tool
                      "emacs_read_lines" (list :buffer_or_file name
                                               :start_line 1))))
      (should
       (string-equal "Lines 3-3 of 3:\nEmacs!\n"
                     (doty-tools--test--invoke-tool
                      "emacs_read_lines" (list :buffer_or_file name
                                               :start_line 3))))
      (should
       (string-equal "Lines 1-2 of 3:\nHello!\nWorld!\n"
                     (doty-tools--test--invoke-tool
                      "emacs_read_lines" (list :buffer_or_file name
                                               :start_line 1
                                               :end_line 2)))))))

(ert-deftest doty-tools--test--emacs_read_lines-line-numbers ()
  "Tests for emacs_read_lines with line numbers."
  (with-temp-buffer
    (let ((name (buffer-name (current-buffer))))
      (insert "Hello!\n")
      (insert "World!\n")
      (insert "Emacs!\n")

      (should
       (string-equal "Lines 1-1 of 3:\n1: Hello!\n"
                     (doty-tools--test--invoke-tool
                      "emacs_read_lines" (list :buffer_or_file name
                                               :start_line 1
                                               :include_line_numbers t))))
      (should
       (string-equal "Lines 3-3 of 3:\n3: Emacs!\n"
                     (doty-tools--test--invoke-tool
                      "emacs_read_lines" (list :buffer_or_file name
                                               :start_line 3
                                               :include_line_numbers t))))
      (should
       (string-equal "Lines 1-2 of 3:\n1: Hello!\n2: World!\n"
                     (doty-tools--test--invoke-tool
                      "emacs_read_lines" (list :buffer_or_file name
                                               :start_line 1
                                               :end_line 2
                                               :include_line_numbers t)))))))


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
        (let* ((use-regex (doty-tools-bool use-regex))
               (count 0)
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
                                                    t t)))
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

(ert-deftest doty-tools--test--emacs_search_text-no-regex ()
  "Tests for emacs_search_text without regular expressions."
  (with-temp-buffer
    (let ((name (buffer-name (current-buffer))))
      (insert "- [ ] Do it!\n")
      (insert "- [X] Done\n")
      (insert "- [ ] Another task\n")
      (should
       (string-equal "Match 1 (line 1):\n1: - [ ] Do it!\n\n"
                     (doty-tools--test--invoke-tool
                      "emacs_search_text" (list :buffer_or_file name
                                                :pattern "- [ ] Do it!"
                                                :context_lines 0
                                                :max_matches 1
                                                :use_regex :json-false))))

      (should
       (string-equal "Match 1 (line 1):
1: - [ ] Do it!

Match 2 (line 3):
3: - [ ] Another task

"
                     (doty-tools--test--invoke-tool
                      "emacs_search_text" (list :buffer_or_file name
                                                :pattern "- [ ]"
                                                :context_lines 0
                                                :max_matches 1000
                                                :use_regex :json-false)))))))

(ert-deftest doty-tools--test--emacs_search_text-regex ()
  "Tests for emacs_search_text without regular expressions."
  (with-temp-buffer
    (let ((name (buffer-name (current-buffer))))
      (insert "- [ ] Do it!\n")
      (insert "- [X] Done\n")
      (insert "- [ ] Another task\n")
      (should
       (string-equal "Match 1 (line 2):\n2: - [X] Done\n\n"
                     (doty-tools--test--invoke-tool
                      "emacs_search_text" (list :buffer_or_file name
                                                :pattern "D.ne"
                                                :context_lines 0
                                                :max_matches 1
                                                :use_regex t)))))))



(defun doty-tools--buffer-info (buffer-or-file)
  "Get metadata about BUFFER-OR-FILE.
Returns file path, modified status, major mode, size, line count, and more."
  (with-current-buffer (doty-tools--buffer-or-file buffer-or-file)
    (let ((file-path (buffer-file-name))
          (modified (buffer-modified-p))
          (mode major-mode)
          (size (buffer-size))
          (line-count (count-lines (point-min) (point-max)))
          (read-only buffer-read-only)
          (coding-system buffer-file-coding-system))
      (format "File path: %s\nModified: %s\nMajor mode: %s\nSize: %d bytes\nLine count: %d\nRead-only: %s\nEncoding: %s%s"
              (or file-path "Buffer has no file")
              (if modified "Yes" "No")
              mode
              size
              line-count
              (if read-only "Yes" "No")
              (or coding-system "default")
              (if file-path
                  (format "\nDirectory: %s"
                          (file-name-directory file-path))
                "")))))

(gptel-make-tool
 :name "emacs_buffer_info"
 :function #'doty-tools--buffer-info
 :description "Get metadata about a buffer or file including path, modified status, major mode, size, line count, read only status, and encoding."
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path"))
 :category "reading"
 :confirm nil
 :include t)

(defun doty-tools--get-project-root ()
  "Get the root directory of the current project."
  (project-root (project-current)))

(gptel-make-tool
 :name "get_project_root"
 :function #'doty-tools--get-project-root
 :description "Get the root directory of the current project."
 :args ()
 :category "reading"
 :confirm nil
 :include t)

(ert-deftest doty-tools--test--get_project_root ()
  "Tests for the get_project_root tool."
  (let* ((tf (make-temp-file "test-project-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory tf))
            (call-process "git" nil t nil "init" ".")
            (should
             (equal (file-name-as-directory tf)
                    (doty-tools--test--invoke-tool "get_project_root" ())))))

      (delete-directory tf t))))


(defun doty-tools--get-current-directory ()
  "Get the directory that relative paths are resolved to."
  default-directory)

(gptel-make-tool
 :name "get_current_directory"
 :function #'doty-tools--get-current-directory
 :description "Get the directory that relative paths are resolved relative to."
 :args ()
 :category "reading"
 :confirm nil
 :include t)

(ert-deftest doty-tools--test--get_current_directory ()
  "Tests for the get_current_directory tool."
  (let* ((tf (make-temp-file "test-cd-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory tf))
            (should
             (equal tf
                    (doty-tools--test--invoke-tool "get_current_directory" ())))))

      (delete-directory tf t))))

(defun doty-tools--search-project-regex (callback regex)
  "Search the current project for instances of a given REGEX.

Call CALLBACK when done."
  (let ((output-buffer (generate-new-buffer " *async-search-output*")))
    (with-current-buffer output-buffer
      ;; Make buffer lightweight - disable undo, make read-only
      (buffer-disable-undo)
      (setq-local inhibit-modification-hooks t)
      (setq-local inhibit-read-only t)) ; Temporarily allow writing by the process

    (set-process-sentinel
     (with-connection-local-variables
      (let* ((quoted-regex (shell-quote-argument regex))
             (rg-command (format "rg %s" quoted-regex)))
        (start-file-process-shell-command "gptel-async-search" output-buffer rg-command)))
     (lambda (_process event)
       (when (string-match "finished" event)
         (with-current-buffer output-buffer
           (funcall callback (buffer-string)))
         (kill-buffer output-buffer))))))

;; === Code Indexing

(defvar doty-tools--treesit-queries
  nil
  "Tree-sitter queries that we've registered for various languages.")

(defun doty-tools--register-treesit-mapper (lang query)
  "Register a mapper based on tree-sitter for LANG (a symbol) that uses QUERY."
  (let ((query (seq-concatenate 'list
                                '((ERROR) @err (MISSING) @err)
                                query)))
    (treesit-query-validate lang query)
    (setq doty-tools--treesit-queries
          (assq-delete-all lang doty-tools--treesit-queries))
    (push (cons lang (treesit-query-compile lang query))
          doty-tools--treesit-queries)))

(doty-tools--register-treesit-mapper
 'python
 `((module (expression_statement (assignment left: (identifier) @loc)))
   (class_definition
    name: (_) @loc
    superclasses: (_) @loc
    body: (block :anchor (expression_statement :anchor (string _ :*) @loc)) :?)
   (function_definition
    name: (_) @loc
    parameters: (_) @loc
    return_type: (_) :? @loc
    body: (block :anchor (expression_statement :anchor (string _ :*) @loc)) :?)))

(doty-tools--register-treesit-mapper
 'scala
 `((trait_definition
    name: (_) :? @loc
    extend: (_) :? @loc)

   (val_definition
    (modifiers (_)) :? @loc
    pattern: (_) @loc)

   (function_definition
    name: (_) :? @loc
    parameters: (_) :? @loc
    return_type: (_) :? @loc)

   (class_definition
    name: (_) :? @loc
    class_parameters: (_) :? @loc
    extend: (_) :? @loc)))

(doty-tools--register-treesit-mapper
 'rust
 `((mod_item
    name: (_) :? @loc)

   (struct_item
    (visibility_modifier (_)) :? @loc
    type_parameters: (_) :? @loc
    name: (_) @loc
    body: (_) @loc)

   (impl_item
    type_parameters: (_) :? @loc
    trait: (_) :? @loc
    type: (_) :? @loc)

   (function_item
    (visibility_modifier (_)) :? @loc
    name: (_) :? @loc
    type_parameters: (_) :? @loc
    parameters: (_) :? @loc
    return_type: (_) :? @loc)))

(defun doty-tools--node-is-error (node)
  "Return t if NODE is some kind of error."
  (or (treesit-node-check node 'has-error)
      (treesit-node-check node 'missing)))


(defun doty-tools--map-buffer (file-or-buffer)
  "Generate a map for FILE-OR-BUFFER."
  (with-current-buffer (doty-tools--buffer-or-file file-or-buffer)
    (let* ((registration (or (assoc (treesit-language-at (point-min))
                                    doty-tools--treesit-queries)
                             (error "Language '%s' not registered as a tree-sitter mapper"
                                    (treesit-language-at (point-min)))))

           (loc-queries (cdr registration))
           (decls (treesit-query-capture (treesit-buffer-root-node) loc-queries nil nil t))

           ;; Count errors.
           (error-nodes (seq-filter #'doty-tools--node-is-error decls))
           (error-count (length error-nodes))

           ;; Remove errors, don't care anymore.
           (decls (seq-filter (lambda (node) (not (doty-tools--node-is-error node))) decls))
           (ranges (mapcar (lambda (node)
                             (cons (treesit-node-start node)
                                   (treesit-node-end node)))
                           decls))

           ;; Sort the result
           (ranges (sort ranges :key #'car :in-place t)))

      (save-excursion
        (let* ((line-count (count-lines (point-min) (point-max)))
               (width (1+ (floor (log line-count 10))))
               (line-format (format "%%%dd: %%s" width))
               (result-lines nil)
               (line-number 1))

          (if (> error-count 0)
              (push (format "[STATUS: ERRORS] File contained %d parse errors"
                            error-count)
                    result-lines)
            (push "[STATUS: SUCCESS] File parsed successfully" result-lines))
          (push "" result-lines)

          (widen)
          (goto-char (point-min))
          (while (and ranges (not (eobp)))
            (let ((line-start (line-beginning-position))
                  (line-end (line-end-position)))

              ;; Remove the head of the ranges while the head is
              ;; before the current line.
              (while (and ranges (< (cdar ranges) line-start))
                (setq ranges (cdr ranges)))

              ;; When the range intersects this line, append this line.
              (when (and ranges
                         (>= (cdar ranges) line-start)
                         (<= (caar ranges) line-end))

                (push (format line-format
                              line-number
                              (buffer-substring-no-properties line-start line-end))
                      result-lines))
            (setq line-number (1+ line-number))
            (forward-line 1)))

          (mapconcat 'identity (nreverse result-lines) "\n"))))))

(gptel-make-tool
 :name "emacs_get_code_map"
 :function #'doty-tools--map-buffer
 :description "Returns structural outline of code files with declarations and their line numbers. Includes parse status. Cheaper than reading the entire file when supported. Supports python, scala, and rust code.

Example:
[STATUS: ERRORS] File contained 2 parse errors

10: LOG_ROOT = pathlib.Path.home() / \".local\" / \"share\" / \"goose\" / \"sessions\"
12: class MyClass(object):
15:     def method(self) -> int:
19: def export_session(session: str):
80: SESSION = \"20250505_222637_59fcedc5\""
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path"))
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
        (if (> line-number 0)
            (progn
              (goto-char (point-min))
              (forward-line (1- line-number)))
          (goto-char (point-max))
          (forward-line (1+ line-number)))
        (if (doty-tools-bool at-end)
            (end-of-line)
          (beginning-of-line))
        (insert text)
        (format "Inserted text at %s of line %d in %s"
                (if (doty-tools-bool at-end) "end" "beginning")
                line-number
                (if (bufferp buffer-or-file)
                    (buffer-name buffer-or-file)
                  buffer-or-file))))))

(gptel-make-tool
 :name "emacs_insert_at_line"
 :function #'doty-tools--insert-at-line
 :description "Insert text at the beginning or end of specified line. Negative numbers are indexed from the end of the buffer. Line 1 is the first line. Line -1 is the last line. Blanks are *not* added automatically, so be sure to include newlines where appropriate. Be sure to carefully consider the context of the insertion point when modifying files, to make sure that the line you specify is where the text should actually go. If inserting at the beginning of the file, insert at the beginning line 1. If inserting at the end of the file, insert at the end of line -1."
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path")
         (:name "line_number"
          :type integer
          :description "1-based index of line to insert at")
         (:name "text"
          :type string
          :description "Text to insert")
         (:name "at_end"
          :type boolean
          :optional t
          :description "If true, insert at end of line; otherwise at beginning (optional)"))
 :category "editing"
 :confirm nil
 :include t)

(ert-deftest doty-tools--test--emacs_insert_line ()
  "Tests for the emacs_insert_line tool."
  (with-temp-buffer
    (doty-tools--test--invoke-tool
     "emacs_insert_at_line" (list :buffer_or_file (buffer-name)
                                  :line_number 1
                                  :text "Hello"))
    (should (equal "Hello" (buffer-string)))

    (doty-tools--test--invoke-tool
     "emacs_insert_at_line" (list :buffer_or_file (buffer-name)
                                  :line_number 1
                                  :text " world!"
                                  :at_end t))

    (should (equal "Hello world!" (buffer-string)))

    (doty-tools--test--invoke-tool
     "emacs_insert_at_line" (list :buffer_or_file (buffer-name)
                                  :line_number 1
                                  :text "Here is the message:\n"))

    (should (equal "Here is the message:\nHello world!" (buffer-string)))

    (doty-tools--test--invoke-tool
     "emacs_insert_at_line" (list :buffer_or_file (buffer-name)
                                  :line_number -2
                                  :text "No really:\n"))

    (should (equal "No really:\nHere is the message:\nHello world!" (buffer-string)))

    (doty-tools--test--invoke-tool
     "emacs_insert_at_line" (list :buffer_or_file (buffer-name)
                                  :line_number -1
                                  :text "\nThat's all!"
                                  :at_end t))

    (should (equal "No really:\nHere is the message:\nHello world!\nThat's all!" (buffer-string)))

    ))


(defun doty-tools--replace-text (buffer-or-file from-text to-text use-regex replace-all)
  "Replace occurrences of FROM-TEXT with TO-TEXT in BUFFER-OR-FILE.
If USE-REGEX is non-nil, treat FROM-TEXT as a regular expression.

If REPLACE-ALL is non-nil, replace all occurrences, otherwise just the
 first one."
  (with-current-buffer (doty-tools--buffer-or-file buffer-or-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((use-regex (doty-tools-bool use-regex))
               (count 0)
               (search-pattern (if use-regex
                                   (doty-tools--convert-regex from-text)
                                 from-text))
               (search-fn (if use-regex 're-search-forward 'search-forward))
               (case-fold-search nil))
          (while (and (funcall search-fn search-pattern nil t)
                      (or replace-all (= count 0)))
            (setq count (1+ count))
            (replace-match to-text t (not use-regex)))
          (format "Replaced %d occurrence%s of %s with %s in %s"
                  count
                  (if (= count 1) "" "s")
                  from-text
                  to-text
                  (if (bufferp buffer-or-file)
                      (buffer-name buffer-or-file)
                    buffer-or-file)))))))

(gptel-make-tool
 :name "emacs_replace_text"
 :function #'doty-tools--replace-text
 :description "Replace occurrences of text in a buffer or file. Can use regex patterns and supports replacing single or all occurrences."
 :args '((:name "buffer_or_file"
          :type string
          :description "Buffer name or file path")
         (:name "from_text"
          :type string
          :description "Text to replace")
         (:name "to_text"
          :type string
          :description "Replacement text")
         (:name "use_regex"
          :type boolean
          :description "Whether from_text is a regex")
         (:name "replace_all"
          :type boolean
          :description "Replace all occurrences if true"))
 :category "editing"
 :confirm nil
 :include t)

(ert-deftest doty-tools--test--emacs_replace_text-no-regex ()
  "Test emacs_replace_text with no regex."
  (with-temp-buffer
    (insert "- [X] `size: Int`\n")
    (insert "- [ ] `knownSize: Int`\n")
    (insert "- [ ] `apply(i: Int): Char`\n")
    (doty-tools--test--invoke-tool
     "emacs_replace_text" (list :buffer_or_file (buffer-name)
                                :from_text "- [ ] `knownSize: Int`"
                                :to_text "- [X] `knownSize: Int`"
                                :use_regex :json-false
                                :replace_all :json-false))
    (should (equal (buffer-string)
                   "- [X] `size: Int`
- [X] `knownSize: Int`
- [ ] `apply(i: Int): Char`
"))))

(ert-deftest doty-tools--test--emacs_replace_text-no-regex-backslash ()
  "Test emacs_replace_text with no regex."
  (with-temp-buffer
    (insert "WOAH / MAN\n")
    (doty-tools--test--invoke-tool
     "emacs_replace_text" (list :buffer_or_file (buffer-name)
                                :from_text "WOAH / MAN"
                                :to_text "WOAH \\ MAN"
                                :use_regex :json-false))
    (should (equal (buffer-string)
                   "WOAH \\ MAN\n"))))

(defun doty-tools--delete-lines (buffer-or-file start-line &optional end-line)
  "Delete lines from START-LINE to END-LINE in BUFFER-OR-FILE.
If END-LINE is not provided, only delete START-LINE."
  (let ((buffer (doty-tools--buffer-or-file buffer-or-file))
        (end (or end-line start-line)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- start-line))
        (let ((beg (point)))
          (forward-line (1+ (- end start-line)))
          (delete-region beg (point)))))
    (format "Deleted lines %d to %d in %s"
            start-line
            end
            (if (bufferp buffer-or-file)
                (buffer-name buffer-or-file)
              buffer-or-file))))

(gptel-make-tool
 :name "emacs_delete_lines"
 :function #'doty-tools--delete-lines
 :description "Delete specified line range."
 :args (list '(:name "buffer_or_file"
                :type string
                :description "Buffer name or file path")
             '(:name "start_line"
                :type integer
                :description "First line to delete")
             '(:name "end_line"
               :type integer
               :optional t
               :description "Last line to delete (optional - single line if omitted)"))
 :category "editing"
 :confirm nil
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
     (start-file-process-shell-command "gptel-async-command" output-buffer command)
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

(provide 'doty-tools)
;;; doty-tools.el ends here
