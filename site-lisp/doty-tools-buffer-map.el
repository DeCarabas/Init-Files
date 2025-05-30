;;; doty-tools-buffer-map.el --- gptel tools for getting buffer summaries for coding     -*- lexical-binding: t; -*-

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

;; These tools attempt to put together a condensed summary of a buffer for an
;; AI to think about. The idea is that we can provide an accurate overview or
;; summary without wasting tokens skimming through implementation details.

;;; Code:
(require 'gptel)
(require 'treesit)

(require 'doty-tools-utils)

(defvar doty-tools--treesit-queries
  nil
  "Tree-sitter queries that we've registered for various languages.")

(defun doty-tools--register-treesit-mapper (lang query)
  "Register a mapper based on tree-sitter for LANG (a symbol) that uses QUERY."
  (when (and (treesit-available-p)
             (treesit-language-available-p lang))
    (let ((query (seq-concatenate 'list
                                  '((ERROR) @err (MISSING) @err)
                                  query)))
      (treesit-query-validate lang query)
      (setq doty-tools--treesit-queries
            (assq-delete-all lang doty-tools--treesit-queries))
      (push (cons lang (treesit-query-compile lang query))
            doty-tools--treesit-queries))))

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
              (push (format "[STATUS: ERRORS] File contained %d parse errors, results might be wrong"
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


(defun doty-tools-register-mappers ()
  "Re-register all available code mappers."
  (interactive)
  (gptel-make-tool
   :name "emacs_get_code_map"
   :function #'doty-tools--map-buffer
   :description (format
                 "Returns structural outline of code files with declarations and their line numbers. Includes parse status. Cheaper than reading the entire file when supported. Supported languages are: %s.

Example:
[STATUS: ERRORS] File contained 2 parse errors, results might be wrong

10: LOG_ROOT = pathlib.Path.home() / \".local\" / \"share\" / \"goose\" / \"sessions\"
12: class MyClass(object):
15:     def method(self) -> int:
19: def export_session(session: str):
80: SESSION = \"20250505_222637_59fcedc5\""
                 (let ((supported (string-join
                                   (--map (symbol-name (car it)) doty-tools--treesit-queries)
                                   ", ")))
                   (if (string-empty-p supported)
                       "none"
                     supported)))
   :args '((:name "buffer_or_file"
                  :type string
                  :description "Buffer name or file path"))
   :category "reading"
   :confirm nil
   :include t))

(doty-tools-register-mappers)

(provide 'doty-tools-buffer-map)
;;; doty-tools-buffer-map.el ends here
