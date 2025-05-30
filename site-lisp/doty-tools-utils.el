;;; doty-tools-utils.el --- utilities for doty-tools     -*- lexical-binding: t; -*-

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

;; These are just helper functions that support the rest of Doty's tools.

;;; Code:
(require 'dash)
(require 'gptel)

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

(defun doty-tools--buffer-or-file (buffer-or-file)
  "Return a buffer for BUFFER-OR-FILE for the well-behaved LLM tool.

If it is a buffer object, just return it. If there is exactly one buffer
that matches that name, return that buffer. Otherwise return nil. Otherwise,
assume that the LLM intends to open a file with that name and visit it."
  (cond
   ((bufferp buffer-or-file) buffer-or-file)
   ((length= (match-buffers (regexp-quote buffer-or-file)) 1)
    (car (match-buffers (regexp-quote buffer-or-file))))
   (t
    (find-file-noselect (expand-file-name buffer-or-file)))))
;; (t (error "File '%s' doesn't exist and does not name an open buffer"
;;           buffer-or-file))))


(provide 'doty-tools-utils)
;;; doty-tools-utils.el ends here
