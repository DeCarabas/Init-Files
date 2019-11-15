;;; cider-buck.el --- Cider + Buck = Love            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  John Doty

;; Author: doty <doty@fb.com>
;; Keywords: languages, clojure, cider

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

;; This is just a bunch of funcitons to make cider work well with a buck
;; project.

;;; Code:

(defun cider-buck--nrepl-target (src-file)
  "Get the nrepl buck target of SRC-FILE."
  (with-temp-buffer
    (let ((stdout-buffer (current-buffer)))
      (with-temp-buffer
        (let* ((stderr-buffer (current-buffer))
               (proc (make-process :name "buck-query"
                                   :command (list "buck" "query" "owner('%s')" src-file)
                                   :buffer stdout-buffer
                                   :noquery 't
                                   :stderr stderr-buffer)))
          (while (accept-process-output proc))
          (unless (eq (process-exit-status proc) 0)
            (error (buffer-string))))))

    (goto-char (point-min))
    (while (re-search-forward "__source-java[0-9]+$" nil t)
      (replace-match "_nrepl"))

    (goto-char (point-min))
    (buffer-substring (point-min) (line-end-position))))

(defun cider-buck--jack-in-cmd ()
  "Get it."
  (let ((src-file (buffer-file-name (current-buffer))))
    (concat "buck run " (cider-buck--nrepl-target src-file) " -- --middleware '["
            (mapconcat (apply-partially #'format "\"%s\"")
                       (cider-jack-in-normalized-nrepl-middlewares)
                       ", ")
            "]'")))

(defun cider-buck--project-dir ()
  "Get the project dir by looking for a targets file."
  (locate-dominating-file (buffer-file-name (current-buffer)) "TARGETS"))

(defun cider-buck-jack-in ()
  "Start an nREPL server for the current file and connect to it."
  (interactive)
  (nrepl-start-server-process
   (cider-buck--project-dir)
   (cider-buck--jack-in-cmd)
   (lambda (server-buffer)
     (cider-connect-sibling-clj () server-buffer))))


(provide 'cider-buck)
;;; cider-buck.el ends here
