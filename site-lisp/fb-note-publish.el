;;; fb-note-publish.el --- Publish a markdown doc as a note  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 John Doty

;; Author: John Doty <doty@fb.com>
;; Keywords: wp, comm

;;; Commentary:

;; This module provides some functions for publishing a markdown document as
;; a facebook note.  (It's derived from the disseminate project's publishing
;; library, and it uses the same interngraph endpoint.)

;;; Code:
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-vars)

;; This is the unique app-id for authenticating to interngraph.
(defconst fbn--app-id "1910641509202249")

;; Go ahead and change this to '$USER.sb' if you want to test against your
;; devserver.
(defconst fbn--interngraph-tier "intern")

(defun fbn--keychain (secret secret-group &optional secrets-tool-path)
  "Extract the SECRET in SECRET-GROUP using the secret_tool at SECRETS-TOOL-PATH."
  (let ((tool-path (or secrets-tool-path "/usr/local/bin/secrets_tool")))
    (with-temp-buffer
      (call-process tool-path nil t nil "get_from_group" secret secret-group)
      (string-trim (buffer-string)))))

(defun fbn--get-secret-token ()
  "Extract the secret interngraph token for the disseminate library."
  (fbn--keychain "INTERNGRAPH_TOKEN" "FBNOTE_PUBLISH"))

(defun fbn--intern-graph-url (path)
  "Construct the url to PATH on the intern graph."
  (concat "https://interngraph." fbn--interngraph-tier ".facebook.com/" path))

(defun fbn--get-unixname ()
  "Get the unixname of the currently executing user."
  (getenv "USER"))

(defun fbn--invoke-json (path params)
  "Invoke the intern graph PATH with PARAMS.

Params should be a list of key/value cons cells describing form request data.
We return the resulting JSON, decoded."
  (let
      ((url (fbn--intern-graph-url path))
       (url-request-method "POST")
       (url-proxy-services nil)
       (url-request-data
        (mapconcat (lambda (pair)
                     (format "%s=%s"
                             (car pair)
                             (url-hexify-string (cdr pair))))
                   params
                   "&")))
    (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^$")
        (json-read)
        )))

(defun fbn--publish (title note-id note-html)
  "Publish a note with specified TITLE, NOTE-ID, and NOTE-HTML.

NOTE-ID may be nil to create a new note, or the ID of an existing note.  This
function returns the ID of the created note."
  (let ((json-response (fbn--invoke-json
                        "research/note/"
                        `((app      . ,fbn--app-id)
                          (token    . ,(fbn--get-secret-token))
                          (title    . ,title)
                          (unixname . ,(fbn--get-unixname))
                          (note_id  . ,note-id)
                          (html     . ,note-html)))))
    (cdr (assoc 'note_id json-response))))

(defun fbn--convert (&optional buffer)
  "Convert the specified BUFFER to the correct html.

Here we force the use of pandoc since we don't want to have to do
that silly thing where we split out the YAML frontmatter first,
and of course we expect that frontmatter to exist so that we can
stash the note ID there."
  (let ((in-buffer (or buffer (current-buffer))))
    (with-temp-buffer
      (let ((out-buffer (current-buffer)))
        (with-current-buffer in-buffer
          (call-process-region (point-min) (point-max)
                               "pandoc" nil out-buffer nil
                               "-f" "markdown+yaml_metadata_block"
                               "-t" "html"
                               ))
        (string-trim (buffer-string))))))

(defun fbn--frontmatter-end (&optional buffer)
  "Go to the end of the frontmatter in BUFFER."
  (let ((in-buffer (or buffer (current-buffer))))
    (with-current-buffer in-buffer
      (let ((frontmatter-start))
        (goto-char (point-min))
        (unless (looking-at "^---$")
          (error "This buffer does not have frontmatter"))
        (setq frontmatter-start (match-end 0))
        (goto-char frontmatter-start)
        (unless (re-search-forward "^---$" (point-max) t)
          (error "Cannot find the end of the frontmatter"))
        (goto-char (match-beginning 0))
        (point)))))

(defun fbn--unquote (value)
  "Unquote the specified VALUE, if it is quoted."
  (cond
   ;; Appears to be quoted.
   ((string-prefix-p "\"" value)
    (let* ((value (replace-regexp-in-string "\\\\\\\"" "\"" value))
           (value (substring value 1 -1)))
      value))
   ;; Not quoted.
   (t value)))

(ert-deftest fbn-test-unquote ()
  "Test that unquoting front matter values works properly"
  (should (equal (fbn--unquote "Foo") "Foo"))
  (should (equal (fbn--unquote "\"Foo\"") "Foo"))
  (should (equal (fbn--unquote "\" \\\" \"") " \" ")))

(defun fbn--frontmatter-field (field buffer)
  "Fetch the specified FIELD from the specified BUFFER.

If successful, this function returns the field value (as a
string), and also leaves the point at the beginning of the note
entry."
  (with-current-buffer buffer
    (goto-char (fbn--frontmatter-end buffer))
    (if (re-search-backward (concat "^" field ": \\(\.\+\\)") nil t)
        (fbn--unquote (match-string-no-properties 1)))))

(defun fbn--set-frontmatter-field (field value buffer)
  "Set the specified FIELD to VALUE in the specified BUFFER.

This replaces the existing field value, if present, otherwise it
adds it."
  (let ((in-buffer (or buffer (current-buffer))))
    (with-current-buffer in-buffer
      (let ((existing-value))
        (setq existing-value (fbn--frontmatter-field field buffer))
        (unless (equal value existing-value)
          (if existing-value
              (delete-region (line-beginning-position)
                             (progn (forward-line 1) (point))))
          (fbn--frontmatter-end buffer)
          (insert (concat field ": " value "\n"))
          )))))

(defun fbn--note-id (&optional buffer)
  "Get the existing note ID out of BUFFER.

If successful, this function returns the note ID (as a string), and also leaves
the point at the beginning of the note entry."
  (fbn--frontmatter-field "fbnote" buffer))

(defun fbn--note-title (&optional buffer)
  "Get the existing note title out of BUFFER.

If successful, this function returns the note title (as a
string), and also leaves the point at the beginning of the note
entry."
  (fbn--frontmatter-field "title" buffer))

(defun fbn--set-note-id (id &optional buffer)
  "Set the specified ID into the frontmatter in BUFFER.

This replaces the existing ID, if present, otherwise it adds it."
  (let ((in-buffer (or buffer (current-buffer))))
    (with-current-buffer in-buffer
      (let ((existing-id))
        (setq existing-id (fbn--note-id in-buffer))
        (unless (equal id existing-id)
          (if existing-id
              (delete-region (line-beginning-position)
                             (progn (forward-line 1) (point))))
          (fbn--frontmatter-end buffer)
          (insert (concat "fbnote: " id "\n"))
          )))))

(defun fbnote-publish-markdown (&optional buffer)
  "Publish the current markdown BUFFER as a facebook note."
  (interactive)
  (let ((in-buffer (or buffer (current-buffer))))
    (save-excursion
      (with-current-buffer in-buffer
        (let ((note-id (fbn--note-id in-buffer))
              (note-title (fbn--note-title in-buffer))
              (note-html (fbn--convert in-buffer))
              (new-id))
          (setq new-id (fbn--publish note-title note-id note-html))
          (fbn--set-note-id new-id)
          (message (concat "Published note " new-id)))))))

(provide 'fb-note-publish)
;;; fb-note-publish.el ends here
