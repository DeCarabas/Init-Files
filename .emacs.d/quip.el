;; Quip API

(require 'cl-extra)
(require 'json)
(require 'url)

;; TODO: Make this actually a variable.
(defconst doty-quip-api-key
  "UU9RQU1BcTNCdU0=|1506626374|H13gPE9bkDAkHQp9PtTlX8i78wYvtSBwEJgLAuChnXs=")

(defun quip-invoke-json (path method params)
  "Submit a request to the Quip API. Returns the parsed JSON from the response."
  (let
      ((url (concat "https://platform.quip.com/1/" path))
       (url-request-method method)
       (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " doty-quip-api-key))
                                    ("Content-Type" . "application/x-www-form-urlencoded")))
       (url-request-data
        (mapconcat (lambda (pair) (format "%s=%s" (car pair) (url-hexify-string (cdr pair))))
                   params
                   "&")))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun quip-new-document (content)
  "Create a new Quip document with the provided content. Returns the parsed JSON response."
  (quip-invoke-json "threads/new-document"
                    "POST"
                    `((format  . "markdown")
                      (content . ,content))))

(defun quip-get-thread (id)
  "Get the Quip thread with the specified ID. Returns the parsed JSON response."
  (quip-invoke-json (concat "threads/" id) "GET" nil))

(defconst quip-location-append 0)
(defconst quip-location-prepend 1)
(defconst quip-location-after-section 2)
(defconst quip-location-before-section 3)
(defconst quip-location-replace-section 4)
(defconst quip-location-delete-section 5)

(defun quip-thread-append (thread content)
  "Append the content to the specified thread."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     . "markdown")
                      (content    . ,content)
                      (location   . ,quip-location-append)
                      (thread_id  . ,thread))))

(defun quip-thread-prepend (thread content)
  "Prepend the content to the specified thread."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     . "markdown")
                      (content    . ,content)
                      (location   . ,quip-location-prepend)
                      (thread_id  . ,thread))))

(defun quip-thread-append-after (thread section content)
  "Append the content to the specified thread after the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     . "markdown")
                      (content    . ,content)
                      (location   . ,quip-location-append-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-prepend-before (thread section content)
  "Prepend the content to the specified thread before the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     . "markdown")
                      (content    . ,content)
                      (location   . ,quip-location-prepend-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-replace-section (thread section content)
  "Replace the content of the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     . "markdown")
                      (content    . ,content)
                      (location   . ,quip-location-replace-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-delete-section (thread section)
  "Delete the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     . "markdown")
                      (content    . ,content)
                      (location   . ,quip-location-delete-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(provide 'quip-api)
