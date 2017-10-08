;;; quip.el --- Quip API client for emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)


;; Customization groups
(defgroup quip-api nil
  "Customization options for using the Quip API"
  :prefix "quip-"
  :group 'external
  :tag "Quip")

(defun quip-api-key ()
  "Retrieve the stored API key.

The API key comes from auth-source, however you have that set up.  If you don't
have a key, get one from https://fb.quip.com/api/personal-token.  Put it in the
password field of an entry for host 'quip'."
  (let ((api-key (cadr (auth-source-user-and-password "quip"))))
    (when (not api-key)
      (error "No API key set for quip in ~/.authinfo"))
    api-key))


(defun quip--handle-error (result)
  "Report an error if RESULT contains one."
  (when (alist-get 'error result)
    (error "Quip error: %s" (alist-get 'error_description result)))
  result)

(defun quip-invoke-json (path method params)
  "Make a request to the Quip API, and return the parsed JSON from the response.

A Quip API call involves issuing an HTTP request to path PATH,
with method METHOD, and parameters PARAMS.  This routine knows the
base URL and adds the necessary headers."
  (let
      ((url (concat "https://platform.quip.com/1/" path))
       (url-request-method method)
       (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (quip-api-key)))
                                    ("Content-Type" . "application/x-www-form-urlencoded")))
       (url-request-data
        (mapconcat (lambda (pair) (format "%s=%s" (car pair) (url-hexify-string (cdr pair))))
                   params
                   "&")))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (quip--handle-error (json-read)))))

(defun quip-new-document (content &optional format)
  "Create a new Quip document with the provided CONTENT.

This function returns the parsed JSON response.  The optional
FORMAT argument is one of 'html' or 'markdown', and indicates
that the content should be interpreted as such."
  (quip-invoke-json "threads/new-document"
                    "POST"
                    `((format  . ,(or format "markdown"))
                      (content . ,content))))

(defun quip-get-thread (id)
  "Get the Quip thread with the specified ID.  Return the parsed JSON response."
  (quip-invoke-json (concat "threads/" id) "GET" nil))

(defconst quip-location-append 0)
(defconst quip-location-prepend 1)
(defconst quip-location-after-section 2)
(defconst quip-location-before-section 3)
(defconst quip-location-replace-section 4)
(defconst quip-location-delete-section 5)

(defun quip-thread-append (thread content &optional format)
                                        ; checkdoc-order: nil
  "Append CONTENT to the specified THREAD.

The optional FORMAT argument is one of 'html' or 'markdown', and
indicates how the content is to be interpreted."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-append)
                      (thread_id  . ,thread))))

(defun quip-thread-prepend (thread content &optional format)
                                        ; checkdoc-order: nil
  "Prepend CONTENT to the specified THREAD.

The optional FORMAT argument is one of 'html' or 'markdown', and
indicates how the content is to be interpreted."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-prepend)
                      (thread_id  . ,thread))))

(defun quip-thread-append-after (thread section content &optional format)
                                        ; checkdoc-order: nil
  "Append CONTENT to specified SECTION in the specified THREAD.

The content is appended after the specified section.

The optional FORMAT argument is one of 'html' or 'markdown', and
indicates how the content is to be interpreted."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-after-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-prepend-before (thread section content &optional format)
                                        ; checkdoc-order: nil
  "Prepend CONTENT to the specified SECTION of THREAD.

The content is added before the specified section.

The optional FORMAT argument is one of 'html' or 'markdown', and
indicates how the content is to be interpreted."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-before-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-replace-section (thread section content &optional format)
                                        ; checkdoc-order: nil
  "Replace the specified SECTION of THREAD with the specified CONTENT.

The optional FORMAT argument is one of 'html' or 'markdown', and
indicates how the content is to be interpreted."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-replace-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-delete-section (thread section)
                                        ; checkdoc-order: nil
  "Delete the specified SECTION of THREAD."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((location   . ,quip-location-delete-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(provide 'quip)
;;; quip.el ends here
