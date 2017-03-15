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

(defcustom quip-api-key ""
  "Your API key for Quip.

Get it from https://fb.quip.com/api/personal-token."
  :type 'string
  :group 'quip-api)

(defun quip-invoke-json (path method params)
  "Make a request to the Quip API, and return the parsed JSON from the response.

A Quip API call involves issuing an HTTP request to path PATH,
with method METHOD, and parameters PARAMS.  This routine knows the
base URL and adds the necessary headers."
  (if (not quip-api-key)
      (error "%s"
             "The custom variable quip-api-key is undefined. Use custom-set-variable to set it before using quip."))
  (let
      ((url (concat "https://platform.quip.com/1/" path))
       (url-request-method method)
       (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " quip-api-key))
                                    ("Content-Type" . "application/x-www-form-urlencoded")))
       (url-request-data
        (mapconcat (lambda (pair) (format "%s=%s" (car pair) (url-hexify-string (cdr pair))))
                   params
                   "&")))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

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


;;; Content parsing functions

(defun quip-get-item-type (item)
  "Classify the specified HTML ITEM."
  (let ((elem-type (car item)))
    (cond
     ((eq elem-type 'p)          'paragraph)
     ((eq elem-type 'h1)         'h1)
     ((eq elem-type 'h2)         'h2)
     ((eq elem-type 'h3)         'h3)
     ((eq elem-type 'blockquote) 'block-quote)
     ((eq elem-type 'q)          'pull-quote)
     ((eq elem-type 'pre)        'code-block)
     ((eq elem-type 'li)         'list-item)
     ((eq elem-type 'span)       'span)
     ((eq elem-type 'div)
      (letrec ((inner           (cl-caddr item))
               (inner-elem-type (car inner)))
        (cond
         ((eq inner-elem-type 'ul) 'ul)
         ((eq inner-elem-type 'ol) 'ol)
         (t                        'unrecognized-inner))))
     (t                          'unrecognized))))

(defun quip-get-item-id (item type)
  "Extract the ID from the provided ITEM given its TYPE."
  (let ((attribs (cadr item)))
    (cond
     ((or (eq type 'ul)  ;; Nested IDs.
          (eq type 'ol))
      (letrec ((inner (cl-caddr item))
               (inner-attribs (cadr inner)))
        (assoc-default 'id inner-attribs)))
     (t (assoc-default 'id attribs)))))

(defun quip-get-item-content (item type)
  "Extract the content from the provided ITEM given its TYPE."
  (cond
   ((or (eq type 'ul)  ;; Nested Content
        (eq type 'ol))
    (letrec ((inner (cl-caddr item))
             (inner-elems (cddr inner)))
      (mapcar #'quip-get-item-from-element inner-elems)))
   (t (cl-caddr item))))

(cl-defstruct quip-item type id content)

(defun quip-get-item-from-element (element)
  "Construct a (type, id, content) list from the given ELEMENT."
  (letrec
      ((item-type (quip-get-item-type element))
       (item-id (quip-get-item-id element item-type))
       (item-content (quip-get-item-content element item-type)))
    (make-quip-item
     :type item-type
     :id item-id
     :content item-content)))


(defun quip-parse-html-content (html)
  "Parse the provided HTML into a list of (type, item, content) lists."
  (with-temp-buffer
    (insert html)
    (letrec
        ((parsed-html (libxml-parse-html-region (point-min) (point-max)))
         (raw-items (cddr (cl-caddr parsed-html)))
         (html-items (cl-remove-if #'stringp raw-items)))

      (mapcar #'quip-get-item-from-element html-items)
    )))

;; (prin1
;;  (quip-parse-html-content
;;   (assoc-default 'html (quip-get-thread "idflAWG6R6Uu"))))

(provide 'quip)
;;; quip.el ends here
