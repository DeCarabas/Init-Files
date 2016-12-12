;; Quip API

(require 'cl-extra)
(require 'json)
(require 'url)

;; Customization groups
(defgroup quip-api nil
  "Customization options for using the Quip API")

(defcustom quip-api-key nil
  "Your API key for Quip. Get it from https://fb.quip.com/api/personal-token"
  :type 'string)

(defun quip-invoke-json (path method params)
  "Submit a request to the Quip API. Returns the parsed JSON from the response."
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
  "Create a new Quip document with the provided content. Returns the parsed JSON response."
  (quip-invoke-json "threads/new-document"
                    "POST"
                    `((format  . ,(or format "markdown"))
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

(defun quip-thread-append (thread content &optional format)
  "Append the content to the specified thread."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-append)
                      (thread_id  . ,thread))))

(defun quip-thread-prepend (thread content &optional format)
  "Prepend the content to the specified thread."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-prepend)
                      (thread_id  . ,thread))))

(defun quip-thread-append-after (thread section content &optional format)
  "Append the content to the specified thread after the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-append-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-prepend-before (thread section content &optional format)
  "Prepend the content to the specified thread before the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-prepend-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-replace-section (thread section content &optional format)
  "Replace the content of the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-replace-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))

(defun quip-thread-delete-section (thread section &optional format)
  "Delete the specified section."
  (quip-invoke-json "threads/edit-document"
                    "POST"
                    `((format     .  ,(or format "markdown"))
                      (content    . ,content)
                      (location   . ,quip-location-delete-section)
                      (section_id . ,section)
                      (thread_id  . ,thread))))


;;; Content parsing functions

(defun quip-get-item-type (item)
  (let ((elem-type (car item))
        (attribs (cadr item)))
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
      (letrec ((style           (assoc-default 'data-section-style attribs))
               (inner           (caddr item))
               (inner-elem-type (car inner)))
        (cond
         ((eq inner-elem-type 'ul) 'ul)
         ((eq inner-elem-type 'ol) 'ol)
         (t                        'unrecognized-inner))))
     (t                          'unrecognized))))

(defun quip-get-item-id (type item)
  (let ((attribs (cadr item)))
    (cond
     ((or (eq type 'ul)  ;; Nested IDs.
          (eq type 'ol))
      (letrec ((inner (caddr item))
               (inner-attribs (cadr inner)))
        (assoc-default 'id inner-attribs)))
     (t (assoc-default 'id attribs)))))

(defun quip-get-item-content (type item)
  (cond
   ((or (eq type 'ul)  ;; Nested Content
        (eq type 'ol))
    (letrec ((inner (caddr item))
             (inner-elems (cddr inner)))
      (mapcar #'quip-get-item-from-element inner-elems)))
   (t (caddr item))))

(defun quip-get-item-from-element (element)
  (letrec
      ((item-type (quip-get-item-type element))
       (item-id (quip-get-item-id item-type element))
       (item-content (quip-get-item-content item-type element)))
    `(,item-type ,item-id ,item-content)))


(defun quip-parse-html-content (content)
  (with-temp-buffer
    (insert content)
    (letrec
        ((html (libxml-parse-html-region (point-min) (point-max)))
         (raw-items (cddr (caddr html)))
         (html-items (remove-if #'stringp raw-items)))

      (mapcar #'quip-get-item-from-element html-items)
    )))

;; (prin1
;;  (quip-parse-html-content
;;   (assoc-default 'html (quip-get-thread "idflAWG6R6Uu"))))

(provide 'quip-api)
