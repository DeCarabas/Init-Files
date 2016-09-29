;; Publisher from org-mode to Quip.  (Export as markdown, push as a new
;; thread or amend to existing quip thread.)

(require 'cl-extra)
(require 'ox-md)
(require 'quip-api)


(defun org-quip-get-thread-identifier ()
  (org-entry-get nil "quip-id" t))

(defun org-quip-put-thread-identifier (identifier)
  (save-excursion
    (while (org-up-heading-safe))
    (org-entry-put nil "quip-id" identifier)))

(defun org-quip-publish-quip (content)
  "Publish content as a new Quip document. Returns the ID of the new document."
  (let ((response (quip-new-document content)))
    (cdr (assoc 'id (cdr (assoc 'thread response))))))

(defun org-quip-publish-to-quip ()
  (interactive)
  (let
      ((quip-id (org-quip-get-thread-identifier))
       (content (org-export-as 'md)))
    (if quip-id
        (org-quip-update-quip quip-id content)
      (let ((new-quip-id (org-quip-publish-quip content)))
        (org-quip-put-thread-identifier new-quip-id)))))

(provide 'ox-quip)
