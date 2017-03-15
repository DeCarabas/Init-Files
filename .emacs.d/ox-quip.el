;;; ox-quip.el -- Publish from org-mode to Quip.

;;; Commentary:
;; Publisher from org-mode to Quip.  (Export as markdown, push as a new
;; thread or amend to existing quip thread.)

;;; Code:
(require 'cl-extra)
(require 'ox-md)
(require 'quip)

(defun org-quip--get-thread-identifier ()
  "Get the Quip thread identifier from the doc in the current buffer, if any."
  (org-entry-get nil "quip-id" t))

(defun org-quip--put-thread-identifier (identifier)
  "Put the Quip thread identifier in IDENTIFIER into the doc."
  (save-excursion
    (while (org-up-heading-safe))
    (org-entry-put nil "quip-id" identifier)))

(defun org-quip--publish-quip (content)
  "Publish CONTENT as a new Quip document.  Return the ID of the new document."
  (let ((response (quip-new-document content)))
    (cdr (assoc 'id (cdr (assoc 'thread response))))))

(defun org-quip-publish-to-quip ()
  "Publish the current buffer to Quip."
  (interactive)
  (let
      ((quip-id (org-quip--get-thread-identifier))
       (content (org-export-as 'md)))
    (if quip-id
        (org-quip-update-quip quip-id content)
      (let ((new-quip-id (org-quip--publish-quip content)))
        (org-quip--put-thread-identifier new-quip-id)))))

;;

;; Org-to-quip filter:
;;
;; So, Quip HTML is a very specific, strict subset of HTML. Quip has only a
;; few different block types, and it can't do certain things (like
;; multi-paragraph list items.)
;;
;; Structure:
;;  - A list of top-level items:
;;
;;    Headlines: <h1> <h2> <h3>
;;    Block quotes: <blockquote>
;;    Code block: <pre>
;;
;;    <h3>
;;
;;


(provide 'ox-quip)
;;; ox-quip.el ends here
