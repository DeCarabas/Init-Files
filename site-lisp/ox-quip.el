;;; ox-quip.el -- Publish from org-mode to Quip. -*- lexical-binding: t -*-

;;; Commentary:
;; Publisher from org-mode to Quip.  (Export as HTML, push as a new
;; thread or amend to existing quip thread.)
;;
;; BUG: QUOTE is not rendered correctly.
;; TODO: Convert the quip ID to a full-on URL

;;; Code:
(require 'cl-extra)
(require 'org-id)
(require 'ox-html)
(require 'url-parse)
(require 'whitespace)
(require 'quip)

;; ===================================
;; Functions to do with cleaning DOMs.
;; ===================================
(defun org-quip--remove-element-p (elem)
  "Return t if ELEM should be removed from a dom.

This basically strips out all-whitespace text, so that we can
compare properly."
  (and (stringp elem)
       (equal 0 (length (string-trim elem)))))

(defun org-quip--clean-string (string tag)
  "Clean up the text in the STRING contained in TAG, to match good text."
  (let ((string string))
    (setq string (replace-regexp-in-string (char-to-string 160) " " string))
    (setq string (replace-regexp-in-string (char-to-string #x200B) "" string))
    (unless (eq 'pre tag)
      (setq string (replace-regexp-in-string "[[:space:]]+" " " string))
      (setq string (string-trim string)))
    (setq string (replace-regexp-in-string "\n" "\\\\n" string))
    string))

(defun org-quip--clean-element-strings (contents tag)
  "Clean up the string CONTENTS of DOM element with tag TAG.

This is structured as a separate pass from
org-quip--clean-element-contents because we want to make sure we
get all the strings, nested or not, and that includes inside
<spans> and the newlines from <br />."
  (let ((new-contents nil))
    (while contents
      (cond
       ((stringp (car contents))

        (if (stringp (cadr contents))
            (setq contents (cons (concat (car contents) (cadr contents))
                                 (cddr contents)))
          (setq new-contents (cons (org-quip--clean-string (car contents) tag)
                                   new-contents))
          (setq contents (cdr contents))))

       (t
        (letrec ((this (car contents))
                 (new-tag (car this))
                 (new-attrs (cadr this))
                 (new-body (org-quip--clean-element-strings (cddr this) new-tag))
                 (new-elem (append (list new-tag new-attrs) new-body)))
          (setq new-contents (cons new-elem new-contents))
          (setq contents (cdr contents))))))

    (nreverse new-contents)))

(defun org-quip--clean-element-contents (contents tag strip)
  "Clean up the CONTENTS of element with tag TAG, optionally STRIP IDs.

We extract and flatten children from wrapper elements like
<html>, <body>, some <divs>, &c.  The goal is to be able to run
this function on both the DOM returned from quip and the DOM
produced by org HTML export and get something that's roughly
comparable.  We also clear all the attributes except for ID, and
we only keep ID in certain cases where we know we can support the
correspondence."
  (let ((new-contents nil))
    (while contents
      (cond
       ;; Strings are strings.
       ((stringp (car contents))
        (setq new-contents (cons (car contents) new-contents))
        (setq contents (cdr contents)))

       ;; <br /> is "\n".
       ((eq (caar contents) 'br)
        (setq new-contents (cons "\n" new-contents))
        (setq contents (cdr contents)))

       ;; This might be an element we're unfolding...
       ((or (member (caar contents) '(div html body span))
            (and (eq (caar contents) 'a)
                 (cdar contents)
                 (not (alist-get 'href (cdar contents)))))
        (setq contents (append (cddar contents) (cdr contents))))

       ;; This is an element we're keeping.
       (t
        (letrec ((el-keep (car contents))
                 (new-tag (car el-keep))
                 (new-id (org-quip--get-element-id el-keep))
                 (attribs (when (or (not strip)
                                    (member new-tag '(h1 h2 h3 h4 h5 h6)))
                            `((id . ,new-id))))
                 (body (org-quip--clean-element-contents (cddr el-keep)
                                                         new-tag
                                                         strip))
                 (new-elem (append (list new-tag attribs) body)))

          (setq new-contents (cons new-elem new-contents))
          (setq contents (cdr contents))))))

    (nreverse new-contents)))

(defun org-quip--parse-html (html)
  "Just parse some HTML.

Emacs HTML parsing only works on a buffer which is bizarre, so this little
wrapper makes it work on a string."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun org-quip--get-cleaned-dom (html &optional strip)
  "Clean HTML as a list of cleaned DOM elements, and maybe STRIP attributes.

The return value is a list of elements, each one having been
scrubbed appropriately.  This function can be called on both the
results of the quip exporter and the HTML we get back from quip
to produce comparable results."
  (letrec ((parsed-html (org-quip--parse-html html))
           (phase-one (org-quip--clean-element-contents (list parsed-html)
                                                        nil
                                                        strip))
           (phase-two (org-quip--clean-element-strings phase-one nil)))
    (cl-remove-if #'stringp phase-two)))

;; ==============================================
;; Functions to do with mapping HTML and ORG IDs.
;; ==============================================
(defun org-quip--ensure-ids (buffer)
  "Ensure that each headline in BUFFER has a valid ID."
  (with-current-buffer buffer
    (org-map-entries
     (lambda ()
       (unless (org-entry-get nil "CUSTOM_ID")
         (let ((id (org-id-new)))
           (org-entry-put nil "CUSTOM_ID" id)
           (org-id-add-location id (buffer-file-name (buffer-base-buffer)))))))
    ))

(defun org-quip--get-element-id (element)
  "Return the ID attribute of the specified ELEMENT."
  (let ((elt-attrs (second element)))
    (if elt-attrs
        (alist-get 'id elt-attrs))))

(defun org-quip--make-id-alist (org-html quip-html)
  "Create a alist mapping IDs in ORG-HTML to IDs in QUIP-HTML.

This works by getting a clean DOM from both and mapping them
together.  We assume that the HTML is compatible, i.e., that you
just published this.  Otherwise the correspondance will be
wrong."
  (letrec ((org-dom (org-quip--get-cleaned-dom org-html))
           (quip-dom (org-quip--get-cleaned-dom quip-html)))

    (unless (equal (length org-dom) (length quip-dom))
      (error "Org DOM and Quip DOM did not clean the same way"))

    (letrec ((make-id-cons
              (lambda (org-elt quip-elt)
                (let ((org-id (org-quip--get-element-id org-elt))
                      (quip-id (org-quip--get-element-id quip-elt)))
                  (if (and quip-id org-id)
                      (cons (intern org-id) quip-id)))))

             (pairs (mapcar* make-id-cons org-dom quip-dom)))

      (cl-remove-if #'null pairs))))

(defun org-quip--remap-entry-id (pom id-map)
  "Remap the CUSTOM_ID for the entry at POM to the corresponding entry in ID-MAP."
  (letrec ((org-id-str (org-entry-get pom "CUSTOM_ID"))
           (org-id (and org-id-str (intern org-id-str)))
           (quip-id (and org-id (alist-get org-id id-map))))
    (when quip-id
      (org-entry-put pom "CUSTOM_ID" quip-id)
      (org-id-add-location quip-id (buffer-file-name (buffer-base-buffer))))))

(defun org-quip--sync-ids-with-quip (org-buffer org-html quip-html)
  "Sync IDs in ORG-BUFFER by mapping ORG-HTML with those in QUIP-HTML.

ORG-HTML should previously have been produced with
org-quip--export-html and QUIP-HTML should be the result we get
back from Quip.  They should be as in-sync as possible-- ideally
you will have just published ORG-HTML to Quip and retrieved
QUIP-HTML with no intervening changes."

  (letrec ((id-map (org-quip--make-id-alist org-html quip-html)))
    (with-current-buffer org-buffer
      (org-map-entries
       (lambda () (org-quip--remap-entry-id (point) id-map))))))

(ert-deftest org-quip-test-stuff ()


  )

;; ==========================================

(defun org-quip--cleanup-quip-html ()
  "Remove things that convert badly."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<div data-section-style='[0-9]+'>" nil t)
      (replace-match "")
      (if (re-search-forward "</div>" nil t) (replace-match "")))

    (goto-char (point-min))
    (while (re-search-forward "<span id='[A-Za-z0-9]+'>" nil t)
      (replace-match "")
      (if (re-search-forward "</span>" nil t) (replace-match "")))

    (goto-char (point-min))
    (while (re-search-forward "<li id='[A-Za-z0-9]+'" nil t)
      (replace-match "<li"))
    ))

(defun org-quip--cleanup-org-buffer ()
  "Run a whole bunch of cleanup on the pandoc-generated org buffer."
  (save-excursion
    ;; Remove the dumb non-breaking spaces.
    (goto-char (point-min))
    (while (re-search-forward (char-to-string 160) nil t)
      (replace-match " "))

    ;; Remove these things that pandoc puts in.
    (goto-char (point-min))
    (while (re-search-forward "\\\\" nil t)
      (replace-match ""))

    ;; Remove zero-width spaces. (Why are they even there?)
    (goto-char (point-min))
    (while (re-search-forward (char-to-string #x200B) nil t)
      (replace-match ""))

    (when org-odd-levels-only
      ;; Move all the sub-headlines one over (because the normal demote
      ;; commands move by two stars not just one). This code is taken from
      ;; org-convert-to-odd-levels, with the interactive yes/no removed.
      ;;
      (let ((outline-level 'org-outline-level)
            (org-odd-levels-only nil) n)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\*\\*+ " nil t)
            (setq n (- (length (match-string 0)) 2))
            (while (>= (setq n (1- n)) 0)
              (org-demote))
            (end-of-line 1)))))

    ;; Indent before whitespace cleanup so that blank lines are still blank.
    ;; (If you indent after whitespace cleanup, indentation adds whitespace
    ;; to align with the paragraph level.)
    ;;
    (indent-region (point-min) (point-max))

    ;; Indent lists to match their enclosing headlines. These don't get
    ;; auto-indented by the following indentation code, but must be done
    ;; after the indentation pass to align with preceeding paragraphs.
    (goto-char (point-min))
    (while (re-search-forward "^[-+] " nil t)
      (move-beginning-of-line nil)
      (while (<= (1+ (current-indentation)) (org-outline-level))
          (org-indent-item-tree)))

    ;; (Indent again; to get all the leftover paragraphs.)
    (indent-region (point-min) (point-max))
    (whitespace-cleanup)

    ;; Fill all the paragraphs, but only the paragraphs. (Filling code blocks
    ;; and tables does bizarre things, and filling headlines breaks them.)
    (org-element-map (org-element-parse-buffer) 'paragraph
      (lambda (paragraph)
        (goto-char (org-element-property :begin paragraph))
        (fill-paragraph nil)
        ))

    ;; Remove consecutive white lines.
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
      (replace-match "\n")
      (forward-char 1))

    ;; Tidy up all the property drawers.
    (org-element-map (org-element-parse-buffer) 'property-drawer
      (lambda (drawer)
        (goto-char (org-element-property :begin drawer))
        (org-cycle)
        ))
    ))

(defun org-quip--get-org-buffer-from-quip-thread (thread buffer)
  "Take the Quip thread THREAD and convert it to org markup in BUFFER.

THREAD should be an alist form of the JSON returned by the
'quip-get-thread' function.

We do this by (a) cleaning up the HTML in the buffer (a little
bit), (b) running the HTML through pandoc to convert it into
acceptable org, and (c) running a cleanup pass over the generated
org markup.

The end result is fairly clean 'org-mode' markup."
  (let ((quip-html (alist-get 'html thread)))
    (with-current-buffer buffer
      (erase-buffer)
      (with-temp-buffer
        (insert quip-html)
        (org-quip--cleanup-quip-html)
        (call-process-region (point-min) (point-max)
                             "pandoc" nil buffer nil
                             "-f" "html"
                             "-t" "org"
                             "--normalize"
                             "--smart"
                             ))
      (goto-char (point-min))
      (org-mode)
      (org-show-subtree)
      (org-quip--cleanup-org-buffer)

      ;; HAX
      (goto-char (point-min))
      (org-entry-put nil "QUIP_ID" (alist-get 'id (alist-get 'thread thread)))

      (goto-char (point-min))
      (insert "#+options: num:nil\n\n")

      (font-lock-fontify-buffer)
      )))

(defun org-quip--get-org-buffer-from-quip-id (thread-id buffer)
  "Fetch the quip thread with THREAD-ID and convert it to org markup in BUFFER.

This is just like 'org-quip--get-org-buffer-from-quip-thread'
except you only have a thread ID, not a full downloaded thread."
  (org-quip--get-org-buffer-from-quip-thread (quip-get-thread thread-id) buffer))

(defun org-quip--get-thread-identifier ()
  "Get the Quip thread identifier from the doc in the current buffer, if any."
  (org-entry-get nil "QUIP_ID" t))

(defun org-quip--put-thread-identifier (identifier)
  "Put the Quip thread identifier in IDENTIFIER into the doc."
  (save-excursion
    (while (org-up-heading-safe))
    (org-entry-put nil "QUIP_ID" identifier)))

(defun org-quip--publish-quip (content)
  "Publish CONTENT as a new Quip document.

Returns the published thread structure."
  (let ((response (quip-new-document content "html")))
    response))

(defun org-quip--extract-thread-id (url)
  "Extract a quip thread identifier from URL."
  (let ((parsed-url (url-generic-parse-url url)))

    (if (and (equal (url-type parsed-url) "https")
             (string-suffix-p "quip.com" (url-host parsed-url)))
        (substring (car (split-string (url-filename parsed-url) "?")) 1)

      (error "%s does not appear to be a valid Quip url" url))))

(defun org-quip--export-html (buffer)
  "Export 'org-mode' BUFFER as HTML with options suitable for quip."
  (with-current-buffer buffer
    (let ((org-html-toplevel-hlevel 1))
      (org-export-as 'html nil nil t))))

(defun org-quip--export-html-fragment (buffer start end)
  "Export org BUFFER as HTML from START to END."
  (save-excursion
   (with-current-buffer buffer
     (save-restriction
       (narrow-to-region start end)
       (let ((org-html-toplevel-hlevel 1))
         (org-export-as 'html nil nil t))))))

(defun org-quip-publish-to-quip ()
  "Publish the current buffer to Quip."
  (interactive)
  (org-quip--ensure-ids (current-buffer))
  (let
      ((quip-id (org-quip--get-thread-identifier))
       (content (org-quip--export-html (current-buffer))))
    (if quip-id
        (org-quip--update-quip (current-buffer) content quip-id)
      (letrec ((quip-thread (org-quip--publish-quip content))
               (new-quip-id (alist-get 'id (alist-get 'thread quip-thread)))
               (quip-html (alist-get 'html quip-thread)))
        (org-quip--put-thread-identifier new-quip-id)
        (org-quip--sync-ids-with-quip (current-buffer) content quip-html)))))

(defun org-quip-refresh ()
  "Refresh the current document from quip.

This replaces what's in the buffer so I hope you're OK with that."
  (interactive)
  (let ((thread-id (org-quip--get-thread-identifier)))
    (unless thread-id (error "This org doc hasn't been published to quip"))
    (org-quip--get-org-buffer-from-quip-id thread-id (current-buffer))))

(defun org-quip-fetch (url)
  "Fetch the given Quip URL into a new buffer named after the thread."
  (interactive "sQuip URL: ")
  (letrec ((thread-id (org-quip--extract-thread-id url))
           (thread (quip-get-thread thread-id))
           (thread-title (alist-get 'title (alist-get 'thread thread)))
           (thread-buffer (get-buffer-create (concat thread-title ".org"))))

    (org-quip--get-org-buffer-from-quip-thread thread thread-buffer)
    (display-buffer thread-buffer)))

;;; Update:
(defun org-quip--get-buffer-thread-id (buffer)
  "Get the Quip thread ID from BUFFER."
  (with-current-buffer buffer
    (save-excursion

      (goto-char (point-min))
      (search-forward ":PROPERTIES:")
      (next-line)
      (org-quip--get-thread-identifier)

      )))

(defun org-quip--get-cleaned-html-string (html)
  "Clean HTML as a nice string for diffing."
  (with-temp-buffer
    (mapcar
     (lambda (elem)
       (insert (or (org-quip--get-element-id elem) "nil"))
       (insert " ")
       (prin1 elem (current-buffer))
       (insert "\n"))
     (org-quip--get-cleaned-dom html t))
    (buffer-string)))

(defun org-quip--find-with-custom-id (custom-id)
  "Find the entry with the specified CUSTOM-ID and return the point."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^[ \t]*:CUSTOM_ID:[ \t]+"
                                     (regexp-quote custom-id)
                                     "[ \t]*$")
                             nil t)
      (org-back-to-heading t)
      (point))))

(defun org-quip--get-element-end (element)
  "Get the end point of 'org-mode' ELEMENT.

This is the end as we care about it: basically the end of the
main part of the element, not the end of the entire element and
its children.  We also don't care about things like a headline's
property-drawer, so we skip that too.

\(Basically if you export from from the start of the element to
the value returned from this you get just the HTML for that
element.)"
  (let ((type (org-element-type element))
        (next-element))
    (cond ((eq type 'headline)
           (save-excursion
             (goto-char (1+ (org-element-property :contents-begin element)))
             (setq next-element (org-element-at-point))
             (if (eq (org-element-type next-element) 'property-drawer)
                 (org-element-property :end next-element)
               (org-element-property :contents-begin element))))

          (t
           (org-element-property :end element))
          )))

(defun org-quip--get-positions-from-ids (buffer org-id-list)
  "With the specified BUFFER and ORG-ID-LIST, compute a list of (start, end) pairs."
  (save-excursion
    (with-current-buffer buffer
      (let ((id-list org-id-list)
            (pos-list nil))
        (while id-list
          ;; Go to either the start of the element in the ID list, if we have
          ;; an ID, or to the end of the previous element if we
          ;; don't. (Assuming that this is how it goes.)
          (goto-char (if (car id-list)
                         (org-quip--find-with-custom-id (car id-list))
                       (cdar pos-list)))

          ;; Examine the element at point and push the start and end position
          ;; of the element.
          (letrec ((element (org-element-at-point))
                   (begin (org-element-property :begin element))
                   (end (org-quip--get-element-end element)))

            ;; (message "SCANNING: %s %s %s %s %s"
            ;;          (car id-list)
            ;;          (point)
            ;;          begin
            ;;          end
            ;;          (org-element-type element))
            (setq pos-list (cons (cons begin end) pos-list)))

          (setq id-list (cdr id-list)))

        ;; We were pushing onto pos-list so we gotta reverse it.
        (nreverse pos-list)))))

(defun org-quip--diff-org-and-quip (quip-html org-html)
  "Generate a diff between the QUIP-HTML string and ORG-HTML string.

The strings are 'cleaned' and formatted a little before they are diffed.
Further processing will parse this diff text and turn it into a series of
commands."
    (let ((old-diff-text (org-quip--get-cleaned-html-string quip-html))
          (new-diff-text (org-quip--get-cleaned-html-string org-html))
          (old-diff-file (make-temp-file "qod"))
          (new-diff-file (make-temp-file "qnd")))

      (unwind-protect
          (progn
            (with-temp-file old-diff-file (insert old-diff-text))
            (with-temp-file new-diff-file (insert new-diff-text))

            (with-temp-buffer
              (call-process "diff"
                            nil (current-buffer) nil
                            old-diff-file new-diff-file
                            "-d" "-U" "1")

              (buffer-string)))

        (ignore-errors (delete-file old-diff-file))
        (ignore-errors (delete-file new-diff-file)))))

(defun org-quip--make-publish-diff (org-buffer new-html quip-html)
  "Generate a diff between the ORG-BUFFER with NEW-HTML and QUIP-HTML.

The return value is a list of commands to execute against Quip."
  ;;
  ;; In order to make this work we need to generate a correspondence between
  ;; the quip document and the org buffer. Quip gives us back a wodge of
  ;; HTML, so what we do is we generate a correspondence between org's HTML
  ;; and quip's HTML.
  ;;
  ;; That works more or less as follows:
  ;;
  ;; 1. Clean up the quip HTML into a list of "top-level" elements. Each
  ;;    element is on its own "line".
  ;;
  ;; 2. Extract a list of element IDs from the quip HTML, one per line.
  ;;
  ;; 3. Generate HTML from org and do the same thing.
  ;;
  ;; 4. Extract a list of element IDs from the org HTML, again one per line.
  ;;
  ;; 5. Extract a list of element "positions" from the org buffer, one per
  ;;    line. We do this by walking the org tree in a manner that's produces
  ;;    equivalent results to the HTML walk. This is the sketchiest part of
  ;;    the whole deal, and we do it because we can't reliably round-trip
  ;;    HTML through Emacs' DOM. (That is, we can't just use the DOM we
  ;;    extracted IDs from in step 4 to generate HTML to push to Quip because
  ;;    the parser escapes things in ways I can't re-escape.)
  ;;
  ;;  6. Diff the quip HTML and org HTML.
  ;;
  ;;  7. Parse the diff, tracking "lines" and using the ID and position lists
  ;;     we built up to generate commands.
  ;;
  (letrec ((old-dom  (org-quip--get-cleaned-dom quip-html))
           (old-id-list (mapcar #'org-quip--get-element-id old-dom))

           (new-dom  (org-quip--get-cleaned-dom new-html))
           (new-id-list (mapcar #'org-quip--get-element-id new-dom))
           (new-pos-list (org-quip--get-positions-from-ids org-buffer
                                                           new-id-list))

           (diff-text (org-quip--diff-org-and-quip quip-html new-html))

           (last-id nil)
           (remove-ids nil)
           (insert-commands nil)
           (old-line 0)
           (new-line 0))

    (with-current-buffer (get-buffer-create "*Debug Diff*")
      (erase-buffer)
      (insert diff-text)
      (diff-mode))

    (with-temp-buffer
      (insert diff-text)
      (goto-char (point-min))
      (forward-line 2)
      (while (< (point) (point-max))
        (cond
         ;; Processing a resync line, e.g. `@@ -4,4 +4,4 @@'
         ;;
         ;; Just reset both line counters. Remember that diff line numbers
         ;; are 1-based but since we use the counters as list indices they
         ;; need to be 0-based.
         ((looking-at "@@ -\\([0-9]+\\),[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@")
          (setq old-line (1- (string-to-int (match-string 1))))
          (setq new-line (1- (string-to-int (match-string 2))))
          (message "Resync: %s %s" old-line new-line)
          )

         ;; Processing a remove.
         ;;
         ;; Remember all the removes come, then all the adds. We don't know
         ;; if a remove is actually a remove or a replace yet so just batch
         ;; up the removes.
         ;;
         ;; Also move the "old" line counter forward.
         ((looking-at "-")
          (let ((old-id (nth old-line old-id-list)))
            (setq remove-ids (cons old-id remove-ids))
            (setq last-id old-id)
            (setq old-line (1+ old-line))
            (message "Old   : %s" old-id)
            ))

         ;; Processing an add.
         ;;
         ;; Adds might actually be "replace", so check to see if the ID we're
         ;; adding is in the set of ID's we're removing. If it is, remove it,
         ;; and the command is a "replace". If it isn't, then it's just an
         ;; "insert-after".
         ;;
         ;; Also move the "new" line counter forward.
         ((looking-at "\\+")
          (letrec ((new-id (nth new-line new-id-list))
                   (new-pos (nth new-line new-pos-list))
                   (new-html (org-quip--export-html-fragment org-buffer
                                                             (car new-pos)
                                                             (cdr new-pos))))
            (setq insert-commands
                  (cons (cond ((member new-id remove-ids)
                               (setq remove-ids (remove new-id remove-ids))
                               (list 'replace new-id new-html))

                              ((null last-id)
                               (list 'prepend new-html))

                              (t
                               (list 'insert-after last-id new-html)))
                        insert-commands))

            (setq new-line (1+ new-line))
            (message "New   : %s" new-id)
            ))

         ;; Processing a shared line.
         ;;
         ;; Just tick both line counters forward. Also remember, we know the
         ;; old ID for this line.
         ((looking-at " ")
          (setq last-id (nth old-line old-id-list))
          (setq new-line (1+ new-line))
          (setq old-line (1+ old-line))

          (message "Common: %s" last-id)
          )
         )

        (message "Tick  : %s %s %s %s" (point) last-id old-line new-line)
        (forward-line))

      ;; Convert all of our accumulated adds and removes into actual commands
      ;; to run.
      (append insert-commands
              (mapcar (lambda (id) (list 'remove id)) remove-ids))
      )))

(defun org-quip--update-quip (buffer new-html thread-id)
  "Update quip from 'org-mode' BUFFER with NEW-HTML and existing THREAD-ID."
  (save-excursion
    (org-quip--ensure-ids buffer)
    (letrec ((old-html (alist-get 'html (quip-get-thread thread-id)))
             (diff-commands (org-quip--make-publish-diff buffer
                                                         new-html
                                                         old-html)))

      (message "COMMANDS: %s" diff-commands)

      ;; ;; Invoke all of the diff commands on quip.
      ;; (mapc (lambda (command)
      ;;         (cond ((eq 'remove (car command))
      ;;                (quip-thread-delete-section thread-id (second command)))

      ;;               ((eq 'replace (car command))
      ;;                (quip-thread-replace-section thread-id
      ;;                                             (second command)
      ;;                                             (third command)
      ;;                                             "html"))

      ;;               ((eq 'insert-after (car command))
      ;;                (quip-thread-append-after thread-id
      ;;                                          (second command)
      ;;                                          (third command)
      ;;                                          "html"))

      ;;               ((eq 'prepend (car command))
      ;;                (quip-thread-prepend thread-id (second command) "html"))
      ;;               ))
      ;;       diff-commands)

      ;; Re-fetch the HTML from quip and sync IDs.
      ;; (setq old-html (alist-get 'html (quip-get-thread thread-id)))
      ;; (org-quip--sync-ids-with-quip buffer new-html old-html)
      )))

(defun test-what ()
  "A test, what."
  (letrec ((thread-id "GPPAAAud6tF")
           (thread (quip-get-thread thread-id))
           (quip-html (alist-get 'html thread))
           )

    (org-quip--get-cleaned-dom quip-html)

    )

  )

(provide 'ox-quip)
;;; ox-quip.el ends here
