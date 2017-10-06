;; arc --- Summary
;;; Commentary:
;;; A bunch of helpers for Phabricator and arc
;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'ido)
(require 'image)
(require 'json)
(require 'projectile)
(require 's)

(defconst arc-binary "arc")
(defconst phabricator-macro-dir "~/public_html")
(defconst phabricator-macro-list "~tulloch/.arc_macros")
(defconst phabricator-macro-refresh-script "~tulloch/bin/arc_macros.sh")

(defcustom arc-repo-prefix-list
  '(("fbcode" . "https://phabricator.fb.com/diffusion/FBCODE/browse/master")
    ("www" . "https://phabricator.fb.com/diffusion/E/browse/tfb/trunk/www"))
  "*Mapping from repository name to path in diffusion."
  :group 'arc)

(defun arc--call-conduit (method req)
  "Call conduit METHOD with the parameters REQ."
  (with-temp-buffer
    (let ((tmp-file (make-temp-file "arc-conduit")))
      (with-temp-file tmp-file (->> req json-encode insert))
      (call-process
       arc-binary tmp-file `(,(current-buffer) nil) nil "call-conduit" method)
      (->> (buffer-string) json-read-from-string (assoc 'response) cdr))))

;;; ----------------------------------------------------------------------------
;;; arc macro commands.
(defun arc--list-macros ()
  "Load phabricator-macro-list into an ido completion window."
  (with-temp-buffer
    (insert-file-contents phabricator-macro-list)
    (->> (buffer-string) (s-split "\n") (ido-completing-read "Macro: "))))

(defun arc-refresh-macros ()
  "Refresh phabricator-macro-list from Phabricator with the latest macros."
  (interactive)
  (call-process-shell-command
   phabricator-macro-refresh-script nil nil nil phabricator-macro-list))

(defun arc-insert-macro (name)
  "Insert image NAME from a list of Phabricator macros."
  (interactive  `(,(arc--list-macros)))
  (if (display-graphic-p)
      (let ((img (->> name arc--get-macro create-image)))
        (when (image-animated-p img)
          (image-animate img 0 t))
        (insert-image img name)
        (insert "\n\n"))
    (insert name)))

(defun arc--get-macro (macro-name)
  "Retrieve the given MACRO-NAME image and save it to *phabricator-macro-dir*."
  (interactive  `(,(arc--list-macros)))
  (let* ((macro-file-name (format "%s/%s" phabricator-macro-dir macro-name))
         (download-uri
          (lambda (uri)
            (when (not (file-exists-p macro-file-name))
              (url-copy-file uri macro-file-name)
              (shell-command (format "chmod 644 %s" macro-file-name)))
            macro-file-name))
         (extract-uri
          (lambda (output)
            (->> output (assoc (intern macro-name)) cdr (assoc 'uri) cdr))))
    (->> (arc--call-conduit "macro.query" `(:names (,macro-name)))
      (funcall extract-uri) (funcall download-uri) message)))


;;; ----------------------------------------------------------------------------
;;; arc paste commands.
(defun arc-paste (start end)
  "Pastes the specified region from START to END (or whole file) with arcanist.
The resulting URL is stored in the kill
ring and messaged in the minibuffer."
  (interactive (if (use-region-p) `(,(region-beginning) ,(region-end))
                 `(,(point-min) ,(point-max))))
  (let* ((extract-uri (lambda (output) (->> output (assoc 'uri) cdr))))
    (->> (arc--call-conduit "paste.create"
          `(:title ,(when (buffer-file-name) (file-name-nondirectory (buffer-file-name)))
            :content ,(buffer-substring start end)))
      (funcall extract-uri) message kill-new)))


;;; ----------------------------------------------------------------------------
;;; arc inlines commands
(defun arc-inlines ()
  "Display the inlines for the current branch in a compilation buffer."
  (interactive)
  (let ((previous-dir default-directory))
    (unwind-protect
        (save-excursion
          (cd (projectile-project-root))
          (compile (format "%s inlines" arc-binary)))
      (cd previous-dir))))

;;; ----------------------------------------------------------------------------
;;; arc browse commands
(defun arc--repo-prefix ()
  "So fbcode2, fbcode_contbuild dirs should still map to fbcode."
  (->> (-first
        (lambda (p) (s-starts-with? (car p) (projectile-project-name)))
        arc-repo-prefix-list)
    cdr))

(defun arc-browse (start end)
  "Paste the specified region from START to END (or current line).
The resulting URL is stored in the kill ring and messaged in the
minibuffer."
  (interactive (if (use-region-p)
                   (mapc 'line-number-at-pos
                         `(,(region-beginning) ,(region-end)))
                 `(,(line-number-at-pos) ,(line-number-at-pos))))
  (when (not (arc--repo-prefix))
    (error "Not in a known Diffusion repository"))
  (let* ((url (format "%s/%s$%s-%s"
                      (arc--repo-prefix)
                      (file-relative-name
                       (file-truename buffer-file-name)
                       (file-truename (projectile-project-root)))
                      start end)))
    (->> url message kill-new)))


;;; ----------------------------------------------------------------------------
;;; Shortcuts
(global-set-key (kbd "C-c C-a i") 'arc-inlines)
(global-set-key (kbd "C-c C-a b") 'arc-browse)
(global-set-key (kbd "C-c C-a p") 'arc-paste)
(global-set-key (kbd "C-c C-a m") 'arc-insert-macro)

(provide '50-arc)
;;; 50-arc.el ends here
