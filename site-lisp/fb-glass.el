;;; fb-glass.el --- Emacs integration for the Glass CLI  -*- lexical-binding: t; -*-

;; Keywords: tools,processes
;; Package-Requires: ((emacs "27.1") (f "1.0"))
;; Version: 0.1

;;; Commentary:
;; Integrates `glass` into Emacs. Improvements welcome!
;; Note that Glass is designed more for static/"offline" navigation of code, without needing
;; a full LSP to be running.
;; This integration will return stale results if you try to use it on files that have been modified.

;;; Code:
(require 'compile)
(require 'f)

(defun glass--call-process-shell-check-success (command)
  (with-temp-buffer
    (let ((exit-code (call-process-shell-command command nil (current-buffer))))
      (unless (equal exit-code 0)
        (error "Error running shell command"))
      (buffer-string))))

(defun glass--root (filename)
  "Gets the root corresponding to the given filename. If none given, the current buffer's filename is used."
  (let ((default-directory (file-name-directory (file-truename filename))))
    (string-trim (glass--call-process-shell-check-success "hg root"))))

(defun glass--repo (filename)
  "Gets the Glean repo ID corresponding to the given filename. If none given, the current buffer's filename is used."
  (let ((default-directory (file-name-directory (file-truename filename))))
    (string-trim (glass--call-process-shell-check-success "hg config remotefilelog.reponame"))))

;;; VISIT SYMBOLS

(defun glass-visit-symbol (symbol)
  "Given a Glass symbol, visit the file that defines that symbol"
  (interactive)
  (let* ((output (glass--call-process-shell-check-success
                  (format "glass --caller=emacs describe -t %s" (shell-quote-argument symbol))))
         (position (split-string (car (split-string output)) ":"))
         (filename (car position))
         (local-filename (concat "~/" filename)))
    (if (file-exists-p local-filename)
        (progn
          (find-file local-filename)
          (goto-char (point-min))
          (forward-line (- (string-to-number (cadr position)) 1))
          (forward-char (- (string-to-number (caddr position)) 1)))
      (let* ((buffer (generate-new-buffer (concat "glass--" (file-name-nondirectory filename))))
             (repo (car (split-string filename "/")))
             (exit-code (call-process "scsc" nil buffer nil
                          "cat"
                          "--repo" (shell-quote-argument repo)
                          "--path" (shell-quote-argument (string-remove-prefix repo filename))
                          "-B" "master")))
        (if (equal exit-code 0)
            (progn
              (with-current-buffer buffer
                (setq-local buffer-read-only t)
                (goto-char (point-min))
                (forward-line (- (string-to-number (cadr position)) 1))
                (forward-char (- (string-to-number (caddr position)) 1)))
              (pop-to-buffer buffer))
          (progn
            (kill-buffer buffer)
            (error "Couldn't display source with `scsc`")))))))

;;; LIST SYMBOLS

(defun glass-list-symbols (&optional filename)
  "Lists all symbols within the given filesystem path. If none given, the current buffer's filename is used."
  (let* ((file (file-truename (or filename (buffer-file-name))))
         (root (glass--root file))
         (repo (glass--repo file))
         (search-path (concat repo "/" (file-relative-name file root))))
    ;; TODO integrate with myles, myles to get a path -> glass to list its symbols
    (compilation-start
     (format "glass --caller emacs list-symbols %s | sed -e 's|^|~/|'" search-path))))

;;; SYMBOL-SEARCH

(with-eval-after-load "counsel"
  (defun glass--symbol-search (input)
    (counsel--async-command
     (format "glass --caller=emacs symbol-search %s"
             (shell-quote-argument input)))
    nil)

  (defun glass-symbol-search-counsel ()
    "Find a symbol by full symbol ID prefix using `glass symbol-search`"
    (interactive)
    (counsel-require-program "glass")
    (let ((default-directory (myles--root)))
      ;; TODO allow entering "/" to grab the current candidate and restart from there
      ;; similar to how it works for counsel-find-file
      (ivy-read
       "symbol prefix: "
       #'glass--symbol-search
       :caller 'glass-counsel
       :dynamic-collection t
       :action #'glass-visit-symbol))))

(provide 'fb-glass)
;;; fb-glass.el ends here
