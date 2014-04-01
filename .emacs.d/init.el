;;; init.el --- Where all the magic begins
;;
;; This was cribbed from the Emacs Starter Kit, but is special for doty.
;;
;; This is the first thing to get loaded.
;;

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
          `(lambda ()
             ;; remember this directory
             (setq init-dir
                   ,(file-name-directory (or load-file-name (buffer-file-name))))
             ;; load up the starter kit
             (load-file (expand-file-name "core.el" init-dir))))

;;; init.el ends here
