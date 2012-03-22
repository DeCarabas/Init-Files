;;; ci.el - query the content indexer from within XEmacs/FSF Emacs
;;; Rob Earhart <earhart@microsoft.com>
;;; John Doty <johndoty@microsoft.com>  (much modification)

;;; Note: I use
;;;
;;; (setq ci-machine nil
;;;       ci-catalog "NT"
;;;       ci-scope "\\")
;;; (define-key global-map "\C-c\C-l" 'ci)
;;;
;;; ... but those're just my preference; the defaults are the ci defaults,
;;; and there's no default key binding.

;;; To use index1, use something like
;;; (setq ci-machine index1
;;;       ci-catalog "sources"
;;;       ci-scope "\\")

;;; History:
;;;   5/1/2002 johndoty Changes to work with standard emacs
;;;                     Use other frames, if desired.
;;;                     Use smart mode of ci.exe if desired.
;;;                     Lots of other goo.
;;;  1/27/2000 earhart  Capitalize drive letter
;;;   9/8/1999 earhart  Jumping on single completion
;;;  8/16/1999 earhart  Made o go back to main buffer
;;;  8/12/1999 earhart  Added ci-lookup
;;;   8/3/1999 earhart  Added ci-goto-file-kill-ci
;;;  7/26/1999 earhart  Added C-k => copy from point to EOL
;;;                     Cleaned up help extent
;;;                     Added current selection extent
;;;  7/23/1999 earhart  button2 => goto-file-other-window
;;;                     Added (provide 'ci)
;;;                     Started adding extent support
;;;                     Fixed button1 handling
;;;                     Removed dependancy on func-menu ('tho it's still used if available)
;;;                     Added ci-goto-file-other-window, mapping, impl
;;;  7/22/1999 1.0 released

(defvar ci-command "ci"
  "The command to run ci")

(defvar ci-machine nil
  "The machine whose content-indexer we should talk to")

(defvar ci-catalog nil
  "The content-indexer catalog to access")

(defvar ci-scope nil
  "The scope to search")

(defvar ci-history nil)

(defvar ci-add-files-to-file-history 't
  "Whether we should add files selected in content-indexer buffers to the global
 file-name-history used when opening files")

(defvar ci-results-in-frames 't
  "Whether or not the results are displayed in another frame.")

(defvar ci-allow-frame-reuse 't
  "Whether or not the results are displayed in the same frame every time.  This
only means something if ci-results-in-frames is not nil.")

(defvar ci-open-switches-frames nil
  "Whether or not opening an file from the results frame raises the target frame.
This only means something if ci-results-in-frames is not nil.")

(defvar ci-define-c++-properties nil
  "Whether or not to define the C++ properties automatically.  Use this if you do
not have a default column file set up for the content indexing service, or if the
column file does not define the C++ properties by default.")

(defvar ci-use-smart-mode nil
  "Whether or not to use the smart mode for ci.exe.  Smart mode uses the current
working directory to pick the catalog from the content indexing service, but does
not limit the search to that scope (it's different from setting ci-scope to '.').
You will need a newer version of ci.exe to let this work, and you must set ci-catalog 
to nil.")

;; Find out what type of emacs we are running in.  We use this for how we
;; handle extents.
(defvar ci-running-emacs nil
  "If the current Emacs is not XEmacs, then this is non-nil.")
(defvar ci-running-xemacs nil
  "If the current Emacs is XEmacs/Lucid, then this is non-nil.")
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (setq ci-running-xemacs t)
  (setq ci-running-emacs t))

(defvar ci-frame-alist
  '((height           . 20)
    (width            . 80)
    (minibuffer       . t)
    (buffer-predicate . ci-frame-buffer-predicate)
    (auto-raise       . t)
    (unsplittable     . t))
  "The alist of properties for a ci results frame: squat, unsplittable, no 
mini-buffer, and a special predicate function.")


(defvar ci-results-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3]        'ci-mouse-goto-file-other-window)
    (define-key map [mouse-1]        'ci-mouse-goto-file)
    (define-key map " "              'scroll-up)
    (define-key map "\^?"            'scroll-down)
    (define-key map "\C-k"           'ci-copy)
    (define-key map "\C-c\C-k"       'ci-kill-query)
    (define-key map "\C-c\C-c"       'ci-goto-file)
    (define-key map "o"              'ci-goto-file-other-window)
    (define-key map [return]         'ci-goto-file)
    (define-key map [(shift return)] 'ci-goto-file-kill-ci)
    (define-key map "q"              'ci-kill-buffer)
    map)
  "Keymap for content-indexer result buffers.")


;;==================================================================
;; Frame manipulation and support.
;;==================================================================
(defun ci-frame-buffer-predicate (buffer)
  "The buffer predicate function for ci frames.  It only allows buffers with 
names that start 'ci:' (like the result buffers from searches, for example)."
  (string= "ci:" (substring (buffer-name buffer) 0 3)))


(defun ci-create-frame ()
  "Create a frame for hosting ci results.  Associates the new frame with the 
frame that created it, so that we can re-use frames if requested."
  (let ((frame (make-frame ci-frame-alist))
        (pframe (selected-frame)))
    ;;
    ;; Stuff new frame into frame parameters so we can grab it later, if we are
    ;; re-using frames.  Also remember the parent frame.
    ;;
    (modify-frame-parameters pframe (list (cons 'ci-results-frame   frame)))
    (modify-frame-parameters frame  (list (cons 'ci-parent-frame    pframe)))
    (modify-frame-parameters frame  (list (cons 'ci-is-result-frame 't)))
    ;;
    ;; And return the new frame.
    ;;
    frame))


(defun ci-get-frame-param (prop frame)
  "Just a quick little function to return the specified frame parameter of the
specified frame"
  (let ((param (assq prop (frame-parameters frame))))
    (if param
        (cdr param)
      nil)))


(defun ci-get-parent-frame ()
  "Get the frame we've marked as this frame's parent, or nil if none."
  (ci-get-frame-param 'ci-parent-frame (selected-frame)))


(defun ci-is-result-frame-p ()
  "See if the current frame is a result frame or not."
  (ci-get-frame-param 'ci-is-result-frame (selected-frame)))


(defun ci-get-or-create-frame ()
  "Either create a new frame for holding ci results, or return the frame already
associated with this one."
  (if ci-allow-frame-reuse
      (let ((frame-prop (ci-get-frame-param 'ci-results-frame (selected-frame))))
        (if (and (framep frame-prop)
                 (frame-live-p frame-prop))
            frame-prop        ;; Return the stashed frame.
          (ci-create-frame))) ;; Stashed frame not viable, create new.
    (ci-create-frame)))       ;; Not reusing frames, create new.
  
  
;;==================================================================
;; Buffer manipulation.
;;==================================================================
(defun ci-init-output-buffer (buf query)
  "Clear the specified output buffer.  This function erases without undo, and makes
sure that the read-only property of the buffer is set when finished."
  (save-excursion
    (set-buffer buf)
    (setq buffer-read-only nil)
    (buffer-disable-undo buf)
    (erase-buffer)
    (set-buffer-modified-p nil)
    (setq buffer-read-only 't)
    (make-local-variable 'ci-local-query)
    (setq ci-local-query query)
    (make-local-variable 'ci-last-search-buffer)
    (setq ci-last-search-buffer nil)
    (use-local-map ci-results-map)))


(defun ci-get-output-buffer (query)
  "Return the output buffer for an invokation of ci (with the specified query).
This function is also responsible for making sure that the buffer is ready for 
feeding (putting it into the right frame, the right window, etc)."
  (let ((target-buffer (get-buffer-create (concat "ci: " query)))) ;; Get the target buffer.    
    (ci-init-output-buffer target-buffer query)                    ;; Init the buffer.       
    (if ci-results-in-frames
        (let ((target-frame  (ci-get-or-create-frame)))            ;; Get the target frame.
          (select-frame target-frame)                              ;; Select the target frame.
          (raise-frame target-frame)                               ;; Make sure that the new frame has focus.
          (set-window-buffer (selected-window) target-buffer))     ;; Put the buffer into the frame

                                                                   ;; No frames...
      (display-buffer target-buffer))                              ;; use std. logic instead.
    target-buffer))

    
;;==================================================================
;; Event handling in the results buffer.
;;==================================================================
(defun ci-get-filename ()
  "Gets the filename from the current position in a results buffer."
  (let ((tf (buffer-substring
             (progn (beginning-of-line) (point))
             (progn (end-of-line) (point)))))
    (if (string-match "^\\([a-z]\\)\\(:.*\\)" tf)
        (concat (upcase (match-string 1 tf))
                (match-string 2 tf))
      tf)))


(defun ci-search-buffer-for-query (query)
  "Search a buffer for the results of a given query.  Uses the currently
selected buffer."
  (let ((querystr (concat
                   (if (string-match "^@\\w+\\W+" query)    ;; Filter out the @foo
                       (substring query (match-end 0))
                     query))))
    (search-forward querystr nil 't)))


(defun ci-switch-window-wisely (buf use-other-window)
  "Choose which window we want do use.  You must choose wisely."
  (progn
    (if ci-results-in-frames
        (progn
          (select-frame (ci-get-parent-frame))
          (if ci-open-switches-frames
              (raise-frame))))
    (if use-other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))))


(defun ci-goto-file-internal (use-other-window)
  "Go visit a file from a ci results buffer."
  (let ((query    (if (boundp 'ci-local-query)        ci-local-query nil))       
        (srcbuf   (if (boundp 'ci-last-search-buffer) ci-last-search-buffer nil))
        (filename (ci-get-filename)))
    (if (string-equal filename "")
        nil
      (let ((tgtbuf (find-file-noselect filename)))
        (if (not (eq srcbuf tgtbuf))
            (setq ci-last-search-buffer tgtbuf))                            ;; Going to different buffer, remember which one
        (ci-switch-window-wisely tgtbuf use-other-window)                   ;; Actually switch buffers
        (if ci-add-files-to-file-history                                    ;; Add filename to history, if desired
            (setq file-name-history
                  (if (boundp 'file-name-history)
                      (if (and (consp file-name-history)
                               (string= (cadr file-name-history) filename))
                          file-name-history
                        (cons filename file-name-history))
                    (list filename))))
        (if query
            (progn
              (if (not (eq srcbuf tgtbuf))                                  ;; Going to different buffer, move to the top of the
                  (beginning-of-buffer))                                    ;; file to search.
              (ci-search-buffer-for-query query)))))))
    

(defun ci-goto-file ()
  (interactive)
  (ci-goto-file-internal nil))


(defun ci-goto-file-kill-ci ()
  (interactive)
  (let ((frame (selected-frame))
        (buf   (current-buffer)))
    (ci-goto-file)
    (if ci-results-in-frames
        (select-frame frame)
      (select-buffer buf))
    (ci-kill-buffer)))


(defun ci-goto-file-other-window ()
  (interactive)
  (let ((buf (current-buffer)))
    (ci-goto-file-internal 't)))


(defun ci-mouse-goto-file (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (ci-goto-file))


(defun ci-mouse-goto-file-other-window (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (ci-goto-file-other-window))


(defun ci-mouse-maybe-goto-file (event count)
  (if (< 1 count)
      (ci-mouse-goto-file event)))


(defun ci-kill-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (if (ci-is-result-frame-p)
        (let ((pframe (ci-get-parent-frame)))                    
          (delete-frame (selected-frame))
          (select-frame pframe)
          (raise-frame)))
    (kill-buffer buf)))


(defun ci-copy ()
  (interactive)
  (save-excursion
    (let ((text (buffer-substring (point)
				  (progn (end-of-line) (point)))))
      (kill-new text)
      (message (concat "Copied: " text)))))


;;==================================================================
;; Result window text manipulations.  
;; (Link creation, process filter, process sentinal, etc.)
;;==================================================================
(defun ci-set-extent-property (start end property value)
  "Sets the property of an 'extent' (convert between XEmacs and Emacs)"
  (cond 
   (ci-running-xemacs
    (set-extent-property (make-extent start end) property value))
   (ci-running-emacs
    (overlay-put (make-overlay start end) property value))))


(defun ci-create-active-link (start end prop-list)
  "Creates an 'active link' in a buffer."
;;(ci-set-extent-property start end 'face 'bold)
  (ci-set-extent-property start end 'mouse-face 'highlight)
  (while prop-list
    (ci-set-extent-property start end (caar prop-list) (cdar prop-list))
    (setq prop-list (cdr prop-list))))


(defun ci-build-links (proc)
  "Build the links in a ci result buffer."
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-line 0)
    (while (not (= (point) (point-max)))
      (let ((sol (progn (beginning-of-line) (point)))
            (eol (progn (end-of-line) (point))))        
        (ci-create-active-link sol eol nil))
      (forward-line))))


(defun ci-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (save-excursion
      (set-buffer (process-buffer proc))
      (let ((moving (= (point) (process-mark proc))))
        (goto-char (process-mark proc))
        (setq buffer-read-only nil)
        (insert string)
        (set-buffer-modified-p nil)
        (setq buffer-read-only 't)
        (set-marker (process-mark proc) (point))
        (if moving 
            (goto-char (process-mark proc)))))))


(defun ci-sentinel (proc event)
  (message (concat "CI query " event))
  (setq ci-ev event)
  (let ((buf (process-buffer proc)))
    (if (string= event "finished\n")
        (ci-build-links proc))))


;;==================================================================
;; Main interactive commands.
;;==================================================================
(defun ci-kill-query ()
  "Kill the current query process."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
	(interrupt-process proc)
      (error "The query process is not running."))))


(defun ci-find-default-str ()
  "Attempt to find something reasonable to search for"
  (save-excursion
    (catch 'unable-to-find
      (let ((sym
             (progn
               ;; if we're between a function and its call, go back to the function
               (if (looking-at "[ \\t\\n\\r]*(")
                   (backward-word 1))
               ;; if we're in the middle of a word, go to its start
               (if (string-match "[a-zA-Z_]" (char-to-string (preceding-char)))
                   (backward-sexp))
               ;; scan backwards for something that looks good
               (while (not (and (looking-at "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \\t\\n\\r]*(")
                                (not (string= (match-string 1) "if"))
                                (not (string= (match-string 1) "while"))
                                (not (string= (match-string 1) "for"))
                                (not (string= (match-string 1) "do"))))
                 ;; attempt to move backwards
                 (condition-case nil
                     (progn 
                       (backward-up-list 1)
                       (while (let ((cont (not (char= (following-char) ?\())))
                                (backward-sexp)
                                cont)
                         nil))
                   ;; if that didn't work, we're doomed.
                   (error (throw 'unable-to-find (current-word)))))
               (match-string 1))))
        (condition-case nil
            (progn
              (backward-up-list 1)
              (concat "@func " sym))
          (error sym))))))


(defun ci-lookup (query)
  (ci ci-machine ci-catalog ci-scope query))


(defun ci-argument-list (machine catalog scope query)
  "Build the arg list for ci.exe, given the specified arguments."
  (append
   (list "/q")                                    ;; quiet: no additional output
   (if ci-define-c++-properties (list "/z")  nil) ;; define c++ properties automatically
   (if ci-use-smart-mode    (list "/smart")  nil) ;; use 'smart' mode
   (if machine (list (concat "/m:" machine)) nil) ;; specify machine
   (if catalog (list (concat "/c:" catalog)) nil) ;; specify catalog
   (if scope   (list (concat "/p:" scope))   nil) ;; specify scope
   (list query)))
;;   (list (concat "\"" query "\""))))              ;; the query

(defun ci (machine catalog scope query)
  "Access the content indexer.  You'll need the ci binary.

For help, contact John Doty <johndoty@microsoft.com>.

Thanks to Rob Earhart <earhart@microsoft.com> for writing the version of
this file that I've hacked to pieces here.

If you want to set this up to understand the C++ properties, you will
either need to use ci-define-c++-properties, or you will need to create
a text file and point the DefaultColumnFile registry key to it.  The 
DefaultColumnFile registry value is a REG_SZ value that lives under
HKLM\\SYSTEM\\CurrentControlSet\\Control\\ContentIndexCommon.  The value
should be the full path to a file that describes the properties, formatted
like:

[Names]
Class(DBTYPE_WSTR)              = 8dee0300-16c2-101b-b121-08002b2ecda9 \"class\"
Func(DBTYPE_WSTR)               = 8dee0300-16c2-101b-b121-08002b2ecda9 \"func\"

(See MSDN's documentation of DefaultColumnFile for more info about setting up
these properties.)

There are additional properties for C#, which can only be defined using this
file, since ci.exe doesn't understand them on its own.

Struct(DBTYPE_WSTR)             = 8dee0300-16c2-101b-b121-08002b2ecda9 \"struct\"
Interface(DBTYPE_WSTR)          = 8dee0300-16c2-101b-b121-08002b2ecda9 \"interface\"
Delegate(DBTYPE_WSTR)           = 8dee0300-16c2-101b-b121-08002b2ecda9 \"delegate\"
Property(DBTYPE_WSTR)           = 8dee0300-16c2-101b-b121-08002b2ecda9 \"property\"
Enum(DBTYPE_WSTR)               = 8dee0300-16c2-101b-b121-08002b2ecda9 \"enum\"
Def(DBTYPE_WSTR)                = 8dee0300-16c2-101b-b121-08002b2ecda9 \"def\"

"
  (interactive
   (let* ((default-res (current-word)) ;;(ci-find-default-str))
          (user-query (read-string "Search for: " default-res 'ci-history)))
     (list ci-machine ci-catalog ci-scope user-query)))

  (ci-internal machine catalog scope query))

(defun ci-def (machine catalog scope query)
  "Access the content indexer, and look for definitions.  See the
ci function for details.
"
  (interactive
   (let* ((default-res (concat "@def " (current-word))) ;;(ci-find-default-str))
          (user-query (read-string "Search for: " default-res 'ci-history)))
     (list ci-machine ci-catalog ci-scope user-query)))

  (ci-internal machine catalog scope query))

(defun ci-internal (machine catalog scope query)
  "Internal helper for ci and ci-def functions."
  (let ((buf  (ci-get-output-buffer query)))
    (let ((proc (eval (append 
                       (list 'start-process ci-command buf ci-command) 
                       (ci-argument-list machine catalog scope query)))))
	  (set-process-filter   proc 'ci-filter)
  	  (set-process-sentinel proc 'ci-sentinel))))

(provide 'ci)
