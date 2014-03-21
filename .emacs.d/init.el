;; =================================================================
;; Filename: .emacs
;; Emacs initialization file
;; John Doty
;;
;; johndoty@microsoft.com
;; =================================================================

;; This is my .emacs.
;; There are many like it, but this one is mine.
;;
;; 2014/03/21 - Started to re-work it based on https://github.com/dimitri/emacs-kicker/blob/master/init.el
;;
;;              This emacs file has been around for a very long time, and it
;;              has accumulated a lot of stuff. I'd like to try to clean it
;;              up a little bit.... 
;;
;;              ...turns out that lots of the customization still makes
;;              sense. But fetching the packages is still the hard part.
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-matching-paren-dont-ignore-comments t)
 '(c-echo-syntactic-information-p t)
 '(c-indent-level 4)
 '(c-label-minimum-indentation 0)
 '(c-label-offset -4)
 '(fast-lock-cache-directories (quote ("~/flc-cache")))
 '(fast-lock-minimum-size nil)
 '(fill-column 77)
 '(find-file-run-dired t)
 '(font-lock-global-modes t)
 '(font-lock-maximum-size nil)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(make-backup-files nil)
 '(mouse-buffer-menu-mode-mult 0)
 '(org-hide-leading-stars t)
 '(org-odd-levels-only t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(rmail-mail-new-frame t)
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t))))
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(sd-user-email "johndoty@microsoft.com")
 '(sd-verbose nil)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(which-func-mode-global t nil (which-func))
 '(widget-editable-list-gui t)
 '(x-stretch-cursor nil))

;; =================================================================
;; Basic setup -- bootstrap el-get
;; =================================================================

(require 'cl)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      ;; NOTE: In order for this to work, you need cygwin and the version of
      ;; openssl from there. (If you see an error like 'symbol's value as
      ;; variable is void: return:0' then this is what is broken.)
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (copy-to-buffer (get-buffer "*scratch*") (point-min) (point-max))
    (eval-print-last-sexp)))

;; =================================================================
;; Custom recipies. Not quire sure how this works.
;; =================================================================
(setq
 el-get-sources
 '(
   (:name smex				     ; a better (ido like) M-x
          :after (progn
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   
   (:name magit				; git meet emacs, and a binding
          :after (progn
                   (global-set-key (kbd "C-x C-z") 'magit-status)))
   
   (:name goto-last-change	    ; move pointer back to last change
          :after (progn
                   ;; when using AZERTY keyboard, consider C-x C-_
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))

   (:name auto-complete
          :after (progn
                   (require 'auto-complete)))
   ))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; =================================================================
;; Custom packages
;; =================================================================

(setq
 my:el-get-packages
 '(
   el-get				    ; el-get is self-hosting
   escreen            		; screen for emacs, C-\ C-h
   switch-window			; takes over C-x o
   auto-complete			; complete as you type with overlays
   zencoding-mode			; http://www.emacswiki.org/emacs/ZenCoding
   ruby-mode                ; Major mode for editing Ruby files
   color-theme              ; Color themes
   color-theme-solarized    ; ...Solarized
   csharp-mode              ; C# mode
   js2-mode                 ; Improved JS mode
   PowerShell-Mode          ; Powershell mode
   lua-mode                 ; LUA
   go-mode                  ; Go programming language mode
   filladapt                ; Adaptive fills
   flyspell                 ; Spell-checking
   flymake                  ; Compiling

   go-autocomplete          ; Autocomplete for golang
   popup                    ; Pretty completions?

   ts-mode                  ; Typescript ?

   ;; ----- PROVISIONAL (for whatever that's worth)
   auto-complete-nxml       ; Auto-complete for nxml (maybe?)
   ))



;; =================================================================
;; GET THE PACKAGES
;; =================================================================
(el-get 'sync my:el-get-packages)

;; =================================================================
;; Load Path Customization
;; =================================================================

;; add private lisp directory to load-path.
(add-to-list 'load-path "~/site-lisp")

;; =================================================================
;; EMACS general look and feel 
;; =================================================================

;; If you want to have comments displayed in italics,
;; uncomment the following line. Note that this must
;; be done before font settings! (Emacs 20)
(setq w32-enable-italics t)

;; Shut off annoying sound
(if (fboundp 'set-message-beep) (set-message-beep 'silent))

;; Set the icon and frame titles %f file name, %b buffer name
(setq frame-title-format "%f")
(setq icon-title-format  "%b")

;; show column number in status bar
(setq column-number-mode t)

;; make searches case-INsensitive
(set-default 'case-fold-search t)

;; Enable uppercase or lowercase conversions
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;; Stop blinking!  For the love of god, STOP BLINKING!!!
(blink-cursor-mode 0)

;; No tool bars! No menu bars! I don't use that stuff anyway.
;;
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Consolas.
;;
(setq my-font-choice "Consolas-11")
;;
;; To obtain new font string, execute eval-expression, and eval this:
;;(insert(prin1-to-string(w32-select-font)))
;; This will show the required string in the scratch buffer.

(setq jd-frame-height
      (cond ((> (display-pixel-height) 900) 60)
            ((> (display-pixel-height) 768) 48)
            ('t 40)))

;; frame settings.  default-frame-alist controls what a default frame
;; looks like.
(setq default-frame-alist
      `((font             . ,my-font-choice)
        (width            . 91)
        (height           . ,jd-frame-height)
        ,@default-frame-alist))

;; initial-frame-alist controls what the first frame looks like.
(setq initial-frame-alist
      `((font             . ,my-font-choice)
        (width            . 91)
        (height           . ,jd-frame-height)))

;; This is just here for playing with things.
;;(set-frame-font my-font-choice)

;; COLORZ!
;;
(if (display-graphic-p)
    (progn
      (require 'color-theme)
      (require 'color-theme-solarized)
      (color-theme-solarized-light)))

;; Modeline format:
(display-time-mode -1)

;; =================================================================
;; FUN WITH KEY BINDINGS!  YAAAAYYY!!!
;; =================================================================
(global-set-key (read-kbd-macro "M-g") 'goto-line)
(global-set-key (read-kbd-macro "C-q") 'copy-region-as-kill)
(global-set-key (read-kbd-macro "C-w") 'kill-region)

(global-set-key (read-kbd-macro "<home>") 'beginning-of-buffer)
(global-set-key (read-kbd-macro "<end>")  'end-of-buffer)

(global-set-key (read-kbd-macro "M-1") 'new-frame)
(global-set-key (read-kbd-macro "M-3") 'delete-frame)

(global-set-key (read-kbd-macro "C-x f") 'font-lock-fontify-buffer)

;; =================================================================
;; Random Goo.
;; Drunken men who don't know where they are, and no longer care.
;; =================================================================

;; Get rid of old versions of files
(setq delete-old-versions t)

;; Font Menus
(setq w32-use-w32-font-dialog t)

;; Adaptive fill for everybody!
(require 'filladapt)
(setq-default filladapt-mode t)

;; Also, ido mode. Which is the BEST thing. Really.
(require 'ido)

;; =================================================================
;; Text mode configuration.
;; =================================================================
(defun my-text-mode-hook ()
  (setq fill-column 70)
  (turn-on-auto-fill)
  (flyspell-mode))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; =================================================================
;; CC-Mode configuration.  Stuff that makes working in IDL, C, and 
;; C++ a whole lot more tolerable.
;; =================================================================
;;
;; Hey, I know what!  Let's force enter to indent the line we're adding, 
;; automatically!  That might be nifty!
;;
;; Turn on fill mode for c-mode, and put c-context-line-break in
;; for newlines.
;;
;; Also, to get the neat vs-like behaviour, indent the block when you 
;; type a closing curly brace...
;;
(defun indent-on-closing-bracket (arg)
  (interactive "p")
  (let ((m1 (make-marker)) (m2 (make-marker)))
    (self-insert-command arg)
    (set-marker m2 (point))
    (forward-char 1)
    (c-backward-token-1 1 t)
    (set-marker m1 (point))
    (goto-char m2)
    (indent-region m1 m2 nil)))

(defun my-c-common-hook ()
  (turn-on-auto-fill)
  (flyspell-prog-mode)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (set-fill-column 120)
  (local-set-key "}" 'indent-on-closing-bracket))

(add-hook 'c-mode-common-hook 'my-c-common-hook)

;; Don't know why I need this all of a sudden...
(require 'flymake)

;; To make working w/ idl files easier:
(defun idl-insert-guid ()
  "Insert a GUID into the current buffer"
  (interactive)
  (call-process "uuidgen" nil t)
  (backward-delete-char 1))

(defun idl-insert-interface ()
  "Insert a well-formed interface definition (complete with new GUID) into the current buffer"
  (interactive)
  (insert "[\n")
  (insert " object,\n")
  (insert " pointer_default(unique),\n")
  (insert " uuid(")
  (idl-insert-guid)
  (insert ")\n")
  (insert "]\n")
  (insert "interface : IUnknown\n")
  (insert "{\n")
  (insert "}\n"))

(add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.w\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("makefile"  . makefile-mode))
(add-to-list 'auto-mode-alist '("sources"   . makefile-mode))

;; My c-mode stuff:
(c-add-style "ms-c"
             '("gnu"
               (c-basic-offset . 4)
               (c-offsets-alist . ((c                     . c-lineup-C-comments)
                                   (inclass               . +)
                                   (access-label          . -)
                                   (defun-block-intro     . +)
                                   (substatement-open     . 0)
                                   (statement-block-intro . +)
                                   (innamespace           . +)
                                   (statement-case-intro  . +)
                                   (statement-case-open   . 0)
                                   (brace-list-intro      . +)
                                   (substatement          . +)
                                   (arglist-intro         . +)
                                   (arglist-close         . 0)
                                   (statement-case-open   . +)
                                   ))))

(defun my-c-mode-hook ()
  (c-set-style "ms-c"))
(add-hook 'c-mode-hook    'my-c-mode-hook)
(add-hook 'c++-mode-hook  'my-c-mode-hook)
(add-hook 'java-mode-hook 'my-c-mode-hook)

(font-lock-add-keywords 'c-mode   '(("\\<\\(__try\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c++-mode '(("\\<\\(__try\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode   '(("\\<\\(__finally\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c++-mode '(("\\<\\(__finally\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode   '(("\\<\\(__except\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c++-mode '(("\\<\\(__except\\)" 1 font-lock-keyword-face t)))

;; Warnings and such.
(set-face-background 'font-lock-warning-face "Yellow")
(set-face-bold-p 'font-lock-warning-face t)

(defun add-todo-keyword (word)
  (font-lock-add-keywords 'c++-mode
                          (list (list (concat "\\<\\(" word "\\):") 
                                      1 font-lock-warning-face t)))
  (font-lock-add-keywords 'c-mode
                          (list (list (concat "\\<\\(" word "\\):") 
                                      1 font-lock-warning-face t)))
  (font-lock-add-keywords 'java-mode
                          (list (list (concat "\\<\\(" word "\\):")
                                      1 font-lock-warning-face t)))

  (font-lock-add-keywords 'csharp-mode
                          (list (list (concat "\\<\\(" word "\\):")
                                      1 font-lock-warning-face t)))
  )
(add-todo-keyword "REVIEW")
(add-todo-keyword "FIXME")
(add-todo-keyword "TODO")
(add-todo-keyword "BUG")
(add-todo-keyword "BUG BUG")
(add-todo-keyword "BUG BUG BUG")
(add-todo-keyword "BUGBUG")
(add-todo-keyword "BUGBUGBUG")
(add-todo-keyword "HACK")
(add-todo-keyword "TRICK")
(add-todo-keyword "NOTE")

;; Also, comments get a different font:
;;(set-face-font 'font-lock-comment-face "-outline-Bitstream Vera Sans Mono-bold-r-normal-normal-12-90-96-96-c-*-iso10646-1")

(defun indent-buffer ()
  "Indent the entire current buffer based on the current mode"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(global-set-key (read-kbd-macro "C-c TAB") 'indent-buffer)

;; IDL
(c-add-style "ms-idl"
             '("gnu"
               (c-basic-offset . 4)
               (c-offsets-alist . ((c                     . c-lineup-C-comments)
                                   (inclass               . +)
                                   (access-label          . -)
                                   (defun-block-intro     . +)
                                   (substatement-open     . 0)
                                   (statement-block-intro . +)
                                   (innamespace           . +)
                                   (statement-case-intro  . +)
                                   (statement-case-open   . 0)
                                   (brace-list-intro      . +)
                                   (substatement          . +)
                                   (arglist-intro         . +)
                                   (arglist-close         . 0)
                                   (statement-case-open   . +)
                                   ))))

(defun my-idl-mode-hook ()
  (c-set-style "ms-idl"))

(add-hook 'idl-mode-hook    'my-idl-mode-hook)

;; =================================================================
;; C#-Mode configuration.
;; =================================================================

;; zbrad's csharp mode is integrated with cc-mode.  
;; (autoload 'csharp-mode "cc-mode")

;; Here is another one that is not.
;;(autoload 'csharp-mode "csharp-mode-0.8.6" "Major mode for editing C# code." t)

;; We're using the one loaded by the package manager, though.

(c-add-style "ms-csharp"
             '((c-basic-offset . 4)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . ((c                     . c-lineup-C-comments)
                                   (inclass               . +)
                                   (namespace-open        . 0)
                                   (namespace-close       . 0)
                                   (innamespace           . +)
                                   (class-open            . 0)
                                   (class-close           . 0)
                                   (defun-open            . 0)
                                   (defun-close           . 0)
                                   (defun-block-intro     . +)
                                   (inline-open           . 0)
                                   (statement-block-intro . +)
                                   (brace-list-intro      . +)
                                   (block-open            . -)
                                   (substatement-open     . 0)
                                   (arglist-intro         . +)
                                   (arglist-close         . 0)
                                   ))))

(defun my-csharp-mode-hook ()
  (turn-on-font-lock)
  (c-set-style "ms-csharp"))

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cool$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.cs$"   . csharp-mode))

;; =================================================================
;; "XML" Support
;;
;; nxml-mode FTW.
;; =================================================================

(add-to-list 'auto-mode-alist '("\\.sgml$" . nxml-mode))

(setq auto-mode-alist 
      (append '(
                ("\\.sgml$"    . nxml-mode)
                ("\\.idd$"     . nxml-mode)
                ("\\.ide$"     . nxml-mode)
                ("\\.htm$"     . nxml-mode)
                ("\\.html$"    . nxml-mode)
                ("\\.xml$"     . nxml-mode)
                ("\\.xsl$"     . nxml-mode)
                ("\\.fo$"      . nxml-mode)
                ("\\.config$"  . nxml-mode)
                ("\\.build$"   . nxml-mode)
                ("\\.mht$"     . nxml-mode)
                ("\\.csproj$"  . nxml-mode)
                ("\\.targets$" . nxml-mode)
                ("\\.proj$"    . nxml-mode)
                ("\\.manifest$". nxml-mode)
                ("\\.xsd$"     . nxml-mode)
                ("\\.xaml$"    . nxml-mode)
                )
              auto-mode-alist
              )
      )

(defun nxml-indent-on-tag-close (arg)
  (interactive "p")
  (self-insert-command arg)
  (nxml-indent-line))

(defun my-nxml-hook ()
  (turn-on-auto-fill)
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key ">"    'nxml-indent-on-tag-close)

  ;; Why does nxml not play well with font lock mode, huh?
  (local-set-key (read-kbd-macro "C-x f") 'nxml-fontify-buffer))

(add-hook 'nxml-mode-hook 'my-nxml-hook)

;; =================================================================
;; Python Support
;; =================================================================
(autoload 'python-mode "python-mode" "Python editing mode." t)

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))

;; =================================================================
;; JavaScript Support
;; =================================================================
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; =================================================================
;; Ruby Mode
;; =================================================================
(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

;; =================================================================
;; Powershell Mode
;; =================================================================
(autoload 'powershell-mode "powershell-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))

;; =================================================================
;; LUA Mode
;; =================================================================
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; =================================================================
;; Source Depot
;; =================================================================
(load-library "sd")
(setq sd-use-sdconfig-exclusively t)
(sd-set-sd-executable "c:/tools/x86/sd.exe")
(setq sd-global-config "sd.ini")
(setenv "SDCONFIG" "sd.ini")

;; =================================================================
;; Code Folding
;; =================================================================
(global-set-key (kbd "C-+")             'hs-toggle-hiding)
(global-set-key (kbd "C-<kp-add>")      'hs-toggle-hiding)
(global-set-key (kbd "M-<kp-add>")      'hs-toggle-hiding)
(global-set-key (kbd "C-<kp-subtract>") 'hs-hide-all)
(global-set-key (kbd "M-<kp-subtract>") 'hs-hide-all)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

(setq hs-set-up-overlay 'display-code-line-counts)

;; =================================================================
;; Go (#golang) Mode
;;
;; Note that apparently go-mode is too special for the standard
;; autoload/add-to-list stuff. Good for it!
;; =================================================================
;;(require 'go-autocomplete)
;;(require 'auto-complete-config)

(require 'go-mode)
(require 'go-autocomplete)
(require 'auto-complete-config)

(defun my-go-mode-hook ()
  (auto-complete-mode))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; =================================================================
;; Org-Mode
;; =================================================================
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; =================================================================
;; No idea why custom-set-faces is way down here, but OK. I must 
;; have been thinking of something.
;; =================================================================
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

