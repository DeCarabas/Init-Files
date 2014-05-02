;; =================================================================
;; Filename: .emacs
;; Emacs initialization file
;; John Doty
;;
;; john@d0ty.me
;; =================================================================

;; This is my .emacs.
;; There are many like it, but this one is mine.
;;
;;  (Well, it *was*.... but then I adopted starter-kit, because why not?)
;;
;; 2014/03/31 - Well, it isn't actually named .emacs anymore; but this is the
;;              real initialization file, for code and junk. init.el just
;;              does the package load stuff now. Don't know why package init
;;              was built to work like it does in emacs 24, but oh well.
;; 
;;              Abandoning el-get for ELPA; ELPA seems more official and more
;;              like what I want anyhow. Of course, this needs the
;;              two-file-dance, but it's worth it. Much of the infrastructure
;;              is based on starter-kit:
;;
;;                  https://github.com/eschulte/emacs24-starter-kit/blob/master/starter-kit.org
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


;; =================================================================
;; Various bits of path setup (init-dir is defined by init.el)
;; =================================================================
(add-to-list 'load-path init-dir)
(setq autoload-file (concat init-dir "loaddefs.el"))
(setq package-user-dir (concat init-dir "elpa"))
(setq custom-file (concat init-dir "custom.el"))
(load custom-file)

;; =================================================================
;; Common stuff that's needed once
;; =================================================================
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; =================================================================
;; Packages
;; =================================================================
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)

(defvar my-packages
  (list 
   'switch-window	    ; takes over C-x o
   'auto-complete	    ; complete as you type with overlays
   'zencoding-mode  	    ; http://www.emacswiki.org/emacs/ZenCoding
   'ruby-mode                ; Major mode for editing Ruby files
   'color-theme              ; Color themes...
   'color-theme-solarized    ; ...Solarized
   'csharp-mode              ; C# mode
   'js2-mode                 ; Improved JS mode
   'powershell-mode          ; Powershell mode
   'lua-mode                 ; LUA
   'go-mode                  ; Go programming language mode
   'flyspell                 ; Spell-checking

   'flymake                  ; Compiling
   'flycheck                 ; Checking

   'go-autocomplete          ; Autocomplete for golang
   'popup                    ; Pretty completions?

   'python-mode              ; Python

   'tss                      ; Typescript, ala https://github.com/aki2o/emacs-tss

   ;; ----- PROVISIONAL (for whatever that's worth)
   'auto-complete-nxml       ; Auto-complete for nxml (maybe?)
   'magit                    ; Magit?
   )
  "Libraries that should be installed by default.")

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

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
(require 'cl)
(defun font-existsp (font)
  (if (and (fboundp 'x-list-fonts) (null (x-list-fonts font)))
      nil t))

(setq my-font-choice
      (find-if 
       'font-existsp
       '("Consolas-11" "Inconsolata-11")))

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
;; (set-frame-font my-font-choice)

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
  (set-fill-column 120)

  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key ">"    'nxml-indent-on-tag-close)

  ;; Why does nxml not play well with font lock mode, huh?
  (local-set-key (kbd "<C-return>") 'nxml-complete)
  (local-set-key (read-kbd-macro "C-x f") 'font-lock-fontify-buffer))

(add-hook 'nxml-mode-hook 'my-nxml-hook)

;; =================================================================
;; Python Support
;; =================================================================
(autoload 'python-mode "python-mode" "Python editing mode." t)

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

;; =================================================================
;; JavaScript Support
;; =================================================================
(autoload 'js2-mode "js2-mode" nil t)
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
;; =================================================================

(require 'go-mode)
(require 'auto-complete-config)
;(require 'go-autocomplete)

(defun my-go-mode-hook ()
  (flycheck-mode)
  ;(auto-complete-mode)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; =================================================================
;; Org-Mode
;; =================================================================
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; =================================================================
;; Typescript-Mode
;; =================================================================
(autoload 'typescript-mode "typescript" nil t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(tss-config-default)

;; =================================================================
;; Archive mode for appx
;; =================================================================
(add-to-list 'auto-mode-alist '("\\.appx\\'" . archive-mode))
(add-to-list 'auto-coding-alist '("\\.appx\\'" . no-conversion))
