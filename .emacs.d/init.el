;;; init.el -- Summary
;;; Emacs initialization file
;;; john@d0ty.me
;;;
;;; Commentary:
;;; This is my .emacs.
;;; There are many like it, but this one is mine.
;;;
;;; 2019/03/27 - Moved back into init.el, to make profiling possible.
;;;
;;; 2016/12/03 - Just a note: been using Emacs far more heavily as my core
;;;              editor @ FB for some reason.  (They push a set of packages
;;;              called Nuclide for the Atom editor, but I stopped using
;;;              that.)
;;;
;;; 2014/03/31 - Well, it isn't actually named .emacs anymore; but this is
;;;              the real initialization file, for code and junk.  init.el
;;;              just does the package load stuff now.  Don't know why
;;;              package init was built to work like it does in Emacs 24, but
;;;              oh well.
;;;
;;;              Abandoning el-get for ELPA; ELPA seems more official and
;;;              more like what I want anyhow.  Of course, this needs the
;;;              two-file-dance, but it's worth it.  Much of the
;;;              infrastructure is based on starter-kit:
;;;
;;;                  https://github.com/eschulte/emacs24-starter-kit/blob/master/starter-kit.org
;;;
;;; 2014/03/21 - Started to re-work it based on https://github.com/dimitri/emacs-kicker/blob/master/init.el
;;;
;;;              This Emacs file has been around for a very long time, and it
;;;              has accumulated a lot of stuff.  I'd like to try to clean it
;;;              up a little bit....
;;;
;;;              ...turns out that lots of the customization still makes
;;;              sense.  But fetching the packages is still the hard part.
;;;
;;; (Changes before this are lost in the mists of time.  This file dates from
;;; at least 1998.)
;;;
;;; Code:
;; =================================================================
;; First, before anything... server goop.
;; =================================================================
(require 'server)
(if (not (server-running-p)) (server-start))

;; =================================================================
;; Various bits of path setup
;; =================================================================
(defvar init-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory containing the init file.")

;; (setq autoload-file (concat init-dir "loaddefs.el"))
(setq custom-file (concat init-dir "custom.el"))
(load custom-file)

;; =================================================================
;; Load Path Customization
;; =================================================================

;; add private lisp directory to load-path.
(add-to-list 'load-path (directory-file-name "~/site-lisp"))

;; =================================================================
;; FB STUFF
;; =================================================================
(defconst master-dir (getenv "LOCAL_ADMIN_SCRIPTS"))
(defconst engshare-master (getenv "ADMIN_SCRIPTS"))
(defconst is-fb-environment
  (file-exists-p "/usr/share/emacs/site-lisp/fb-master.el")
  "Are we running on an FB machine or not?")

(when is-fb-environment
  ;; Load the master.emacs file which apparently has stuff in it I want?
  (load-library "/usr/share/emacs/site-lisp/fb-master.el")

  ;; Set up the proxy for working properly from the devserver.
  (if (and
       (getenv "HOSTNAME")
       (string-match-p ".+\.facebook\.com" (getenv "HOSTNAME")))
      (setq url-proxy-services
            '(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . "fwdproxy:8080")
              ("https" . "fwdproxy:8080"))))
  )

;; =================================================================
;; Packages
;; =================================================================
;; See http://dotyl.ink/l/qbmhz43kju
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (setq package-archives
        '(("gnu"         . "https://elpa.gnu.org/packages/")
          ("org"         . "https://orgmode.org/elpa/")
          ))

  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  )
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents)
  (package-install-selected-packages))

;; =================================================================
;; Common stuff that's needed once
;; =================================================================
(require 'saveplace) ;; Am I using this?
(require 'ffap)      ;; Am I using this?
(require 'uniquify)  ;; Unique buffers based on file name.
(require 'ansi-color)
(when is-fb-environment
  (require '50-arc))
(require 'ert) ;; I don't know, I started getting probs.

(prefer-coding-system 'utf-8)

;; =================================================================
;; EMACS general look and feel
;; =================================================================

;; If you want to have comments displayed in italics,
;; uncomment the following line. Note that this must
;; be done before font settings! (Emacs 20)
(when (boundp 'w32-enable-italics)
  (setq w32-enable-italics t))

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
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Modeline format:
(display-time-mode -1)

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; ================================================================
;; Fonts and windows and the like, only if graphics.
;; ================================================================
;;
;; (I added this because for some reason on 2016-09-26 my emacs started
;; segfaulting on my devserver when it called find-font, and I'll be damned
;; if I'm going to debug it.)
;;

(if (display-graphic-p)
    (let ((jd-frame-height))
      ;; Consolas. (And, to a lesser extent, Inconsolata.)
      ;;
      (defun font-candidate (&rest fonts)
        "Return existing font which first match."
        (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

      (defvar my-font-choice
        (cond
         ((string-equal (downcase (system-name)) "bifrost")
          "InputMonoNarrow-14")

         ((string-equal (downcase (system-name)) "unstablesurface")
          "Input Mono Narrow:pixelsize=28:weight=normal")

         (t
          (font-candidate
           "Input Mono Narrow:pixelsize=14:weight=normal"
           "InputMonoNarrow-14"
           "Consolas-10"
           "Inconsolata-11"
           "Monaco-14")))
        "The font I'm using, in graphics mode.")

      ;; This is just here for playing with things.
      (set-frame-font my-font-choice)

      ;;
      ;; To obtain new font string, execute eval-expression, and eval this:
      ;; (insert(prin1-to-string(w32-select-font)))
      ;; This will show the required string in the scratch buffer.


      ;; NOTE: I used to compute the height of the initial frame based on
      ;; display pixel height but it got unsustainable and I hated it.  (setq
      ;; jd-frame-height
      ;;       (cond ((> (display-pixel-height) 900) 60)
      ;;             ((> (display-pixel-height) 768) 48)
      ;;             ('t 40)))


      ;; frame settings.  default-frame-alist controls what a default frame
      ;; looks like.
      (setq default-frame-alist
            `((font             . ,my-font-choice)
              (width            . 91)
              ,@default-frame-alist))

      ;; initial-frame-alist controls what the first frame looks like.
      (setq initial-frame-alist
            `((font             . ,my-font-choice)
              (width            . 91)))
      ))

(use-package modus-themes :ensure
  :config
  (load-theme (if (display-graphic-p)
                  'modus-operandi
                'modus-vivendi)
              t))

;; =================================================================
;; FUN WITH KEY BINDINGS!  YAAAAYYY!!!
;; =================================================================
(global-set-key (read-kbd-macro "<end>")   'end-of-buffer)
(global-set-key (read-kbd-macro "<home>")  'beginning-of-buffer)

(global-set-key (read-kbd-macro "C-/")     'comment-or-uncomment-region)
(global-set-key (read-kbd-macro "C-c TAB") 'indent-buffer)
(global-set-key (read-kbd-macro "C-q")     'copy-region-as-kill)
(global-set-key (read-kbd-macro "C-w")     'kill-region)
(global-set-key (read-kbd-macro "C-x f")   'font-lock-fontify-buffer)

(global-set-key (read-kbd-macro "M-1")     'new-frame)
(global-set-key (read-kbd-macro "M-3")     'delete-frame)
(global-set-key (read-kbd-macro "M-g")     'goto-line)

;; In addition, make sure various things are working properly with xterm-keys
;; on under tmux. (This has been the most reliable way to get putty to send
;; the right keystrokes into emacs.)
(defadvice terminal-init-screen
  ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
  (before tmux activate)
  "Apply xterm keymap, allowing use of keys passed through tmux."
  (if (getenv "TMUX")
    (let ((map (copy-keymap xterm-function-map)))
      (message "Activating tmux keys...")
      (set-keymap-parent map (keymap-parent input-decode-map))
      (set-keymap-parent input-decode-map map))))

;; =================================================================
;; Random Goo.
;; Drunken men who don't know where they are, and no longer care.
;; =================================================================

;; Font Menus
(setq w32-use-w32-font-dialog t)

;; Adaptive fill for everybody!
(use-package filladapt :ensure t
  :init (setq-default filladapt-mode t))

(require 'ido)

;; Cleanup all the whitespaces.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Fix path loading on MacOS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Take a multi-line REGION and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun my--fix-aspell ()
  "Fix aspell location when it's not there, by looking in hard-coded locations."
  (require 'ispell)
  (if (and (not (executable-find ispell-program-name))
           (file-exists-p "c:/msys64/usr/bin/aspell.exe"))
      (progn
        (message "Redirecting aspell to known location")
        (setq ispell-program-name "c:/msys64/usr/bin/aspell.exe"))))

(add-hook 'ispell-minor-mode-hook 'my--fix-aspell)
(add-hook 'flyspell-mode-hook 'my--fix-aspell)

;; =================================================================
;; Text mode configuration.
;; =================================================================
(defun my-text-mode-hook ()
  "Doty's hook for text mode."
  (setq fill-column 70)
  (turn-on-auto-fill)
  (my--fix-aspell)
  (flyspell-mode))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; =================================================================
;; Company?
;; =================================================================
(use-package company :ensure t
  :commands company-mode
  :hook (typescript-mode . company-mode))

;; =================================================================
;; Common configuration for LSP-based systems.
;; =================================================================
(defvar my-clangd-executable
  (executable-find "clangd")
  "Path to the clangd binary.")

(defvar my-cppls-fbcode-executable
  (executable-find "cppls-wrapper")
  "The path to the fbcode C++ language service wrapper.")

(defvar my-pylsp-executable
  (executable-find "pylsp")
  "The path to the python-lsp-server binary.")

(defvar my-pyls-executable
  (executable-find "pyls")
  "The path to the python-language-server binary.")

(defvar my-pyls-language-server-executable
  (executable-find "pyls-langauge-server")
  "The path to the pyls-language-server binary (used at FB).")

(defun my-disable-flycheck-on-eglot ()
  "Disable flycheck in eglot-managed buffers."
  (flycheck-mode (if (eglot-managed-p) -1 nil)))

(defun my-eglot-connect-hook (server)
  "Connect to SERVER. Don't send configuration information in C or C++."
  (unless (or (eq major-mode 'c++-mode)
              (eq major-mode 'c-mode))
    (eglot-signal-didChangeConfiguration server)))


(use-package eglot :ensure
  :commands (eglot-ensure eglot)
  :hook
  (python-mode . eglot-ensure)
  (rust-mode   . eglot-ensure)
  (c++-mode    . eglot-ensure)
  (c-mode      . eglot-ensure)
  (go-mode     . eglot-ensure) ;; 2022-07-29 Add eglot for go
  :bind
  ("C-c \\" . eglot-code-actions) ;; 2022-07-29 I want to make code actions easier.
  :config
  (let ((cpp-executable (or my-cppls-fbcode-executable
                            my-clangd-executable)))
    (when cpp-executable
      (add-to-list 'eglot-server-programs
                   `((c++-mode c-mode) . (,cpp-executable)))))

  (let ((py-executable (or my-pyls-language-server-executable
                           my-pylsp-executable
                           my-pyls-executable)))
    (when py-executable
      (add-to-list 'eglot-server-programs
                   `(python-mode . (,py-executable)))))

  ;; 2022-04-28 Configuration for Deno.
  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")
  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    (list :enable t :lint t))
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))
  ;; --

  (add-hook 'eglot-managed-mode-hook 'my-disable-flycheck-on-eglot)
  (remove-hook 'eglot-connect-hook 'eglot-signal-didChangeConfiguration)
  (add-hook 'eglot-connect-hook 'my-eglot-connect-hook))

;; NOTE: elgot defers to flymake for error information.
(use-package flymake
  :bind (("C-c n" . 'flymake-goto-next-error)
         ("C-c p" . 'flymake-goto-prev-error)))


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
    (c-backward-token-2 1 t)
    (set-marker m1 (point))
    (goto-char m2)
    (indent-region m1 m2 nil)))

(defun my-c-common-hook ()
  "My common hook for C/C++/&c."
  (turn-on-auto-fill)
  (flyspell-prog-mode)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  ;; (local-set-key "}" 'indent-on-closing-bracket)
  )

(add-hook 'c-mode-common-hook 'my-c-common-hook)

;; Don't know why I need this all of a sudden...
(require 'flymake)

(add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.w\\'"   . c++-mode))

(add-to-list 'auto-mode-alist '("makefile"  . makefile-mode))
(add-to-list 'auto-mode-alist '("sources"   . makefile-mode))
(add-to-list 'auto-mode-alist '("dirs"      . makefile-mode))

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

(defun clang-format-cpp-buffer ()
  "Format a buffer with clang-format but only if it's C or C++.

Or, uh, Objective C, I guess."
  (when (or (eq major-mode 'c++-mode)
            (eq major-mode 'c-mode)
            (eq major-mode 'objc-mode))
    (clang-format-buffer)))

(defun my-c-mode-hook ()
  "Doty's `c-mode' hook."
  (c-set-style (if is-fb-environment "fb-c-style" "ms-c"))
  (add-hook 'before-save-hook 'clang-format-cpp-buffer))

(add-hook 'c-mode-hook    'my-c-mode-hook)
(add-hook 'c++-mode-hook  'my-c-mode-hook)
(add-hook 'java-mode-hook 'my-c-mode-hook)
(add-hook 'objc-mode-hook 'my-c-mode-hook)

(defconst jd-more-keywords
  '(;; These are keywords in Microsoft C/C++
    ("\\<\\(__try\\)" 1 font-lock-keyword-face t)
    ("\\<\\(__finally\\)" 1 font-lock-keyword-face t)
    ("\\<\\(__except\\)" 1 font-lock-keyword-face t)
    ;; Warnings
    ("\\<\\(REVIEW\\)" 1 font-lock-warning-face t)
    ("\\<\\(FIXME\\)" 1 font-lock-warning-face t)
    ("\\<\\(TODO\\)" 1 font-lock-warning-face t)
    ("\\<\\(BUG\\)" 1 font-lock-warning-face t)
    ("\\<\\(BUGBUG\\)" 1 font-lock-warning-face t)
    ("\\<\\(BUGBUGBUG\\)" 1 font-lock-warning-face t)
    ("\\<\\(HACK\\)" 1 font-lock-warning-face t)
    ("\\<\\(TRICK\\)" 1 font-lock-warning-face t)
    ("\\<\\(NOTE\\)" 1 font-lock-warning-face t))
  "Keywords to add to C/C++.")

(font-lock-add-keywords 'c-mode jd-more-keywords)
(font-lock-add-keywords 'c++-mode jd-more-keywords)

(defun indent-buffer ()
  "Indent the entire current buffer based on the current mode."
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

;; IDL
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
                                   (arglist-close         . +)
                                   (statement-case-open   . +)
                                   ))))

(defun my-idl-mode-hook ()
  "Doty's `idl-mode' hook."
  (c-set-style "ms-idl"))

(add-hook 'idl-mode-hook    'my-idl-mode-hook)

;; =================================================================
;; C#-Mode configuration.
;; =================================================================
(use-package csharp-mode :ensure t

  :preface
  (defun my-csharp-mode-hook ()
    "My C# mode hook."
    (require 'prettysharp)
    (prettysharp-mode)
    (turn-on-font-lock)
    (omnisharp-mode)
    (c-set-style "ms-csharp"))

  :mode "\\.cs\\'"

  :config
  (use-package omnisharp :ensure t
    :commands omnisharp-mode
    :bind (:map omnisharp-mode-map
                ([remap xref-find-definitions] . omnisharp-go-to-definition)
                ([remap xref-find-references] . omnisharp-find-usages)
                ;; `xref-pop-marker-stack' works as expected.
                )
    :config
    (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp)))

  (use-package prettysharp
    :commands prettysharp-mode
    :config
    (if (file-executable-p "c:/src/prettysharp/prettysharp.exe")
        (setq prettysharp-command "c:/src/prettysharp/prettysharp.exe")))

  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
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
                                     )))))



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
;; Elm
;; =================================================================
(require 'flycheck-elm)
(add-to-list 'flycheck-checkers 'elm)

(defun my-elm-hook ()
  "My ELM-MODE hook."
  (company-mode +1)
  (setq company-backends '(company-elm))
  (elm-oracle-setup-completion)
  (flycheck-elm-setup))

(add-hook 'elm-mode-hook 'my-elm-hook)

;; =================================================================
;; Flycheck
;; =================================================================
(require 'flycheck)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; (flycheck-define-checker python-fb-flake8
;;   "A Python syntax and style checker using FB's Flake8."
;;   :command ("flake8" source-original "--shebang" "--py2" "--py3")
;;   :standard-input nil
;;   :error-filter (lambda (errors)
;;                   (let ((errors (flycheck-sanitize-errors errors)))
;;                     (seq-do #'flycheck-flake8-fix-error-level errors)
;;                     errors))
;;   :error-patterns
;;   ((warning line-start
;;             (file-name) ":" line ":" (optional column ":") " "
;;             (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;             (message (one-or-more not-newline))
;;             line-end))
;;   :modes python-mode)
;; (add-to-list 'flycheck-checkers 'python-fb-flake8)

(global-flycheck-mode)

;; =================================================================
;; Python Support
;; =================================================================
(defun my-python-mode-hook ()
  "My hook for `python-mode`."
  (when is-fb-environment
    (flycheck-select-checker `python-fb-flake8))
  (unless (and (buffer-file-name)
               (string-match-p "TARGETS" (buffer-file-name)))
    (blacken-mode)))

(use-package python-mode :ensure
  :mode "\\.py\\'"
  :config
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  (add-hook 'python-mode-hook 'my-python-mode-hook))

(autoload 'blacken-mode "blacken" "Automatically run black before saving." t)

;; =================================================================
;; Bazel Support
;; =================================================================

(use-package bazel :ensure
  :mode  (("/\\.bazelignore\\'"                     . bazelignore-mode)
          ("/\\(?:\\(?:bazel\\)?\\.bazelrc\\)\\'"   . bazelrc-mode)
          ("/.+\\.bzl\\'"                           . bazel-starlark-mode)
          ("/MODULE\\.bazel\\'"                     . bazel-module-mode)
          ("/\\(?:WORKSPACE\\(?:\\.bazel\\)?\\)\\'" . bazel-workspace-mode)
          ("/\\(?:BUILD\\(?:\\.bazel\\)?\\)\\'"     . bazel-build-mode)))

;; =================================================================
;; JavaScript Support
;; =================================================================
;; (require 'rjsx-mode)
(require 'flycheck-flow)
(require 'flow-minor-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-next-checker 'javascript-eslint 'javascript-flow)

;; (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))

;; (defun my-js-mode-hook ()
;;   "My custom javascript mode hook."
;;   (add-node-modules-path)
;;   (flow-minor-enable-automatically)
;;   (prettier-js-mode))

;; (add-hook 'rjsx-mode-hook #'my-js-mode-hook)


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

(defun my-powershell-hook()
  (set (make-local-variable 'powershell-indent) 2)
  (set (make-local-variable 'powershell-continuation-indent) 2))

(add-hook 'powershell-mode-hook 'my-powershell-hook)

;; =================================================================
;; LUA Mode
;; =================================================================
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

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
  "Put the line counts in the fold overlay OV."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

(setq hs-set-up-overlay 'display-code-line-counts)

;; =================================================================
;; Go (#golang) Mode
;; =================================================================
(require 'project)

;;-----
;; 2022-07-28 Forgot why I added this configuration; disabling it because it
;; makes it very slow, and .git is accurate anyways.
;;
;; (defun project-find-go-module (dir)
;;   "A function for finding the dominating go.mod file in DIR for a go project."
;;   (when-let ((root (locate-dominating-file dir "go.mod")))
;;     (cons 'go-module root)))

;; (cl-defmethod project-root ((project (head go-module)))
;;   "Shrug PROJECT."
;;   (cdr project))

;; (add-hook 'project-find-functions #'project-find-go-module)
;;-----

(use-package go-mode :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

;; (require 'auto-complete-config)
;; (require 'go-autocomplete)

;; (defun my-go-mode-hook ()
;;   "My go-mode hook."
;;   (auto-complete-mode)
;;   )

;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;; =================================================================
;; Org-Mode
;; =================================================================
(defun my-org-mode-hook ()
  "My org mode hook."
  (turn-off-filladapt-mode)
  (my--fix-aspell)
  (require 'ox-quip))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda))
  :config
  (add-hook 'org-mode-hook 'my-org-mode-hook)

  ;; I want a sane approach to multi-line emphasis, and this is the only way
  ;; to get it. Think about 10 lines.
  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  )

;; =================================================================
;; Typescript-Mode
;; =================================================================
(defun ts/is-deno-project ()
  "Return non-nil if this is a deno project, otherwise nil."
  (locate-dominating-file (buffer-file-name) ".deno"))

(defun ts/enable-eglot-or-tide ()
  "Enable eglot if this is a deno project, otherwise enable tide."
  (if (ts/is-deno-project)
      (eglot-ensure)

    ;; Not a deno project; just enable tide and the normal
    (eldoc-mode)
    (tide-setup)
    (tide-hl-identifier-mode)))

(use-package typescript-mode :ensure t
  :config
  (add-hook 'typescript-mode-hook 'ts/enable-eglot-or-tide))

(use-package add-node-modules-path :ensure t
  :hook typescript-mode)

(use-package prettier-js :ensure t
  :hook (typescript-mode . prettier-js-mode))

(use-package tide :ensure t)

;; =================================================================
;; Archive mode for appx
;; =================================================================
(add-to-list 'auto-mode-alist '("\\.appx\\'" . archive-mode))
(add-to-list 'auto-coding-alist '("\\.appx\\'" . no-conversion))

;; =================================================================
;; Some build stuff; I swiped this from handmade-hero. Good for
;; unibuild setups.
;; =================================================================
(defun set-vs-environment ()
  "Load the VS environment variables into the current Emacs process."
  (interactive)
  (dolist
      (ev (split-string
           (shell-command-to-string "cmd /c \" \"%ProgramFiles(x86)%\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Auxiliary\\Build\\vcvars64.bat\" && set \"")
           "[\n]+"))
    (letrec ((spev (split-string ev "="))
             (vn (car spev))
             (vv (cadr spev)))
      (setenv vn vv))))



;; ;; =================================================================
;; ;; PHP stuff
;; ;; =================================================================
;; (if is-fb-environment
;;     (progn
;;       ;; Hack support for stuff in www
;;       (setq hack-for-hiphop-root (expand-file-name "www" "~"))
;;       ;;(load "/home/engshare/tools/hack-for-hiphop")

;;       (load-library "/usr/share/emacs/site-lisp/emacs-packages/hh-client.el")
;;       (require 'hh-client)

;;       (defun my-fb-php-hook ()
;;         (global-set-key (kbd "M-.") 'hh-client-find-definition))
;;       (add-hook 'php-mode-hook 'my-fb-php-hook)
;;       ))


;; =================================================================
;; Magit stuff
;; =================================================================
(use-package magit :ensure
  :bind ("C-x g" . magit-status))


;; =================================================================
;; Mercurial stuff
;; =================================================================
;; I remove Hg from VC-mode because it is SO SLOW.
(setq vc-handled-backends (remove 'Hg vc-handled-backends))
;; But I have monky enabled so I can use it instead.
(use-package monky
  :ensure t
  :config
  (setq monky-process-type 'cmdserver)
  :bind
  ("C-x h" . monky-status))

;; =================================================================
;; Shell stuff
;; =================================================================
(defun my-shell-mode-hook ()
  "Doty's hook for text mode."
  (setq show-trailing-whitespace nil)
  (buffer-disable-undo))

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "TERM" "xterm-256color")
(setenv "INEMACS" "true")
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; xterm-color
(use-package xterm-color :ensure
  :config
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output
                comint-output-filter-functions)))

;; ag
(global-set-key (kbd "M-p") 'ag-project-regexp)

(defun doty-term ()
  "Start a shell, the way doty wants it right now."
  (interactive)
  (ansi-term "screen"))


;; =================================================================
;; OCAML stuff
;; =================================================================
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

;; =================================================================
;; Insert timestamp
;; =================================================================
(defun insert-date ()
  "Insert the current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "C-c t") 'insert-date)

;; =================================================================
;; Markdown mode
;; =================================================================
(defun my-markdown-mode-hook ()
  "My hook for markdown mode."
  (turn-off-auto-fill)
  (setq truncate-lines nil)
  (setq word-wrap 't)
  (when is-fb-environment
    (require 'fb-note-publish)))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)


;; =================================================================
;; Rust
;; =================================================================
(use-package rust-mode :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust :ensure t
  :hook (rust-mode . flycheck-rust-setup))

;; =================================================================
;; Clojure
;; =================================================================
(use-package clojure-mode :ensure t
    :mode (("\\.clj\\'" . clojure-mode)
           ("\\.edn\\'" . clojure-mode))
    :config
    (use-package cider :ensure
      :config
      ;; Put TARGETS in clojure-build-tool-files so that directories with TARGETS
      ;; get identified as projects.
      (unless (member "TARGETS" clojure-build-tool-files)
        (setq clojure-build-tool-files (append clojure-build-tool-files '("TARGETS")))))

    (require 'cider-buck))

;; ================================================================
;; TRAMP
;; ================================================================
(use-package tramp
  :config
  ;; Since we're going to be doing this a lot, the minibar message tramp
  ;; spits out for every file access is both spammy, distracting, and often
  ;; hides more relevant messages.
  (setq tramp-message-show-message nil)
  ;; Let tramp search $PATH as given to the $USER on the remote machine
  ;; (necessary to find 'hphpd' for instance)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Use putty on windows
  (when (eq window-system 'w32)
    (setq tramp-default-method "plink")))


;; ================================================================
;; Zig
;; ================================================================
(use-package zig-mode :ensure t
    :mode (("\\.zig\\'" . zig-mode))
    :config
    (require 'lsp) ;; There's a use-package somewhere else...?
    (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "<path to zls>")
      :major-modes '(zig-mode)
      :server-id 'zls)))

;; ================================================================
;; Pico-8
;; ================================================================
(defun my-pico8-hook ()
  "My hook for pico-8 mode."
  ;; Pico-8 has a small indent.
  (setq lua-indent-level 2)
  (set-fill-column 32))

(use-package pico8-mode
  :mode (("\\.p8\\'" . pico8-mode))
  :config (add-hook 'pico8-mode-hook 'my-pico8-hook))

;; ================================================================
;; Ink
;; ================================================================
(defun my-ink-mode-hook ()
  "My hook for ink mode."
  (flymake-mode)
  (flycheck-mode 0)
  (turn-off-auto-fill)
  (setq truncate-lines nil)
  (visual-line-mode))

(use-package ink-mode :ensure t
  :mode (("\\.ink\\'" . ink-mode))
  :bind (:map ink-mode-map
              ("M-." . ink-follow-link-at-point)
              ("C-c ! n" . flymake-goto-next-error))
  :config
  (add-hook 'ink-mode-hook 'my-ink-mode-hook))

;; =================================================================
;; Note taking
;; =================================================================
;; howm http://howm.osdn.jp/
;; Based on http://dotyl.ink/l/kc56hcn64e

(defvar my-dropbox-dir
  (expand-file-name
   (cond
    ((file-directory-p "~/Dropbox (Personal)") "~/Dropbox (Personal)")
    ((file-directory-p "~/Dropbox") "~/Dropbox")
    ((file-directory-p "/mnt/c/Users/john/Dropbox") "/mnt/c/Users/john/Dropbox")))
  "Where is my dropbox?")

(use-package howm :ensure
  :init
  ;; Directory configuration
  ;;
  ;; (This is in :init because apparently you need to set this stuff before
  ;; you load howm?)
  (setq howm-home-directory (expand-file-name "notes/howm" my-dropbox-dir))
  (setq howm-directory      howm-home-directory)
  (make-directory howm-directory t)
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")

  ;; Use ripgrep as grep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)

  :config
  ;; un-bind control-h from the howm thing
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)

  ;; Rename buffers to their title
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name)
  )

;; =================================================================
;; Protocol Buffers
;; =================================================================
(use-package protobuf-mode :ensure)

;; =================================================================
;; Deadgrep for searching
;; =================================================================
(use-package deadgrep :ensure)


;;; init.el ends here
