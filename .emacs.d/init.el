;;; init.el -- Summary  -*- lexical-binding: t; -*-
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
  (package-refresh-contents))

;; Really we should be marking *everyting* here with :ensure rather than
;; downloading it all up front.
;;
;;   (package-install-selected-packages))

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
          "InputMonoNarrow-12")

         ((string-equal (downcase (system-name)) "fred")
          "InputMonoNarrow-16")

         ((string-equal (downcase (system-name)) "unstablesurface")
          "Input Mono Narrow:pixelsize=28:weight=normal")

         ((string-equal (downcase (system-name)) "oldconvert")
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

(use-package doom-themes :ensure t
  :config
  (load-theme 'doom-vibrant t)

  ;; I like fairy-floss' italic keywords, so let's do this.
  (custom-set-faces
   '(font-lock-keyword-face ((t (:slant italic))))))

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
  (use-package exec-path-from-shell :ensure t)
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

(defun my/copy-buffer-file-as-kill ()
  "Copy the current buffer file name to the kill ring."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))

        (select-enable-clipboard t))
    (when filename
      (kill-new filename)
      (message "Copied file name to kill ring: %s" filename))))


;; =================================================================
;; Tree Sitter
;; =================================================================
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;
;; 2023-08-26 Wow, like what am I even doing? This goes at the top of the
;; various things because we're going to be playing with modes and whatnot.

(when (and (functionp 'treesit-available-p) (treesit-available-p))
  (require 'treesit)
  (setq treesit-language-source-alist
        '(
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (hlsl "https://github.com/tree-sitter-grammars/tree-sitter-hlsl")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (scala "https://github.com/tree-sitter/tree-sitter-scala")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          ))

  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  (add-to-list 'major-mode-remap-alist '(scala-mode . scala-ts-mode))
  (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))
  )

(defun install-known-tree-sitter-grammars ()
  "Install all known tree-sitter grammars."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; 2023-08-28 Maybe I like line numbers everywhere? Who can say?
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; 2025-04-25 This is combined with adaptive-wrap is kinda nice, let's try it.
(add-hook 'prog-mode-hook 'visual-line-mode)


;; =================================================================
;; clipetty makes the kill ring interact with the terminal.
;; =================================================================
(when (getenv "TMUX")
  (use-package clipetty
    :ensure t
    :hook (after-init . global-clipetty-mode)))

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
;; Company? Company.
;; =================================================================
(use-package company :ensure t
  :commands company-mode
  :init
  ;; 2023-08-26: Enable company mode globally.
  (global-company-mode t)
  :config
  ;; 2023-08-26: Enable comapny mode globally.
  (setq company-idle-delay 0.3)
  )

;; =================================================================
;; Common configuration for LSP-based systems.
;; =================================================================
(defvar my-clangd-executable
  (or (executable-find "clangd-mp-14")  ;; Support newer clangd
      (executable-find "cppls-wrapper")
      (executable-find "clangd"))
  "Path to the clangd binary.")

;; 2023-08-23 Disabling all this nonsense for now; I'm using pyright at
;;            work and don't feel like maintaining this stuff.
;; (defvar my-pylsp-executable
;;   (executable-find "pylsp")
;;   "The path to the python-lsp-server binary.")
;;
;; (defvar my-pyls-executable
;;   (executable-find "pyls")
;;   "The path to the python-language-server binary.")
;;
;; (defvar my-pyls-language-server-executable
;;   (executable-find "pyls-langauge-server")
;;   "The path to the pyls-language-server binary (used at FB).")

(defun my-disable-flycheck-on-eglot ()
  "Disable flycheck in eglot-managed buffers."
  (flycheck-mode (if (eglot-managed-p) -1 nil)))

(defun my-eglot-connect-hook (server)
  "Connect to SERVER.  Don't send configuration information in C or C++."
  (unless (or (eq major-mode 'c++-mode)
              (eq major-mode 'c-mode))
    (eglot-signal-didChangeConfiguration server)))

(defun ts/server-program (interactive)
  "Choose which server program to run."
  (cond ((ts/is-deno-project) '("deno" "lsp" :initializationOptions :enable t :lint t))
        (t                    '("typescript-language-server" "--stdio"))))

(defun my-find-swift-lsp ()
  "Try to find the swift LSP."
  ;; On windows with scoop this lives in a weird place.
  (or (executable-find "sourcekit-lsp")  ;; Support newer clangd
      (executable-find "C:/Users/john/scoop/apps/swift/current/Developer/Toolchains/unknown-Asserts-development.xctoolchain/usr/bin/sourcekit-lsp")
      "sourcekit-lsp"))

(defun my-eglot-format-before-save ()
  "Format with eglot when I'm in a buffer that supports it."
  (when (or (eq major-mode 'csharp-mode)
            (eq major-mode 'csharp-ts-mode))
    (eglot-format)))

(use-package eglot :ensure t
  :commands (eglot-ensure eglot)
  :hook
  (python-mode        . eglot-ensure)
  (python-ts-mode     . eglot-ensure)
  (rust-mode          . eglot-ensure)
  (rust-ts-mode       . eglot-ensure) ;; 2023-08-26 Add eglot for tree-sitter rust?
  (c++-mode           . eglot-ensure)
  (c-mode             . eglot-ensure)
  (go-mode            . eglot-ensure) ;; 2022-07-29 Add eglot for go
  (typescript-mode    . eglot-ensure) ;; 2023-09-03 Eglot for typescript
  (typescript-ts-mode . eglot-ensure) ;; 2023-09-03 Eglot for typescript
  (swift-mode         . eglot-ensure) ;; 2023-11-11 Eglot for swift?
  (scala-mode         . eglot-ensure) ;; 2024-09-24 Eglot for scala
  (scala-ts-mode      . eglot-ensure) ;; 2024-09-24 Eglot for scala
  (csharp-mode        . eglot-ensure) ;; 2025-04-19 Eglot for csharp
  (csharp-ts-mode     . eglot-ensure) ;; 2025-04-19 Eglot for csharp

  ;; 2023-09-10 Respect language-specific formatters
  ;;
  ;; Something inside me *yearns* for the LSP to be the authoritative
  ;; formatter, and therefore to be able to just say "eglot, take care of it"
  ;; but it's just not the case right now. (The inciting incident is that
  ;; prettier and typescript-language-server diagree on how typescript is to
  ;; be formatted, and after much soul-searching I have decided to side with
  ;; prettier.)
  ;;
  ;; 2025-04-19 Use a custom function that is conditional on mode.
  (before-save     . my-eglot-format-before-save)
  :bind
  ("C-c r"  . eglot-rename)       ;; 2022-08-23 Make rename more accessible
  ("C-c \\" . eglot-code-actions) ;; 2022-07-29 I want to make code actions easier.
  :config
  (when my-clangd-executable
    (add-to-list 'eglot-server-programs
                 `((c++-mode c-mode) . (,my-clangd-executable))))

  ;; 2023-08-23 Disabling all this nonsense for now; I'm using pyright at
  ;;            work and don't feel like maintaining this stuff.
  ;;
  ;; (let ((py-executable (or my-pyright-executable
  ;;                          my-pyls-language-server-executable
  ;;                          my-pylsp-executable
  ;;                          my-pyls-executable)))
  ;;   (when py-executable
  ;;     (add-to-list 'eglot-server-programs
  ;;                  `(python-mode . (,py-executable)))))

  ;; 2023-09-03 Re-work the way that the JS/Deno switch is handled.
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . ts/server-program))

  ;; 2023-10-26 Use cargo clippy instead of cargo check
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                             (:check (:command "clippy")))))

  ;; 2024-09-19 Metals stuff
  (add-to-list 'eglot-server-programs
               '((scala-mode scala-ts-mode) . ("metals" :initializationOptions (:isHttpEnabled t))))

  ;; --

  (add-to-list 'eglot-server-programs
               `(swift-mode . ( ,(my-find-swift-lsp) "--sync" "--log-level" "debug")))

  (add-hook     'eglot-managed-mode-hook 'my-disable-flycheck-on-eglot)
  (remove-hook  'eglot-connect-hook      'eglot-signal-didChangeConfiguration)
  (add-hook     'eglot-connect-hook      'my-eglot-connect-hook))

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
;; (add-hook 'java-mode-hook 'my-c-mode-hook)
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
    (c-set-style "ms-csharp"))

  :mode "\\.cs\\'"

  :config
  ;; 2023-09-03 Stop using omnisharp
  ;; 2025-04-19 Still not using omnisharp, use it through eglot maybe
  ;; (use-package omnisharp :ensure t
  ;;   :commands omnisharp-mode
  ;;   :bind (:map omnisharp-mode-map
  ;;               ([remap xref-find-definitions] . omnisharp-go-to-definition)
  ;;               ([remap xref-find-references] . omnisharp-find-usages)
  ;;               ;; `xref-pop-marker-stack' works as expected.
  ;;               )
  ;;   :config
  ;;   (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp)))

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
(use-package flycheck-elm :ensure t
  :after (flycheck)
  :config
  (add-to-list 'flycheck-checkers 'elm))

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
(use-package flycheck :ensure t
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  :hook (emacs-lisp-mode elm-mode) ;; 2023-11-05 NOTE I'm gonna have to rebuild this
  )

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

;; (use-package python-mode :ensure
;;   :init
;;   ;; NOTE: Not using :mode here because it implies :defer which... doesn't
;;   ;; work with python-mode because it fights the built-in python mode.
;;   (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;   :config
;;   (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;;   (add-hook 'python-mode-hook 'my-python-mode-hook))

(use-package blacken :ensure t
  :commands (blacken-mode)
  :hook (python-mode . blacken-mode))

;; 2023-08-23 Disabling all this nonsense for now; I'm using pyright at
;;            work and don't feel like maintaining this stuff.
;; (use-package lsp-pyright :ensure
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

;; =================================================================
;; Bazel Support
;; =================================================================

(use-package bazel :ensure t
  :mode  (("/\\.bazelignore\\'"                     . bazelignore-mode)
          ("/\\(?:\\(?:bazel\\)?\\.bazelrc\\)\\'"   . bazelrc-mode)
          ("/.+\\.bzl\\'"                           . bazel-starlark-mode)
          ("/MODULE\\.bazel\\'"                     . bazel-module-mode)
          ("/\\(?:WORKSPACE\\(?:\\.bazel\\)?\\)\\'" . bazel-workspace-mode)
          ("/\\(?:BUILD\\(?:\\.bazel\\)?\\)\\'"     . bazel-build-mode)
          ("/.+\\.tilt\\'"                          . bazel-starlark-mode)
          ("/Tiltfile$"                             . bazel-starlark-mode)
          ("BUCK"                                   . bazel-build-mode)
          ("/..bxl\\'"                              . bazel-starlark-mode)
          ))

(defun my/open-bazel-build ()
  "Open the build.bazel file that dominates this source file."
  (interactive)
  (find-file-other-window
   (concat (locate-dominating-file (buffer-file-name) "BUILD.bazel")
           "BUILD.bazel")))


;; =================================================================
;; JavaScript Support
;; =================================================================
;; (require 'rjsx-mode)
;; (require 'flycheck-flow)
;; (require 'flow-minor-mode)

;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-jshint)))
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)

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

;; 2023-09-03 Trying eglot instead of TIDE for now.
;; (defun ts/enable-eglot-or-tide ()
;;   "Enable eglot if this is a deno project, otherwise enable tide."
;;   (if (ts/is-deno-project)
;;       (eglot-ensure)

;;     ;; Not a deno project; just enable tide and the normal
;;     (tide-setup)
;;     (flycheck-mode +1)
;;     (tide-hl-identifier-mode)
;;     (eldoc-mode +1)))

(use-package typescript-mode :ensure t
  ;; 2023-09-03 Trying eglot instead of TIDE for now.
  ;; :config
  ;; (add-hook 'typescript-mode-hook 'ts/enable-eglot-or-tide)
  )

(use-package add-node-modules-path :ensure t
  :hook (typescript-mode . add-node-modules-path))

(use-package prettier-js :ensure t
  :hook (typescript-mode . prettier-js-mode))

;; 2023-09-03 Trying eglot instead of TIDE for now.
;; (use-package tide :ensure t)

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
(use-package magit :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-dispatch)
         ("C-c f" . magit-file-dispatch)))

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
(use-package xterm-color :ensure t
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

(use-package markdown-mode :ensure t
  :mode "\\.md\\'"
  :config (add-hook 'markdown-mode-hook 'my-markdown-mode-hook))

(use-package adaptive-wrap :ensure t
  :commands adaptive-wrap-prefix-mode
  :init
  (add-hook 'markdown-mode-hook 'adaptive-wrap-prefix-mode)
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))


;; =================================================================
;; Rust
;; =================================================================
(use-package rust-mode :ensure t
  :mode "\\.rs\\'"
  :commands rust-format-buffer ; 2023-10-26 Make this command available even when the mode isn't used.
  :config
  (setq rust-format-on-save t))

;; 2023-10-18 We're using eglot now, I presume we don't want this.
;; (use-package flycheck-rust :ensure t
;;   :hook (rust-mode . flycheck-rust-setup))

(defun my/rust-ts-mode-hook ()
  "Extra stuff for my rust mode."
  ;; 2023-10-26 Use rust-format-buffer instead of eglot-format-buffer, I was
  ;; having a hard time and it looked like eglot-format-buffer was screwing
  ;; things up.
  (add-hook 'before-save-hook 'rust-format-buffer 0 t))

(add-hook 'rust-ts-mode-hook 'my/rust-ts-mode-hook)

;; =================================================================
;; Clojure
;; =================================================================
(use-package clojure-mode :ensure t
    :mode (("\\.clj\\'" . clojure-mode)
           ("\\.edn\\'" . clojure-mode))
    :config
    (use-package cider :ensure t
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
  (setq lua-indent-level 1)
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

(use-package howm :ensure t
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
(use-package protobuf-mode :ensure t)

;; =================================================================
;; Deadgrep for searching
;; =================================================================
(use-package deadgrep :ensure t
  :bind ("C-c d" . deadgrep))

;; =================================================================
;; Terraform
;; =================================================================
(use-package terraform-mode :ensure t
  :mode "\\.tf(vars)?\\'"
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; =================================================================
;; Earthly
;; =================================================================
(use-package earthfile-mode :ensure t
  :mode ("\\.earth\\'" "Earthfile\\'"))

;; =================================================================
;; Java
;; =================================================================
(use-package eglot-java :ensure t
  :after (eglot)
  :hook
  (java-mode  . eglot-java-mode))

;; =================================================================
;; SQL?
;; =================================================================
(use-package sql-indent :ensure t)

;; =================================================================
;; Swift
;; =================================================================
(use-package swift-mode :ensure t
  :mode "\\.swift\\(interface\\)?\\'")

;; =================================================================
;; Scala
;; =================================================================
(use-package scala-ts-mode :ensure t
  :mode "\\.scala\\'"
  :interpreter ("scala" . scala-mode)
  )

(defun my/metals-import-project ()
  "Tell metals to import the current project. Again.

Do this when you edit your project view."
  (interactive)
  (jsonrpc-async-request (eglot--current-server-or-lose)
                         :workspace/executeCommand
                         '(:command "build-import" :arguments ())))

;; =================================================================
;; Jsonnet
;; =================================================================
(use-package jsonnet-mode :ensure t)

;; =================================================================
;; Fish shell
;; =================================================================
(use-package fish-mode :ensure)

;; =================================================================
;; AI Shit
;; =================================================================
(defun claude-get-api-key ()
  "Get Claude API key from ~/.config/io.datasette.llm/keys.json file."
  (let* ((keys-file (expand-file-name "~/.config/io.datasette.llm/keys.json"))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (if (file-exists-p keys-file)
        (let* ((json-data (with-temp-buffer
                            (insert-file-contents keys-file)
                            (json-read-from-string (buffer-string))))
               (claude-key (gethash "claude" json-data)))
          (if claude-key
              claude-key
            (error "Claude API key not found in keys.json")))
      (error "Key file keys.json file not found at %s" keys-file))))

(defconst my/gptel-databricks-path
  (expand-file-name "~/universe/experimental/john.doty/gptel-databricks/")
  "The path to the databricks gptel backend.")

(defconst my/has-gptel-databricks
  (file-exists-p my/gptel-databricks-path)
  "Whether or not we have the databricks gptel backend.")

(defconst my/gptel-backend
  (if my/has-gptel-databricks
      (progn
        (add-to-list 'load-path (directory-file-name my/gptel-databricks-path))
        (require 'gptel-databricks)
        (gptel-make-databricks "Databricks" :stream t))
    (gptel-make-anthropic "Claude"
      :stream t
      :key #'claude-get-api-key
      :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                                  :max_tokens 4096)))
  "The right backend based on my environment.")

(defconst my/gptel-model
  (if my/has-gptel-databricks
      'claude-3-7-sonnet-internal-tools
    'claude-3-7-sonnet-20250219)
  "Which model do we want by default?")

(use-package gptel :ensure
  :bind (:map gptel-mode-map
              ("C-c C-g" . gptel-menu)
              ("C-c C-t" . gptel-tools))

  :commands (gptel gptel-menu gptel-tools)
  :config

  (setq
   gptel-model my/gptel-model ;  "claude-3-opus-20240229" also available
   gptel-backend my/gptel-backend)
  (if (file-exists-p "~/llm-hints.md")
      (gptel-add-file (expand-file-name "~/llm-hints.md")))
  (require 'doty-tools)
  (require 'doty-tools-buffer-map))


;; =================================================================
;; Debugging
;; =================================================================
(use-package dap-mode :ensure
  :commands dap-debug
  :config
  (require 'dap-netcore))

;; =================================================================
;; WGSL
;; =================================================================
(use-package wgsl-mode :ensure
  :mode "\\.wgsl\\'")

;;; init.el ends here
