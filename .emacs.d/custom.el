;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--line-number" "--smart-case" "--column" "--"))
 '(ag-executable "rg")
 '(blink-matching-paren-dont-ignore-comments t)
 '(c-basic-offset 2)
 '(c-echo-syntactic-information-p t)
 '(c-indent-level 4)
 '(c-label-minimum-indentation 0)
 '(c-label-offset -4)
 '(clojure-build-tool-files
   '("project.clj" "build.boot" "build.gradle" "build.gradle.kts" "deps.edn"
     "shadow-cljs.edn" "TARGETS"))
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(company-backends
   '(company-bbdb company-nxml company-css company-capf
                  (company-dabbrev-code company-keywords)))
 '(company-minimum-prefix-length 1)
 '(css-indent-offset 2)
 '(custom-safe-themes t)
 '(fast-lock-cache-directories '("~/flc-cache"))
 '(fast-lock-minimum-size nil)
 '(fill-column 77)
 '(find-file-run-dired t)
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-gcc-language-standard "c++11")
 '(flycheck-javascript-flow-args nil)
 '(flycheck-python-flake8-executable "python3")
 '(font-lock-global-modes t)
 '(font-lock-maximum-size nil)
 '(font-lock-support-mode 'jit-lock-mode t)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(ido-enable-flex-matching t)
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-strict-trailing-comma-warning nil)
 '(make-backup-files nil)
 '(monky-process-type 'cmdserver)
 '(mouse-buffer-menu-mode-mult 0)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
 '(nrepl-use-ssh-fallback-for-remote-hosts t)
 '(omnisharp-server-executable-path nil)
 '(org-adapt-indentation t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-export-with-toc nil)
 '(org-hide-leading-stars t)
 '(org-log-done t)
 '(org-odd-levels-only t)
 '(org-todo-keywords '((sequence "TODO" "|" "DONE" "ABANDONED" "DEFERRED")))
 '(package-check-signature nil)
 '(package-selected-packages
   '(adaptive-wrap add-node-modules-path ag async auto-complete
                   auto-complete-nxml bazel blacken cider clang-format
                   clipetty clojure-mode color-theme-monokai
                   color-theme-sanityinc-solarized
                   color-theme-sanityinc-tomorrow company company-jedi
                   company-lsp compat cquery dap-mode dap-netcore
                   dash-functional deadgrep dockerfile-mode doom-themes
                   earthfile-mode editorconfig eglot eglot-java elm-mode esup
                   exec-path-from-shell filladapt fish-mode flycheck
                   flycheck-elm flycheck-rust flymake flyspell fsharp-mode
                   geiser gnu-elpa-keyring-update go-autocomplete go-mode
                   gptel graphviz-dot-mode hack-mode haxe-mode howm ink-mode
                   js2-mode js2-refactor json-mode jsonnet-mode lsp-hack
                   lsp-pyright lsp-ui lua-mode magit markdown-mode merlin
                   mocha modus-themes monky monokai-theme multi-term
                   mustache-mode nyan-mode paredit popup prettier-js
                   projectile protobuf-mode python-mode request rjsx-mode
                   ruby-mode rust-mode scala-ts-mode simple-httpd sql-indent
                   swift-mode switch-window terraform-mode thrift tide
                   tree-sitter tss tuareg typescript-mode use-package uuidgen
                   vterm web-mode wgrep wgsl-mode xref-js2 xterm-color
                   yaml-mode zig-mode))
 '(reb-re-syntax 'string)
 '(rmail-mail-new-frame t)
 '(safe-local-variable-directories '("/home/john.doty/universe/"))
 '(safe-local-variable-values
   '((docker-image-name . "onceandfuture")
     (eval ignore-errors
           "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil (delete-trailing-whitespace) nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0) (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail) (require-final-newline . t)))
 '(scala-indent:use-javadoc-style t)
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(sd-user-email "johndoty@microsoft.com")
 '(sd-verbose nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tramp-completion-reread-directory-timeout nil)
 '(tramp-default-method "sshx")
 '(tramp-use-connection-share nil)
 '(tramp-use-ssh-controlmaster-options nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(typescript-indent-level 2)
 '(use-dialog-box nil)
 '(warning-suppress-types '((emacs) ((unlock-file))))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(which-func-mode-global t nil (which-func))
 '(widget-editable-list-gui t)
 '(x-stretch-cursor nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:slant italic)))))
