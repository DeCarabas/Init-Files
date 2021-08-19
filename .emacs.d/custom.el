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
   '("project.clj" "build.boot" "build.gradle" "build.gradle.kts" "deps.edn" "shadow-cljs.edn" "TARGETS"))
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(company-backends
   '(company-tasks company-reviewers company-bbdb company-nxml company-css company-capf
                   (company-dabbrev-code company-keywords)))
 '(company-minimum-prefix-length 1)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "3dbb18bf06f41012d4525e6c64c392d6cfef06a2f8fe1bf7b565c4e020255466" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(fast-lock-cache-directories '("~/flc-cache"))
 '(fast-lock-minimum-size nil)
 '(fill-column 77)
 '(find-file-run-dired t)
 '(flycheck-gcc-language-standard "c++11")
 '(flycheck-javascript-flow-args nil)
 '(flycheck-python-flake8-executable "python3")
 '(font-lock-global-modes t)
 '(font-lock-maximum-size nil)
 '(font-lock-support-mode 'jit-lock-mode)
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
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-export-with-toc nil)
 '(org-hide-leading-stars t)
 '(org-log-done t)
 '(org-odd-levels-only t)
 '(org-todo-keywords '((sequence "TODO" "|" "DONE" "ABANDONED" "DEFERRED")))
 '(package-selected-packages
   '(flycheck-rust eglot ink-mode prettier-js zig-mode modus-operandi-theme esup gnu-elpa-keyring-update lsp-hack hack-mode rust-mode filladapt lsp-ui yaml-mode wgrep fsharp-mode company-lsp cquery mustache-mode clang-format projectile dash-functional mocha add-node-modules-path rjsx-mode xref-js2 js2-refactor company omnisharp geiser cider clojure-mode graphviz-dot-mode multi-term xterm-color thrift markdown-mode tuareg merlin ag use-package flycheck dockerfile-mode js2-mode web-mode tss switch-window python-mode paredit magit lua-mode go-mode go-autocomplete exec-path-from-shell csharp-mode color-theme-monokai auto-complete auto-complete-nxml flymake flyspell json-mode popup ruby-mode company-jedi tide elm-mode monky))
 '(reb-re-syntax 'string)
 '(rmail-mail-new-frame t)
 '(safe-local-variable-values
   '((docker-image-name . "onceandfuture")
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace)
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)))
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(sd-user-email "johndoty@microsoft.com")
 '(sd-verbose nil)
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tramp-completion-reread-directory-timeout nil)
 '(tramp-default-method "sshx")
 '(tramp-use-ssh-controlmaster-options nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(typescript-indent-level 2)
 '(use-dialog-box nil)
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
 )
