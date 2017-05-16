(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments (quote ("--line-number" "--smart-case" "--column" "--")))
 '(ag-executable "rg")
 '(blink-matching-paren-dont-ignore-comments t)
 '(c-basic-offset 2)
 '(c-echo-syntactic-information-p t)
 '(c-indent-level 4)
 '(c-label-minimum-indentation 0)
 '(c-label-offset -4)
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(css-indent-offset 2)
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
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-strict-trailing-comma-warning nil)
 '(make-backup-files nil)
 '(mouse-buffer-menu-mode-mult 0)
 '(org-export-with-toc nil)
 '(org-hide-leading-stars t)
 '(org-odd-levels-only t)
 '(org-todo-keywords
   (quote
    ((sequence "TODO" "|" "DONE" "ABANDONED" "DEFERRED"))))
 '(package-selected-packages
   (quote
    (multi-term xterm-color thrift markdown-mode tuareg merlin ag use-package flycheck dockerfile-mode js2-mode web-mode ahg zencoding-mode tss switch-window python-mode paredit monky magit lua-mode go-mode go-autocomplete exec-path-from-shell csharp-mode color-theme-solarized color-theme-monokai auto-complete auto-complete-nxml flymake flyspell json-mode popup ruby-mode)))
 '(quip-api-key
   "UU9RQU1Ba0pjR08=|1517609175|FmwD/EJT5K//+ntQqzVopKmzq/juUmBQsS2hsNi8MeQ=")
 '(reb-re-syntax (quote string))
 '(rmail-mail-new-frame t)
 '(safe-local-variable-values
   (quote
    ((docker-image-name . "onceandfuture")
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t))))
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
