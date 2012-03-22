;;; sd.el --- Simple SourceDepot-Emacs Integration
;;

;;; Commentary:
;;
;;    Applied the GNU G.P.L. to this file - rv 3/27/1997

;;    Programs for  Emacs <-> SourceDepot Integration.
;;    Copyright (C) 1999-2001  Michael Hotchin
;;    Copyright (C) 1997-1999  Rajesh Vaidheeswarran
;;    Copyright (C) 1996, 1997 Eric Promislow
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;    If you have any problems to report, or suggestions, please send them
;;    to mhotchin@microsoft.com


;;
;; WARNING:
;; --------
;;
;;    % sd edit foo.c
;;    ... make changes to foo.c in emacs
;;    % sd submit
;;     ... keep the writable copy of foo.c in emacs.  Start making changes
;;     to it.  Discover that you can't save it.  If you do M-x:sd-edit,
;;     you'll lose your changes.  You need to do a 'sd edit' at the
;;     command-line.
;;

;; Original Functions:   (Contributed by Eric Promislow)
;; sd-exec-sd            (not exported)
;; sd-buffer-action      (not exported)
;; sd-edit
;; sd-revert
;; sd-diff


;; NOTES:
;; ------
;;
;; It is best if you take this file and byte compile it. To do that, you
;; need to do the following:
;;
;; % emacs -batch -f batch-byte-compile /full/path/to/file/sd.el
;;
;; This creates a binary file sd.elc in the path. Add the path to your
;; load-path variable in .emacs like this:
;;
;; (setq load-path (cons "/full/path/to/file" load-path))
;;
;; Then add the library like this:
;;
;; (load-library "sd")
;;

;;
;; Under Win32, you will need to have at least the following set up before
;; this will work:
;;
;;  - The environment variable SDPORT *must* be set.
;;  - After the library is loaded, you may need to call sd-set-sd-executable,
;;    something like this in your startup .el file:
;;       (load-library "~/sd.el")
;;       (sd-set-sd-executable "c:/scc/sd.exe")
;;    We will look along the path for it.
;;  - If you don't set SDCLIENT at all, on NT we default to %COMPUTERNAME%, which
;;    seems to be what SD.EXE uses.
;;


;;; Code:

;; We need to remap C-x C-q to sd-toggle-read-only, so, make sure that we
;; load vc first.. or else, when vc gets autoloaded, it will remap C-x C-q
;; to vc-toggle-read-only.
(require 'vc)
(require 'cl)
(require 'executable)

(defvar sd-emacs-version "8.5.1" "The Current SD-Emacs Integration Revision.")

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(defvar sd-running-emacs nil
  "If the current Emacs is not XEmacs, then, this is non-nil.")
(defvar sd-running-xemacs nil
  "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (setq sd-running-xemacs t)
  (setq sd-running-emacs t))

(defvar sd-emacs-maintainer "Michael Hotchin <mhotchin@microsoft.com>"
  "The maintainer of the emacs-sd integration. Used for bug reports.")

(defvar sd-web-page "http://www.dsmit.com/p4" "The home of p4.el, the basis of SD.EL.")

(eval-and-compile
  (if (< (string-to-int emacs-version) 20)
      (progn
    (defmacro defgroup (sym memb doc &rest args)
      t)
    (defmacro defcustom (sym val doc &rest args)
      `(defvar ,sym ,val ,doc)))))

(defgroup sd nil  "SourceDepot VC System."  :group 'tools)

;; This can be set to wherever 'sd' lies using sd-set-sd-executable
(defcustom sd-executable
  (let ((lst (list "/usr/swlocal/bin/sd"
           "/usr/local/bin/sd"
           "/usr/bin/sd"
           "/bin/sd"))
    (sdex (if (memq system-type '(ms-dos windows-nt)) (executable-find "sd.exe") nil)))
    (while (and lst (not sdex))
      (if (file-executable-p (car lst))
      (setq sdex (car lst)))
      (setq lst (cdr lst)))
    sdex)
  "This is the sd executable.
To set this, use the function  `sd-set-sd-executable' or `customize'"
  :type 'string
  :group 'sd)

;; This is a string with default arguments to pass to "sd diff",
;; "sd diff2", "sd describe", etc.
(defcustom sd-default-diff-options "-du"
  "Type of sd diff output to be displayed. \(regular or context or
unified.\)"
  :type 'string
  :group 'sd)

;; Set this variable to nil to turn off colorized diff buffers.
(defcustom sd-colorized-diffs t
  "Set this to nil to disable colorized diffs."
  :type 'boolean
  :group 'sd)

;; Set whether SDCONFIG should be used exclusively for VC checking
(defcustom sd-use-sdconfig-exclusively nil
  "Whether SD mode should use SDCONFIG exclusively to check whether a file
is under SD version control. If set to nil, `sd-check-mode' is always
called; otherwise, it checks to see if the file named by SDCONFIG exists in
this or a parent directory, and if so, only then runs sd-check-mode.

This provides for a much faster `sd-find-file-hook'."
  :type 'boolean
  :group 'sd)

;; Set the null device
(defcustom sd-null-device
  (if (memq system-type '(ms-dos windows-nt)) "NUL" "/dev/null")
  "Filesystem null device."
  :type 'string
  :group 'sd)

;; Auto-refresh?
(defcustom sd-auto-refresh nil
  "Set this to automatically refresh sd submitted files in buffers."
  :type 'boolean
  :group 'sd)

;; Check for empty diffs at submit time
(defcustom sd-check-empty-diffs t
  "Set this to check for files with empty diffs before submitting."
  :type 'boolean
  :group 'sd)

(defcustom sd-verbose t
  "When set, sd will pop up the output buffer with the result of the
command."
  :type 'boolean
  :group 'sd)

(defcustom sd-mode-hook nil
  "Hook run by `sd-mode'."
  :type 'sexp
  :group 'sd)


(defvar sd-output-buffer-name "*SD Output*" "SD Output Buffer.")
(defvar sd-global-config (getenv "SDCONFIG") "SD Config to use.")

(defvar sd-global-clt (if sd-global-config nil
			(if (memq system-type '(windows-nt))
			    (or (getenv "SDCLIENT")
				(getenv "COMPUTERNAME"))
			  (getenv "SDCLIENT")))
  "The SD Client to use.")

;; Set this variable in .emacs if you want sd-set-client-name to complete
;; your client name for you.
(defvar sd-my-clients nil
  "This variable holds the alist of sd clients that the function
`sd-set-client-name' can complete on.

Set this variable *only* if you don't want SD to complete on all the clients
in the SD server.

This is a alist, and should be set using the function
`sd-set-my-clients'. For example, in your .emacs:

\(load-library \"sd\"\)
\(sd-set-my-clients \'(client1 client2 client3)\)")

;; Set this variable in .emacs if you want to alter the completion
;; behavior of sd-set-client-name.

(defvar sd-send-mail-function 'sd-do-send-mail "\
Function that SD uses to send mail on notify - see `sd-notify'.
Default value is `sd-do-send-mail'.
`send-mail-function' is a good choice too, set it like this in your startup file:
(setq sd-send-mail-function send-mail-function)
")

(defcustom sd-strict-complete t
  "Set this variable in .emacs \(or using `customize'\) if you want to alter
the completion behavior of `sd-set-client-name'.
"
  :type 'boolean
  :group 'sd)

(defvar sd-global-server-port  (if sd-global-config
                   (progn
                     (setenv "SDPORT" nil)
                     nil)
                 (getenv "SDPORT"))
  "The SD Server/Port in use.")
(if (and (eq sd-global-server-port nil) (eq sd-global-config nil))
    (progn
      (setq sd-global-server-port "sd:1666") ;; Default SD port.
      (setenv "SDPORT" sd-global-server-port)))

(defvar sd-old-notify-list (getenv "SDNOTIFY") "The SD Notify List.")
(defvar sd-notify-list (getenv "SDNOTIFY") "The SD Notify List.")

(defcustom sd-sendmail-program (if (boundp 'sendmail-program)
                   sendmail-program
                 nil)
  "The sendmail program. To set this use `sd-set-sendmail-program' or
`customize'."
  :type 'string
  :group 'sd)

(defcustom sd-user-email (if (boundp 'user-mail-address)
                 user-mail-address nil)
  "The e-mail address of the current user. This is used with the
notification system, and must be set if notification should take place. To
set this use `sd-set-user-email' or `customize'."
  :type 'string
  :group 'sd)

(defcustom sd-notify nil
  "If this is t then the users in the notification list set by
`sd-set-notify-list' will get a notification of any SD change submitted from
within emacs."
  :type 'boolean
  :group 'sd)

;; This can be set with sd-toggle-vc-mode
(defcustom sd-do-find-file t
  "If non-nil, the `sd-find-file-hook' will run when opening files."
  :type 'boolean
  :group 'sd)

;; Now add a hook to find-file-hooks
(add-hook 'find-file-hooks 'sd-find-file-hook)
;; .. and one to kill-buffer-hook
(add-hook 'kill-buffer-hook 'sd-kill-buffer-hook)

;; Tell Emacs about this new kind of minor mode
(defvar sd-mode nil "Is this file under sd?")
(make-variable-buffer-local 'sd-mode)
(put 'sd-mode 'permanent-local t)

(defvar sd-local-client nil "Buffer Local value of the sd client name.")
(make-variable-buffer-local 'sd-local-client)
(put 'sd-local-client 'permanent-local t)
(set-default 'sd-local-client nil)

(defvar sd-local-server-port nil "Buffer Local value of the sd server/port.")
(make-variable-buffer-local 'sd-local-server-port)
(put 'sd-local-server-port 'permanent-local t)
(set-default 'sd-local-server-port nil)

(if (not (assoc 'sd-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(sd-mode sd-mode)
                 minor-mode-alist)))

(defvar sd-minor-mode nil
  "The minor mode for editing sd asynchronous command buffers.")
(make-variable-buffer-local 'sd-minor-mode)
(defvar sd-minor-map (make-keymap) "Keymap for sd minor mode")
(fset 'sd-minor-map sd-minor-map)

(or (assoc 'sd-minor-mode minor-mode-alist)
    (setq minor-mode-alist
      (cons '(sd-minor-mode " SD") minor-mode-alist)))

(or (assoc 'sd-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
      (cons '(sd-minor-mode . sd-minor-map) minor-mode-map-alist)))

(defvar sd-current-command nil)
(make-variable-buffer-local 'sd-current-command)
(put 'sd-current-command 'permanent-local t)
(set-default 'sd-current-command nil)

(defvar sd-current-args nil)
(put 'sd-current-args 'permanent-local t)
(set-default 'sd-current-args nil)

;; To check if the current buffer's modeline and menu need to be altered

(defvar sd-vc-check nil)
(make-variable-buffer-local 'sd-vc-check)
(put 'sd-vc-check 'permanent-local t)
(set-default 'sd-vc-check nil)

(defvar sd-set-client-hooks nil
  "List of functions to be called after a sd client is changed.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(if sd-running-emacs (require 'timer))

(defvar sd-timer nil "Timer object that will be set to cleanup the caches
periodically.")

(defcustom sd-cleanup-time 600 "seconds after which `sd-cache-cleanup' will
check for dirty caches."
  :type 'integer
  :group 'sd)

(defcustom sd-cleanup-cache t "`sd-cache-cleanup' will cleanup the
branches/clients/dirs/labels caches once in a while if this is non-nil."
  :type 'boolean
  :group 'sd)

(defvar sd-all-buffer-files nil "An associated list of all buffers and
theirs files under sd version control. This is to enable autorefreshing of
sd submitted files being visited by the buffer.")

(defvar sd-file-refresh-timer nil "Timer object that will be set to refresh
the files in Emacs buffers that have been modified by a `sd-submit'.")

(defcustom sd-file-refresh-timer-time 60 "seconds after which
`sd-file-refresh' will check for modified files in Emacs buffers."
  :type 'integer
  :group 'sd)

(defvar sd-async-command-hook nil
  "This hook is run after an async buffer has been set up by
`sd-async-process-command'")

(defvar sd-window-config-stack nil
  "Stack of saved window configurations.")

(defcustom sd-window-config-stack-size 20 "Maximum stack size
for saved window configurations."
  :type 'integer
  :group 'sd)

(defvar sd-basic-map
  (let ((map (make-sparse-keymap)))
    (cond (sd-running-xemacs
       (define-key map [button2] 'sd-buffer-mouse-clicked))
      (sd-running-emacs
       (define-key map [mouse-2] 'sd-buffer-mouse-clicked)))
    (define-key map [return]  'sd-buffer-commands)
    (define-key map "\r" 'sd-buffer-commands)
    (define-key map "q"  'sd-quit-current-buffer)
    (define-key map "k"  'sd-scroll-down-1-line)
    (define-key map "j"  'sd-scroll-up-1-line)
    (define-key map "b"  'sd-scroll-down-1-window)
    (define-key map [backspace] 'sd-scroll-down-1-window)
    (define-key map " "  'sd-scroll-up-1-window)
    (define-key map "<"  'sd-top-of-buffer)
    (define-key map ">"  'sd-bottom-of-buffer)
    (define-key map "="  'sd-delete-other-windows)
    map))

(defvar sd-filelog-map
  (let ((map (make-sparse-keymap)))
    (cond (sd-running-xemacs
       (define-key map [button2] 'sd-buffer-mouse-clicked))
      (sd-running-emacs
       (define-key map [mouse-2] 'sd-buffer-mouse-clicked)))
    (define-key map "d"  'sd-buffer-commands)
    (define-key map [return]  'sd-buffer-commands)
    (define-key map "\r" 'sd-buffer-commands)
    (define-key map "q"  'sd-quit-current-buffer)
    (define-key map "f"  'sd-find-file-other-window)
    (define-key map "s"  'sd-filelog-short-format)
    (define-key map "l"  'sd-filelog-long-format)
    (define-key map "k"  'sd-scroll-down-1-line-other-w)
    (define-key map "j"  'sd-scroll-up-1-line-other-w)
    (define-key map "b"  'sd-scroll-down-1-window-other-w)
    (define-key map [backspace] 'sd-scroll-down-1-window-other-w)
    (define-key map " "  'sd-scroll-up-1-window-other-w)
    (define-key map "<"  'sd-top-of-buffer-other-w)
    (define-key map ">"  'sd-bottom-of-buffer-other-w)
    (define-key map "="  'sd-delete-other-windows)
    (define-key map "n"  'sd-goto-next-change)
    (define-key map "p"  'sd-goto-prev-change)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting filelog properties.")

(defun sd-make-derived-map (base-map)
  (let (map)
    (cond (sd-running-xemacs
       (setq map (make-sparse-keymap))
       (set-keymap-parents map (list base-map)))
      (sd-running-emacs
       (setq map (cons 'keymap base-map))))
    map))

(defvar sd-opened-map
  (let ((map (sd-make-derived-map sd-basic-map)))
    (define-key map "n"  'sd-next-depot-file)
    (define-key map "p"  'sd-prev-depot-file)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting opened files.")

(defvar sd-diff-map
  (let ((map (sd-make-derived-map sd-basic-map)))
    (define-key map "n"  'sd-goto-next-diff)
    (define-key map "p"  'sd-goto-prev-diff)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "d"  'sd-next-depot-diff)
    (define-key map "u"  'sd-prev-depot-diff)
    map))

(defvar sd-print-rev-map
  (let ((map (sd-make-derived-map sd-basic-map)))
    (define-key map "n"  'sd-next-change-rev-line)
    (define-key map "p"  'sd-prev-change-rev-line)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for browsing print-revs buffers.")

;;; All functions start here.

;; A generic function that we use to execute sd commands
(defun sd-exec-sd (output-buffer args &optional clear-output-buffer)
  "Internal function called by various sd commands."
  (save-excursion
    (if clear-output-buffer
        (progn
          (set-buffer output-buffer)
          (delete-region (point-min) (point-max))))
    (apply 'call-process (sd-check-sd-executable) sd-null-device
           output-buffer
           nil              ; update display?
           args)
    (sd-menu-add)))

(defun sd-push-window-config ()
  "Push the current window configuration on the `sd-window-config-stack'
stack."
  (interactive)
  (setq sd-window-config-stack
    (cons (current-window-configuration)
          sd-window-config-stack))
  (while (> (length sd-window-config-stack) sd-window-config-stack-size)
    (setq sd-window-config-stack
      (reverse (cdr (reverse sd-window-config-stack))))))

(defun sd-pop-window-config (num)
  "Pop `num' elements from the `sd-window-config-stack' stack and use
the last popped element to restore the window configuration."
  (interactive "p")
  (while (> num 0)
    (if (eq sd-window-config-stack nil)
    (error "window config stack empty"))
    (set-window-configuration (car sd-window-config-stack))
    (setq sd-window-config-stack (cdr sd-window-config-stack))
    (setq num (1- num)))
  (message "window config popped (stack size %d)"
       (length sd-window-config-stack)))

;; We use the noinput version for commands like sd opened, sd get etc.
;; which don't take any input file name.
;;
(defun sd-noinput-buffer-action (cmd do-revert show-output &optional argument)
  "Internal function called by various sd commands."
  (save-excursion
    (if (not (stringp cmd))
        (error "sd-noinput-buffer-action: Command not a string."))
    (save-excursion
      (sd-exec-sd (get-buffer-create sd-output-buffer-name)
                  (if argument
                      (append (list cmd) argument)
                    (list cmd))
                  t))
    (sd-partial-cache-cleanup cmd)
    (if (and do-revert buffer-file-name)
        (revert-buffer t t))
    (if show-output
        (progn
          (if (and
               (eq show-output 's)
               (= (save-excursion
                    (set-buffer sd-output-buffer-name)
                    (count-lines (point-min) (point-max)))
                  1))
              (save-excursion
                (set-buffer sd-output-buffer-name)
                (message (buffer-substring (point-min)
                                           (save-excursion
                                             (goto-char (point-min))
                                             (end-of-line)
                                             (point)))))
            (sd-push-window-config)
            (delete-other-windows)
            (display-buffer sd-output-buffer-name t))))))

;; The sd edit command
(defun sd-edit (show-output)
  "To open the current depot file for edit, type \\[sd-edit].

Open or re-open an existing file for edit.

If file is already open for edit or delete then it is reopened
for edit and moved into the specified change number (or 'default'
change if no change number is given.)

If -t type is given the file is explicitly opened as the specified
file type, which may be text, ltext, xtext, binary, xbinary, ktext
kxtext, symlink, or resource.  Not all types are supported on all
operating systems.  See the Users' Guide for a description of file
types.  If no type is specified, the type is determined automatically
by examination of the file's contents and execution permission bits.

Argument SHOW-OUTPUT  displays the *SD Output* buffer on executing the
command if t."

  (interactive (list sd-verbose))
  (let ((args (if (sd-buffer-file-name-2)
                  (sd-buffer-file-name-2)
                ""))
        (refresh-after nil))
    (if (or current-prefix-arg (string= "" args))
        (progn
          (setq args (sd-make-list-from-string
                      (sd-read-arg-string "sd edit: " (cons args 0))))
          (setq refresh-after t))
      (setq args (list args)))
    (sd-noinput-buffer-action "edit" t (and show-output 's) args)
    (if refresh-after
        (sd-refresh-files-in-buffers)))
  (sd-check-mode)
  (sd-update-opened-list))

;; The sd reopen command
(defun sd-reopen (show-output)
  "To change the type or changelist number of an opened file, type
\\[sd-reopen].

Reopen takes an already opened file and moves it to a new changelist
or changes its type (text, ltext, xtext, binary, xbinary, ktext,
kxtext, symlink, or resource).

Argument SHOW-OUTPUT  displays the *SD Output* buffer on executing the
command if t."

  (interactive (list sd-verbose))
  (let ((args (if buffer-file-name
          buffer-file-name
        "")))
    (setq args (sd-make-list-from-string
        (sd-read-arg-string "sd reopen: " (cons args 0))))
    (sd-noinput-buffer-action "reopen" t (and show-output 's) args))
  (sd-check-mode)
  (sd-update-opened-list))

;; The sd revert command
(defun sd-revert (show-output)
  "Revert all change in the current file.

Argument SHOW-OUTPUT  displays the *SD Output* buffer on executing the
command if t."
  (interactive (list sd-verbose))
  (let ((args (list (buffer-file-name)))
    (refresh-after nil))
    (if (or current-prefix-arg (not buffer-file-name))
    (progn
      (setq args (sd-make-list-from-string
              (sd-read-arg-string "sd revert: "
                      (sd-buffer-file-name-2))))
      (setq refresh-after t)))
    (if (yes-or-no-p "Really revert changes? ")
    (progn
      (sd-noinput-buffer-action "revert" t (and show-output 's) args)
      (if refresh-after
          (sd-refresh-files-in-buffers)))))
  (sd-check-mode)
  (sd-update-opened-list))

;; The sd lock command
(defun sd-lock ()
  "Lock an opened file against changelist submission."
  (interactive)
  (let ((args (list (sd-buffer-file-name-2))))
    (if (or current-prefix-arg (not (sd-buffer-file-name-2)))
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd lock: "
                    (sd-buffer-file-name-2)))))
    (sd-noinput-buffer-action "lock" t 's args)
    (sd-update-opened-list)))

;; The sd unlock command
(defun sd-unlock ()
  "Release a locked file but leave open."
  (interactive)
  (let ((args (list (sd-buffer-file-name-2))))
    (if (or current-prefix-arg (not (sd-buffer-file-name-2)))
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd unlock: "
                    (sd-buffer-file-name-2)))))
    (sd-noinput-buffer-action "unlock" t 's args)
    (sd-update-opened-list)))

;; The sd diff command
(defun sd-diff ()
  "To diff the current file and topmost depot version, type \\[sd-diff].

Run diff (on the client) of a client file against the corresponding
revision in the depot.  The file is only compared if the file is
opened for edit or the revision provided with the file argument is
not the same as the revision had by the client.

If no file argument is given, diff all open files.
This can be used to view pending changes.

The -f flag forces a diff for every file, regardless of whether
they are opened or if the client has the named revision.
This can be used to verify the client contents.

The -s flag outputs reduces the output of diff to the names of files
satisfying the following criteria:

    -sa Opened files that are different than the revision
        in the depot, or missing.

    -sd Unopened files that are missing on the client.

    -se Unopened files that are different than the revision
        in the depot.

    -sr Opened files that are the same as the revision in the
        depot."

  (interactive)
  (let ((args (sd-make-list-from-string (concat sd-default-diff-options " "
                        (sd-buffer-file-name-2)))))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd diff: " sd-default-diff-options))))
    (sd-noinput-buffer-action "diff" nil 's args)
    (sd-activate-diff-buffer "*SD diff*")))

;; The sd diff2 command
(defun sd-diff2 (version1 version2)
  "Display diff of two depot files.

When visiting a depot file, type \\[sd-diff2] and
enter the versions.

Example:  (find-file \"/us/rv/tag/main/Construct\")
      \\[sd-diff2] <RET>
      First Version to diff: 113
      Second Version to diff: 100

      Will produce the diff between the two versions in the
      output buffer *SD Output*

Run diff (on the server) of two files in the depot.  Both files
may optionally include a revision specification; the default is
to compare the head revision.  Wildcards may be used, but they
must match between file1 and file2 and they must be escaped from
the user's shell by quoting or with backslashes (\).

The -d flag allows you to pass flags to the underlying diff
program.

-dc passes the -c (context diff) flag.

-du passes the -u (unified diff) flag.

-dn passes the the -n (rcs diff) flag.

Other diff flags are not supported.

Argument VERSION1 First Version to use.
Argument VERSION2 Second Version to use."
  (interactive
   (list (sd-read-arg-string "First Depot File or Version# to diff: ")
     (sd-read-arg-string "Second Depot File or Version# to diff: ")))
  (let ((sd-diff-version1 sd-vc-check)
    (sd-diff-version2 sd-vc-check)
    (sd-diff-options (sd-make-list-from-string sd-default-diff-options)))
    (if current-prefix-arg
    (setq sd-diff-options (sd-make-list-from-string
                   (sd-read-arg-string "Optional Args: "
                           sd-default-diff-options))))
    ;; try to find out if this is a revision number, or a depot file
    (cond ((string-match "^[0-9]+$" version1)
       ;; this is a revision of the current file
       (setq sd-diff-version1 (concat (sd-buffer-file-name-2)
                      "#" version1)))
      ((string= "" version1)
       (setq sd-diff-version1 (sd-buffer-file-name-2)))
      (t
       ;; this is default.. any random file or a depot file
       (setq sd-diff-version1 version1)))
    (cond ((string-match "^[0-9]+$" version2)
       ;; this is a revision of the current file
       (setq sd-diff-version2 (concat (sd-buffer-file-name-2)
                      "#" version2)))
      ((string= "" version2)
       (setq sd-diff-version2 (sd-buffer-file-name-2)))
      (t
       ;; this is default.. any random file or a depot file
       (setq sd-diff-version2 version2)))
    (sd-noinput-buffer-action "diff2" nil t
                  (append sd-diff-options
                      (list sd-diff-version1
                        sd-diff-version2)))
    (sd-activate-diff-buffer "*SD diff2*")))


;; sd-ediff for all those who diff using ediff

(defun sd-ediff ()
  "Use ediff to compare file with its original client version."
  (interactive)
  (require 'ediff)
  (sd-noinput-buffer-action "print" nil nil
                (list "-q"
                  (concat (buffer-file-name) "#have")))
  (let ((local (current-buffer))
    (depot (get-buffer-create sd-output-buffer-name))
	(local-buffer-mode major-mode))
    (set-buffer depot)
    (funcall local-buffer-mode)
    (rename-buffer (concat "SD_have#" (buffer-file-name local)))
    (ediff-buffers local
           depot
           `((lambda ()
               (make-local-variable 'ediff-cleanup-hook)
               (setq ediff-cleanup-hook
                 (cons (lambda ()
                     (kill-buffer ,depot)
                     (sd-menu-add))
                   ediff-cleanup-hook)))))))

;; The sd add command
(defun sd-add ()
  "To add the current file to the depot, type \\[sd-add].

Optional arguments like '-tktext' can be passed as prefix arguments.

Open a new file for adding to the depot.  If the file exists
on the client it is read to determine if it is text or binary.
If it does not exist it is assumed to be text.  The file must
either not exist in the depot, or it must be deleted at the
current head revision.  Files may be deleted and re-added arbitrarily.

If the -c flag is given the open files are associated with the
specified pending change number; otherwise the open files are
associated with the current 'default' change.

If file is already open it is moved into the specified pending
change.  It is not permissible to reopen a file for add unless
it was already open for add.

If -t type is given the file is explicitly opened as the specified
file type, which may be text, ltext, xtext, binary, xbinary, ktext
kxtext, symlink, or resource.  Not all types are supported on all
operating systems.  See the Users' Guide for a description of file
types.  If no type is specified, the type is determined automatically
by examination of the file's contents and execution permission bits."

  (interactive)
  (if (not (sd-is-vc))
      (progn
    (let ((args (if buffer-file-name
            buffer-file-name
              "")))
      (if (or current-prefix-arg (string= "" args))
          (setq args (sd-make-list-from-string
              (sd-read-arg-string "sd add: " (cons args 0))))
        (setq args (list args)))
      (sd-noinput-buffer-action "add" nil 's args))
    (sd-check-mode "Add"))
    (message "%s already in depot client %s!" buffer-file-name
         (sd-current-client)))
  (sd-update-opened-list))


;; The sd delete command
(defun sd-delete ()
  "To delete the current file from the depot, type \\[sd-delete].

Opens a file that currently exists in the depot for deletion.
If the file is present on the client it is removed.  If a pending
change number is given with the -c flag the opened file is associated
with that change, otherwise it is associated with the 'default'
pending change.

If file is already open it is reopened for delete and moved into
the specified pending change (or 'default' change if no change
number is given.)

Files that are deleted generally do not appear on the have list."

  (interactive)
  (let ((args (buffer-file-name)))
    (if (or current-prefix-arg (not args))
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd delete: "
                    (sd-buffer-file-name-2))))
      (setq args (list args)))
    (if (yes-or-no-p "Really delete from depot? ")
    (sd-noinput-buffer-action "delete" nil 's args)))
  (sd-check-mode)
  (sd-update-opened-list))

;; The sd filelog command
(defun sd-filelog ()

  "To view a history of the change made to the current file, type
\\[sd-filelog].

List the revision history of the files named, working backwards
from the latest revision to the most recent revision 'added'.
If file is given as a client file, the depot file last gotten is
listed.  The -l flag produces long output with the full text of the
change descriptions."

  (interactive)
  (let ((file-name (sd-buffer-file-name-2)))
    (if (or current-prefix-arg (not file-name))
    (setq file-name (sd-read-arg-string "sd filelog: " file-name)))
    (sd-file-change-log "filelog" file-name)))

(defun sd-set-extent-property (start end property value)
  (cond (sd-running-xemacs
     (set-extent-property (make-extent start end)
                  property value))
    (sd-running-emacs
     (overlay-put (make-overlay start end)
              property value))))

(defun sd-create-active-link (start end prop-list)
  (sd-set-extent-property start end 'face 'bold)
  (sd-set-extent-property start end 'mouse-face 'highlight)
  (while prop-list
    (sd-set-extent-property start end (caar prop-list) (cdar prop-list))
    (setq prop-list (cdr prop-list))))

(defun sd-move-buffer-point-to-top (buf-name)
  (if (get-buffer-window buf-name)
      (save-selected-window
	(select-window (get-buffer-window buf-name))
	(goto-char (point-min)))))

(defun sd-file-change-log (cmd filespec)
  (let ((sd-filelog-buffer (concat "*SD " cmd ": " filespec "*"))
    (sd-cur-rev nil)
    (sd-cur-change nil)
    (sd-cur-action nil)
    (sd-cur-user nil)
    (sd-cur-client nil)
    (sd-filename (sd-make-list-from-string filespec))
    (sd-this-client (sd-current-client))
    (sd-this-server-port (sd-current-server-port)))
    (get-buffer-create sd-output-buffer-name);; We do these two lines
    (kill-buffer sd-output-buffer-name);; to ensure no duplicates
    (sd-noinput-buffer-action cmd nil t (cons "-l" sd-filename))
    (sd-make-depot-list-buffer sd-filelog-buffer)
    (set-buffer sd-filelog-buffer)
    (setq buffer-read-only nil)
    (setq sd-local-client sd-this-client
      sd-local-server-port sd-this-server-port)
    (make-local-variable 'sd-fname)
    (setq sd-fname (if (equal cmd "filelog")
               (car sd-filename)
             nil))
    (goto-char (point-min))
    (while (re-search-forward (concat
                   "^\\(\\.\\.\\. #\\([0-9]+\\) \\)?change "
                   "\\([0-9]+\\) \\([a-z]+\\)?.*on.*by "
                   "\\([^ @]+\\)@\\([^ \n]+\\).*\n"
                   "\\(\\(\\([ \t].*\\)?\n\\)*\\)") nil t)
      (let ((rev-match 2)
        (ch-match 3)
        (act-match 4)
        (user-match 5)
        (cl-match 6)
        (desc-match 7))
    (setq sd-cur-rev (match-string rev-match))
    (setq sd-cur-change (match-string ch-match))
    (setq sd-cur-action (match-string act-match))
    (setq sd-cur-user (match-string user-match))
    (setq sd-cur-client (match-string cl-match))

    (if (match-beginning rev-match)
        (sd-create-active-link (match-beginning rev-match)
                   (match-end rev-match)
                   (list (cons 'rev sd-cur-rev))))
    (sd-create-active-link (match-beginning ch-match)
                   (match-end ch-match)
                   (list (cons 'change sd-cur-change)))
    (if (match-beginning act-match)
        (sd-create-active-link (match-beginning act-match)
                   (match-end act-match)
                   (list (cons 'action sd-cur-action)
                     (cons 'rev sd-cur-rev))))
    (sd-create-active-link (match-beginning user-match)
                   (match-end user-match)
                   (list (cons 'user sd-cur-user)))
    (sd-create-active-link (match-beginning cl-match)
                   (match-end cl-match)
                   (list (cons 'client sd-cur-client)))
    (sd-set-extent-property (match-beginning desc-match)
                (match-end desc-match)
                'invisible t)))
    (sd-find-change-numbers sd-filelog-buffer (point-min) (point-max))
    (use-local-map sd-filelog-map)
    (setq buffer-invisibility-spec (list))
    (setq buffer-read-only t)
    (sd-move-buffer-point-to-top sd-filelog-buffer)))

;; Scan specified region for references to change numbers
;; and make the change numbers clickable.
(defun sd-find-change-numbers (buffer start end)
  (save-excursion
    (set-buffer buffer)
    (goto-char start)
    (while (re-search-forward "\\(changes?\\|submit\\|sd\\):?[ \t\n]+" end t)
      (while (looking-at
          (concat "\\(#\\|number\\|no\\.\\|\\)[ \t\n]*"
              "\\([0-9]+\\)[, \t\n]*"
              "\\(and/or\\|and\\|or\\|\\)[ \t\n]*"))
    (let ((ch-start (match-beginning 2))
          (ch-end (match-end 2))
          (ch-str (match-string 2))
          (next (match-end 0)))
      (set-text-properties 0 (length ch-str) nil ch-str)
      (sd-create-active-link ch-start ch-end (list (cons 'change ch-str)))
      (goto-char next))))))

;; The sd files command
(defun sd-files ()
  "List files in the depot. Type, \\[sd-files].

Optional args [file ...] are passed as prefix arguments.

List files named or matching wild card specification.  Display shows depot
file name, revision, file type, change action and change number of the
current head revision.  If client file names are given as arguments the view
mapping is used to list the corresponding depot files."

  (interactive)
  (let ((args (sd-buffer-file-name-2)))
    (if (or current-prefix-arg (not args))
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd files: " (sd-buffer-file-name-2))))
      (setq args (list args)))
    (get-buffer-create sd-output-buffer-name);; We do these two lines
    (kill-buffer sd-output-buffer-name);; to ensure no duplicates
    (sd-noinput-buffer-action "files" nil t args)
    (save-excursion
      (set-buffer sd-output-buffer-name)
      (sd-find-change-numbers sd-output-buffer-name (point-min) (point-max)))
    (sd-make-depot-list-buffer
     (concat "*SD Files: (" (sd-current-client) ") " (car args) "*"))))

(make-face 'sd-depot-unmapped-face)
(set-face-foreground 'sd-depot-unmapped-face "grey30")

(make-face 'sd-depot-deleted-face)
(set-face-foreground 'sd-depot-deleted-face "red")

(make-face 'sd-depot-added-face)
(set-face-foreground 'sd-depot-added-face "blue")

;; Take the sd-output-buffer-name buffer, rename it to bufname, and
;; make all depot file names active, so that clicking them opens
;; the corresponding client file.
(defun sd-make-depot-list-buffer (bufname)
  (let (args max files sd-client-root sd-server-version sd-opened-buffer
	     depot-regexp
	     (sd-this-client (sd-current-client)))
    (if sd-this-client
	(progn
	  (set-buffer sd-output-buffer-name)
	  (goto-char (point-min))
	  (setq depot-regexp
		"^\\(\\.\\.\\. [^/\n]*\\|==== \\)?\\(//[a-zA-Z]+/[^ #\n]*\\)")
	  (while (re-search-forward depot-regexp nil t)
	    (setq args (cons (match-string 2) args)))
	  (setq max (point-max))
	  (goto-char max)
	  (setq sd-client-root (sd-get-client-root sd-this-client))
	  (setq sd-server-version (sd-get-server-version))
	  (if (memq system-type '(ms-dos windows-nt))
	      ;; For Windows, since the client root will be terminated with
	      ;; a \ as in c:\ or drive:\foo\bar\, we need to strip the
	      ;; trailing \ .
	      (let ((sd-clt-root-len (length sd-client-root)))
		(setq sd-clt-root-len (1- sd-clt-root-len))
		(setq sd-client-root (substring sd-client-root 0
						sd-clt-root-len))))
	  (setq sd-opened-buffer bufname)
	  (get-buffer-create sd-opened-buffer);; We do these two lines
	  (kill-buffer sd-opened-buffer);; to ensure no duplicates
	  (set-buffer sd-output-buffer-name)
	  (delete-region max (point-max))
	  (insert "\n")
	  ;;
	  ;;  Command line lengths make this problematic - too many filenames,
	  ;;  and the command won't execute!
;;;      (apply 'call-process
;;;         sd-executable nil t nil "where" (reverse args))
	  ;;
	  ;;  Instead, apply the command one filename at a time
	  (progn (let ((filelist (reverse args)))
		   (while (not (eq filelist nil))
		     (call-process (sd-check-sd-executable) nil t nil "where" (car filelist))
		     (setq filelist (cdr filelist)))))
	  (goto-char max)
	  (if (< sd-server-version 98)
	      (progn
		(while (re-search-forward
			(concat "^\\([^ \n]+\\) //" sd-this-client
				"\\(.*\\)$") nil t)
		  (setq files (cons
			       (cons
				(match-string 1)
				(concat sd-client-root (match-string 2)))
			       files))))
	    (progn
	      (while (re-search-forward
		      "^\\([^ \n]+\\) //\\([^ \n]+\\) \\(.*\\)$" nil t)
		(setq files (cons
			     (cons
			      (match-string 1)  (match-string 3)) files)))))
	  (delete-region max (point-max))
	  (goto-char (point-min))
	  (rename-buffer sd-opened-buffer t)
	  (while (re-search-forward depot-regexp nil t)
	    (let ((sd-client-file (cdr (assoc (match-string 2) files)))
		  (sd-depot-file (match-string 2))
		  (start (match-beginning 2))
		  (end (match-end 2)))
	      (if (and sd-client-file
		       (file-readable-p sd-client-file))
		  (sd-set-extent-property start end 'link-client-name
					  sd-client-file)
		(sd-set-extent-property start end 'link-depot-name
					sd-depot-file))
	      (cond ((not sd-client-file)
		     (sd-set-extent-property start end 'face
					     'sd-depot-unmapped-face))
		    ((save-excursion
		       (goto-char end)
		       (looking-at ".* deleted?[ \n]"))
		     (sd-set-extent-property start end 'face
					     'sd-depot-deleted-face))
		    ((save-excursion
		       (goto-char end)
		       (looking-at ".* \\(add\\|branch\\)\\(ed\\)?[ \n]"))
		     (sd-create-active-link start end
					    (list (cons 'face
							'sd-depot-added-face))))
		    (t
		     (sd-create-active-link start end nil)))))
	  (use-local-map sd-opened-map)
	  (setq buffer-read-only t)
	  (sd-move-buffer-point-to-top sd-opened-buffer)))))

;; The sd print command
(defun sd-print ()
  "To print a depot file to a buffer, type \\[sd-print].

Retrieve the contents of a depot file to the client's standard
output.  The client's gotten list is not affected.  If file is
specified as a client file name, the client view is used to
find the corresponding depot file.  The -q flag suppresses the
initial line that displays the file name and revision."

  (interactive)
  (let ((arg-string (sd-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (sd-read-arg-string "sd print: " arg-string)))
    (sd-noinput-buffer-action
     "print" nil t (sd-make-list-from-string arg-string))
    (sd-activate-print-buffer "*SD print*")))

(defun sd-activate-print-buffer (buffer-name)
  (sd-make-depot-list-buffer buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward "^//[a-zA-Z]+/" nil t)
      (let ((link-client-name (get-char-property (match-end 0)
						 'link-client-name))
	    (link-depot-name (get-char-property (match-end 0)
						'link-depot-name))
	    (start (match-beginning 0))
	    (end (point-max)))
	(save-excursion
	  (if (re-search-forward "^//[a-zA-Z]+/" nil t)
	      (setq end (match-beginning 0))))
	(if link-client-name
	    (sd-set-extent-property start end
				    'block-client-name link-client-name))
	(if link-depot-name
	    (sd-set-extent-property start end
				    'block-depot-name link-depot-name))))
;       (let ((fname (get-char-property (match-end 0) 'filename)))
;     (sd-set-extent-property (match-beginning 0) (point-max)
;                 'local-fname fname)))
    (setq buffer-read-only t)))


(defun sd-print-with-rev-history ()
  "To Print a depot file with revision history to a buffer,
type \\[sd-print-with-rev-history]"
  (interactive)
  (let ((arg-string (sd-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (sd-read-arg-string "sd print-revs: " arg-string)))
    (sd-print-with-rev-history-int arg-string)))

(defun sd-print-with-rev-history-int (file-spec)
  (let ((file-name file-spec)
	(buffer (get-buffer-create sd-output-buffer-name))
	change head-rev fullname headseen ch-alist)
    (if (string-match "\\(.*\\)@\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq change (string-to-int (match-string 2 file-spec)))))
    (if (string-match "\\(.*\\)#\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq head-rev (string-to-int (match-string 2 file-spec)))))
    (sd-exec-sd buffer (list "files" file-name) t)
    (save-excursion
      (set-buffer buffer)
      (if (> (count-lines (point-min) (point-max)) 1)
	  (error "File pattern maps to more than one file.")))
    (sd-exec-sd buffer (list "filelog" file-name) t)
    (setq fullname (sd-read-depot-output buffer))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (looking-at (concat "^\\.\\.\\. #\\([0-9]+\\) change \\([0-9]+\\)"
				"\\s-+\\(\\w+\\) .* by \\(.*\\)@"))
	    (let ((rev (string-to-int (match-string 1)))
		  (ch (string-to-int (match-string 2)))
		  (op (match-string 3)))
	      (cond ((and change (< change  ch))
		     nil)
		    ((and head-rev (< head-rev rev))
		     nil)
		    ((string= op "delete")
		     (goto-char (point-max)))
		    (t
		     (setq ch-alist (cons (cons rev ch) ch-alist))
		     (if (not head-rev)
			 (setq head-rev rev))
		     (setq headseen t))))
	  (if headseen
	      (if (looking-at "^\\.\\.\\. \\.\\.\\. branch from")
		  (goto-char (point-max)))))
	(forward-line)))
    (if (< (length ch-alist) 1)
	(error "Head revision not available"))
    (let ((base-rev (int-to-string (caar ch-alist)))
	  (ch-buffer (get-buffer-create "sd-ch-buf"))
	  (tmp-alst (copy-alist ch-alist)))
      (sd-exec-sd ch-buffer (list "print" "-q"
				  (concat fullname "#" base-rev))
		  t)
      (save-excursion
	(set-buffer ch-buffer)
	(goto-char (point-min))
	(while (re-search-forward ".*\n" nil t)
	  (replace-match (concat base-rev "\n"))))
      (while (> (length tmp-alst) 1)
	(let ((rev-1 (caar tmp-alst))
	      (rev-2 (car (cadr tmp-alst)))
	      ins-string)
	  (setq ins-string (concat rev-2 "\n"))
	  (sd-exec-sd buffer (list "diff2"
				   (concat fullname "#"
					   (int-to-string rev-1))
				   (concat fullname "#"
					   (int-to-string rev-2)))
		      t)
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (while (re-search-backward
		    (concat "^\\([0-9]+\\),?\\([0-9]*\\)\\([acd]\\)"
			    "\\([0-9]+\\),?\\([0-9]*\\)")
		    nil t)
	      (let ((la (string-to-int (match-string 1)))
		    (lb (string-to-int (match-string 2)))
		    (op (match-string 3))
		    (ra (string-to-int (match-string 4)))
		    (rb (string-to-int (match-string 5))))
		(if (= lb 0)
		    (setq lb la))
		(if (= rb 0)
		    (setq rb ra))
		(cond ((string= op "a")
		       (setq la (1+ la)))
		      ((string= op "d")
		       (setq ra (1+ ra))))
		(save-excursion
		  (set-buffer ch-buffer)
		  (goto-line la)
		  (let ((beg (point)))
		    (forward-line (1+ (- lb la)))
		    (delete-region beg (point)))
		  (while (<= ra rb)
		    (insert ins-string)
		    (setq ra (1+ ra)))))))
	  (setq tmp-alst (cdr tmp-alst))))
      (sd-noinput-buffer-action "print" nil t
				(list (concat fullname "#" (int-to-string
							    head-rev))))
      (let (line rev ch (old-rev 0))
	(save-excursion
	  (set-buffer buffer)
; 	  (make-local-variable 'sd-fname)
; 	  (setq sd-fname file-name)
	  (goto-line 2)
	  (move-to-column 0)
	  (insert "  Change  Rev\n")
	  (while (setq line (sd-read-depot-output ch-buffer))
	    (setq rev (string-to-int line))
	    (setq ch (cdr (assq rev ch-alist)))
	    (if (= rev old-rev)
		(insert (format "%13s : " ""))
	      (insert (format "  %6d %4d : " ch rev))
	      (move-to-column 0)
	      (if (looking-at " *\\([0-9]+\\) *\\([0-9]+\\)")
		  (progn
		    (sd-create-active-link (match-beginning 1)
					   (match-end 1)
					   (list (cons 'change
						       (match-string 1))))
		    (sd-create-active-link (match-beginning 2)
					   (match-end 2)
					   (list (cons 'rev
						       (match-string 2)))))))
	    (setq old-rev rev)
	    (forward-line))))

      (kill-buffer ch-buffer))
    (let ((buffer-name (concat "*SD print-revs " file-name "*")))
      (sd-activate-print-buffer buffer-name)
      (save-excursion
	(set-buffer buffer-name)
	(use-local-map sd-print-rev-map)))))

;; The sd refresh command
(defun sd-refresh ()
  "Refresh the contents of an unopened file. \\[sd-refresh].

Optional args [file ...] are passed as prefix arguments.

Refresh replaces the files with their contents from the depot.  Refresh only
refreshes unopened files; opened files must be reverted.  This command
requires naming files explicitly."

  (interactive)
  (let ((args (buffer-file-name)))
    (if (or current-prefix-arg (not args))
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd refresh: ")))
      (setq args (list args)))
    (sd-noinput-buffer-action "refresh" nil t args)
    (sd-refresh-files-in-buffers)
    (sd-make-depot-list-buffer
     (concat "*SD Refresh: (" (sd-current-client) ") " (car args) "*"))))

;; The sd get/sync command
(defun sd-sync ()
  (interactive)
  (sd-get))

(defun sd-get ()
  "To synchronise the local view with the depot, type \\[sd-get].

Optional args [-n] [file ...] can be passed as prefix arguments.

Synchronize a client with its view for the files named, or for the entire
client if no files named.  Get handles the case where files have been
updated in the depot and need to be brought up-to-date on the client as well
as the case when the view itself has changed.

Depot files in the clients view not currently gotten on the client will be
added.  Client files that were gotten from the depot but that are no longer
in the clients view (or have been deleted in the depot) will be deleted from
the client. Client files that are still in the client view but which have
been updated in the depot are replaced by the needed revision from the
depot.

If file gives a revision specifier, then retrieve the revision so indicated.

The client view is used to map client file names to depot file names and
vice versa.

If -n is given show what revisions would need to be gotten to synchronize
the client with the view, but do not actually get the files.  If no files
are named show the result for the entire client view."

  (interactive)
  (let ((args 'nil))
    (if current-prefix-arg
	(setq args (sd-make-list-from-string (sd-read-arg-string "sd get: "))))
    (sd-noinput-buffer-action "get" nil t args)
    (sd-refresh-files-in-buffers)
    (sd-make-depot-list-buffer
     (concat "*SD Get: (" (sd-current-client) ") " (car args) "*"))))

;; The sd have command
(defun sd-have ()
  "To list revisions last gotten, type \\[sd-have].

Optional args [file ...] are passed as prefix arguments.

List revisions of named files that were last gotten from the depot.  If no
file name is given list all files gotten on this client."

  (interactive)
  (let ((args (list "...")))
    (if current-prefix-arg
	(setq args (sd-make-list-from-string
		    (sd-read-arg-string "sd have: " (sd-buffer-file-name-2)))))
    (get-buffer-create sd-output-buffer-name);; We do these two lines
    (kill-buffer sd-output-buffer-name);; to ensure no duplicates
    (sd-noinput-buffer-action "have" nil t args)
    (sd-make-depot-list-buffer
     (concat "*SD Have: (" (sd-current-client) ") " (car args) "*"))))

;; The sd changes command
(defun sd-changes ()
  "To list changes, type \\[sd-changes].

Optional args [file ...] are passed as prefix arguments.

List pending and submitted changes of named files.  If no file name is
given list all changes affecting the current directory and below."

  (interactive)
  (let ((arg-string "..."))
    (if current-prefix-arg
	(setq arg-string (sd-read-arg-string "sd changes: " "-m 200")))
    (sd-file-change-log "changes" arg-string)))

;; The sd help command
(defun sd-help (arg)
  "To print help message , type \\[sd-help].

Print a help message about command.  If no command name is given print a
general help message about SourceDepot and give a list of available client
commands.

Argument ARG command for which help is needed."

  (interactive "sHelp on which command: ")
  (sd-noinput-buffer-action "help" nil t (sd-make-list-from-string arg))
  (sd-make-basic-buffer "*SD help*"))

(defun sd-make-basic-buffer (buf-name)
  (get-buffer-create buf-name)
  (kill-buffer buf-name)
  (set-buffer sd-output-buffer-name)
  (goto-char (point-min))
  (rename-buffer buf-name t)
  (use-local-map sd-basic-map)
  (setq buffer-read-only t)
  (sd-move-buffer-point-to-top buf-name))

;; The sd info command
(defun sd-info ()
  "To print out client/server information, type \\[sd-info].

Info dumps out what the server knows about the client (the user
name, the client name, and the client directory) and some server
information (the server's address, version, and license data)."

  (interactive)
  (sd-noinput-buffer-action "info" nil t)
  (sd-make-basic-buffer "*SD info*"))

;; The sd integrate command
(defun sd-integ ()
  "To schedule integrations between branches, type \\[sd-integ].

Optional args [-n -r] [-c change#] [file ...] are passed as prefix
arguments.

Integ determines what integrations are necessary between related files,
according to the branch named and its view.  These integrations, represented
by a list of revisions of branch source files which need to be merged into
the related branch target file, are scheduled for later action.  The actual
merge and any necessary conflict resolution is performed using the resolve
command.  The -n flag displays what integrations would be necessary but does
not schedule them.  A branch name is required.

If the -r flag is present, the mappings in the branch view are reversed,
with the target files and source files exchanging place.

If no file names are given then the entire branch view is examined for
needed integrations.  Files that are not mapped in the client's view are
ignored.  Files scheduled for integration are opened for the appropriate
action in the default change.  If -c change# is given the files are opened
in the numbered pending change.

Argument BRANCH is the branch to integrate into."

  (interactive)
  (let ((args (sd-make-list-from-string
           (sd-read-arg-string "sd integ: " "-b "))))
    (sd-noinput-buffer-action "integrate" nil t args)))

(defun sd-rename ()
  "To rename a file in the depot, type \\[sd-rename].

SourceDepot does not support a single 'rename' command, but files can
be renamed by branching one file into another and then deleting the
original file.

The 'from' and 'to' file arguments may include wildcards as long as
they are matched.

Integrating from files require read access to the files, but deleting
them requires write access.

For further information, see the help for the individual commands."

  (interactive)
  (let (from-file to-file)
    (setq from-file (sd-read-arg-string "rename from: " buffer-file-name))
    (setq to-file (sd-read-arg-string "rename to: " buffer-file-name))
    (sd-noinput-buffer-action "integ" nil t (list from-file to-file))
    (sd-exec-sd (get-buffer-create sd-output-buffer-name)
        (list "delete" from-file)
        nil)))

(defun sd-scroll-down-1-line ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1))

(defun sd-scroll-up-1-line ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1))

(defun sd-scroll-down-1-window ()
  "Scroll down one window"
  (interactive)
  (scroll-down
   (- (window-height) next-screen-context-lines)))

(defun sd-scroll-up-1-window ()
  "Scroll up one window"
  (interactive)
  (scroll-up
   (- (window-height) next-screen-context-lines)))

(defun sd-top-of-buffer ()
  "Top of buffer"
  (interactive)
  (goto-char (point-min)))

(defun sd-bottom-of-buffer ()
  "Bottom of buffer"
  (interactive)
  (goto-char (point-max)))

(defun sd-delete-other-windows ()
  "Make buffer full height"
  (interactive)
  (delete-other-windows))

(defun sd-goto-next-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^@@" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun sd-goto-prev-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^@@" nil "")
  (set-window-start (selected-window) (point)))

(defun sd-next-depot-file ()
  "Next file"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^//[a-zA-Z]+/" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun sd-prev-depot-file ()
  "Previous file"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^//[a-zA-Z]+/" nil "")
  (set-window-start (selected-window) (point)))


(defun sd-next-depot-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun sd-prev-depot-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (set-window-start (selected-window) (point)))

(defun sd-next-change-rev-line ()
  "Next change/revision line"
  (interactive)
  (let ((c (if (< (current-column) 8) 7 12)))
    (move-to-column 2)
    (re-search-forward "^ +[0-9]+ +[0-9]+ :" nil "")
    (move-to-column c)))

(defun sd-prev-change-rev-line ()
  "Previous change/revision line"
  (interactive)
  (let ((c (if (< (current-column) 8) 7 12)))
    (forward-line -1)
    (move-to-column 16)
    (re-search-backward "^ +[0-9]+ +[0-9]+ :" nil "")
    (move-to-column c)))

(defun sd-quit-current-buffer (pnt)
  "Quit a buffer"
  (interactive "d")
  (if (not (one-window-p))
      (delete-window)
    (bury-buffer)))

(defun sd-buffer-mouse-clicked (event)
  "Function to translate the mouse clicks in a SD filelog buffer to
character events"
  (interactive "e")
  (cond 
   (sd-running-xemacs
    (select-window (event-window event))
    (sd-buffer-commands (event-point event)))
   (sd-running-emacs
    (select-window (posn-window (event-end event)))
    (sd-buffer-commands (posn-point (event-start event))))))

(defun sd-buffer-commands (pnt)
  "Function to get a given property and do the appropriate command on it"
  (interactive "d")
  (let ((rev (get-char-property pnt 'rev))
	(change (get-char-property pnt 'change))
	(action (get-char-property pnt 'action))
	(user (get-char-property pnt 'user))
	(client (get-char-property pnt 'client))
	(job (get-char-property pnt 'job)))
    (cond ((and (not action) rev)
	   (let ((fn1 (concat filename "#" rev)))
	     (sd-noinput-buffer-action "print" nil t (list fn1))
	     (sd-activate-print-buffer "*SD print*")))
	  (action
	   (let ((rev2 (int-to-string (1- (string-to-int rev))))
		 (fn1 (concat filename "#" rev))
		 (fn2 nil))
	     (setq fn2 (concat filename "#" rev2))
	     (if (> (string-to-int rev2) 0)
		 (progn
		   (sd-noinput-buffer-action
		    "diff2" nil t
		    (append (sd-make-list-from-string
			     sd-default-diff-options)
			    (list fn2 fn1)))
		   (sd-activate-diff-buffer "*SD diff*"))
	       (error "There is no earlier revision to diff."))))
	  (change (sd-describe-internal
		   (concat sd-default-diff-options " " change)))
	  (user (sd-async-process-command "user" nil
					  (concat
					   "*SD User: " user "*")
					  "user" (list user)))
	  (client (sd-async-process-command
		   "client" nil (concat "*SD Client: " client
					"*") "client" (list client)))

	  (job (sd-async-process-command "job" "Description:\n\t" nil nil (list job)))

	  ;; Check if a "filename link" or an active "diff buffer area" was
	  ;; selected.
	  (t
	   (let ((link-client-name (get-char-property pnt 'link-client-name))
		 (link-depot-name (get-char-property pnt 'link-depot-name))
		 (block-client-name (get-char-property pnt 'block-client-name))
		 (block-depot-name (get-char-property pnt 'block-depot-name))
		 (first-line (get-char-property pnt 'first-line))
		 (start (get-char-property pnt 'start)))
	     (cond ((or link-client-name link-depot-name)
		    (sd-find-file-or-print-other-window
		     link-client-name link-depot-name))
		   ((or block-client-name block-depot-name)
		    (if first-line
			(let ((c (max 0 (- pnt
					   (save-excursion
					     (goto-char pnt)
					     (beginning-of-line)
					     (point))
					   1)))
			      (r first-line))
			  (save-excursion
			    (goto-char start)
			    (while (re-search-forward "^[ +>].*\n" pnt t)
			      (setq r (1+ r))))
			  (sd-find-file-or-print-other-window
			   block-client-name block-depot-name)
			  (goto-line r)
			  (if (not block-client-name)
			      (forward-line 1))
			  (beginning-of-line)
			  (goto-char (+ (point) c)))
		      (sd-find-file-or-print-other-window
		       block-client-name block-depot-name)))
		   (t
		    (error "There is no file at that cursor location!"))))))))

(defun sd-find-file-or-print-other-window (client-name depot-name)
  (if client-name
      (find-file-other-window client-name)
    (sd-noinput-buffer-action "print" nil t
			      (list depot-name))
    (sd-activate-print-buffer depot-name)
    (other-window 1)))

(defun sd-find-file-other-window ()
  "Open file"
  (interactive)
  (if (sd-buffer-file-name-2)
      (progn
	(find-file-other-window
	 (sd-buffer-file-name-2))
	(other-window 1))))

(defun sd-filelog-short-format ()
  "Short format"
  (interactive)
  (setq buffer-invisibility-spec t)
  (redraw-display))

(defun sd-filelog-long-format ()
  "Long format"
  (interactive)
  (setq buffer-invisibility-spec (list))
  (redraw-display))

(defun sd-scroll-down-1-line-other-w ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window -1))

(defun sd-scroll-up-1-line-other-w ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))

(defun sd-scroll-down-1-window-other-w ()
  "Scroll other window down one window"
  (interactive)
  (scroll-other-window
   (- next-screen-context-lines (window-height))))

(defun sd-scroll-up-1-window-other-w()
  "Scroll other window up one window"
  (interactive)
  (scroll-other-window
   (- (window-height) next-screen-context-lines)))

(defun sd-top-of-buffer-other-w ()
  "Top of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window -1))

(defun sd-bottom-of-buffer-other-w ()
  "Bottom of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun sd-goto-next-change ()
  "Next change"
  (interactive)
  (let ((c (current-column)))
    (forward-line 1)
    (while (get-char-property (point) 'invisible)
      (forward-line 1))
    (move-to-column c)))

(defun sd-goto-prev-change ()
  "Previous change"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (while (get-char-property (point) 'invisible)
      (forward-line -1))
    (move-to-column c)))


;; Activate special handling for a buffer generated with a diff-like command
(make-face 'sd-diff-file-face)
(set-face-background 'sd-diff-file-face "gray90")

(make-face 'sd-diff-head-face)
(set-face-background 'sd-diff-head-face "gray95")

(make-face 'sd-diff-ins-face)
(set-face-foreground 'sd-diff-ins-face "blue")

(make-face 'sd-diff-del-face)
(set-face-foreground 'sd-diff-del-face "red")

(make-face 'sd-diff-change-face)
(set-face-foreground 'sd-diff-change-face "dark green")

(defun sd-buffer-set-face-property (regexp face-property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0))
        (end (match-end 0)))
    (sd-set-extent-property start end
                'face face-property)))))

(defun sd-activate-diff-buffer (buffer-name)
  (sd-make-depot-list-buffer buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (if sd-colorized-diffs
	(progn
	  (sd-buffer-set-face-property "^=.*\n" 'sd-diff-file-face)
	  (sd-buffer-set-face-property "^[@*].*" 'sd-diff-head-face)
	  (sd-buffer-set-face-property "^\\([+>].*\n\\)+" 'sd-diff-ins-face)
	  (sd-buffer-set-face-property "^\\([-<].*\n\\)+" 'sd-diff-del-face)
	  (sd-buffer-set-face-property "^\\(!.*\n\\)+" 'sd-diff-change-face)))
;       (sd-buffer-set-face-property "^=.*\n" 'sd-diff-file-face)
;       (sd-buffer-set-face-property "^\\(@\\|\\*\\).*" 'sd-diff-head-face)
;       (sd-buffer-set-face-property "^\\(\\+\\|>\\).*$" 'sd-diff-ins-face)
;       (sd-buffer-set-face-property "^\\(\\-\\|<\\).*$" 'sd-diff-del-face)
;       (sd-buffer-set-face-property "^!.*$" 'sd-diff-change-face)))

    (goto-char (point-min))
    (while (re-search-forward "^\\(==== //\\).*\n\\(\\(\n\\|[^=\n].*\n\\)*\\)"
                  nil t)
      (let ((link-client-name (get-char-property (match-end 1) 'link-client-name))
	    (link-depot-name (get-char-property (match-end 1) 'link-depot-name))
	    (start (match-beginning 2))
	    (end (match-end 2)))
	(if link-client-name
	    (sd-set-extent-property start end
				    'block-client-name link-client-name))
	(if link-depot-name
	    (sd-set-extent-property start end
				    'block-depot-name link-depot-name))))

    (goto-char (point-min))
    (while (re-search-forward
        (concat "^[@0-9].*\\([cad+]\\)\\([0-9]*\\).*\n"
            "\\(\\(\n\\|[^@0-9\n].*\n\\)*\\)") nil t)
      (let ((first-line (string-to-int (match-string 2)))
        (start (match-beginning 3))
        (end (match-end 3)))
    (sd-set-extent-property start end 'first-line first-line)
    (sd-set-extent-property start end 'start start)))

    (goto-char (point-min))
    (let ((stop
       (if (re-search-forward "^\\(\\.\\.\\.\\|====\\)" nil t)
           (match-beginning 0)
         (point-max))))
      (sd-find-change-numbers buffer-name (point-min) stop))

    (use-local-map sd-diff-map)
    (setq buffer-read-only t)))


;; The sd describe command
(defun sd-describe ()
  "To get a description for a change number, type \\[sd-describe].

Display a changelist description, including the changelist number,
user, client, date of submission, textual description, list
of affected files and diffs of files updated.  Pending changelists
are flagged as 'pending' and the list of affected files and
file diffs is not displayed.

The -d<flag> passes a flag to the built-in diff routine to
modify the output: -dn (RCS), -dc (context), -du (unified).

The -s flag requests a shortened form of describe that doesn't
include the diffs of files updated."

  (interactive)
  (let ((arg-string (read-string "sd describe: "
                 (concat sd-default-diff-options " "))))
    (sd-describe-internal arg-string)))

;; Internal version of the sd describe command
(defun sd-describe-internal (arg-string)
  (get-buffer-create sd-output-buffer-name) ;; We do these two lines
  (kill-buffer sd-output-buffer-name)       ;; to ensure no duplicates
  (sd-noinput-buffer-action
   "describe" nil t (sd-make-list-from-string arg-string))
  (sd-activate-diff-buffer
   (concat "*SD describe: " arg-string "*")))

;; The sd opened command
(defun sd-opened ()
  "To display list of files opened for pending change, type \\[sd-opened].

Optional args [-a] [file ...] are passed as prefix arguments.

Shows files currently opened for pending changes or indicates for the
specified individual files whether they are currently opened.
If no file names are given, all files open on the current client
are listed.  The -a flag lists opened files in all clients."

  (interactive)
  (let ((args '()))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd opened: "
                    (sd-buffer-file-name-2)))))
    (sd-opened-internal args)))

(defun sd-opened-internal (args)
  (let ((sd-client (sd-current-client)))
    (get-buffer-create sd-output-buffer-name) ;; We do these two lines
    (kill-buffer sd-output-buffer-name)       ;; to ensure no duplicates
    (sd-noinput-buffer-action "opened" nil t args)
    (sd-make-depot-list-buffer (concat "*Opened Files: " sd-client "*"))))

(defun sd-update-opened-list ()
  (if (get-buffer-window (concat "*Opened Files: " (sd-current-client) "*"))
      (progn
    (setq current-prefix-arg nil)
    (sd-opened-internal nil))))

;; The sd users command
(defun sd-users ()
  "To display list of known users, type \\[sd-users].

Optional args [user ...] are passed as prefix arguments.

Reports the list of all users, or those users matching the argument,
currently known to the system.  The report includes the last time
each user accessed the system."

  (interactive)
  (let ((args '()))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd users: " nil "user"))))
    (sd-noinput-buffer-action "users" nil t args))
  (sd-make-basic-buffer "*SD users*"))


;; For highlighting
(defun sd-buffer-set-mouse-highlight (regexp tag)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0))
        (end (match-end 0)))
	(sd-create-active-link start end (list (cons tag (match-string 1))))))))


;; The sd jobs command
(defun sd-jobs ()
  "To display list of jobs, type \\[sd-jobs].

Optional args [ -e jobview -i -l -m max ] [ file[revRange] ... ] are passed
as prefix arguments.

Reports the list of all jobs currently known to the system.  If a
file (pattern) is given, only fixes for changelists affecting that
file (or set of files) are listed.  The file pattern may include
wildcards and/or a revision number range.  See 'sd help revisions'
for help specifying revisions.

The -e jobview limits the output to jobs satisfying the expression
given as 'jobview'.  See 'sd help jobview' for a description of
jobview syntax.

The -i flag also includes any fixes made by changelists integrated
into the specified files.

The -l flag produces long output with the full text of the job
descriptions.

The -m max flag limits the output to the first 'max' jobs,
ordered by their job name."
  (interactive)
  (let ((args '()))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string (sd-read-arg-string "sd jobs: "))))
    (sd-noinput-buffer-action "jobs" nil t args))
  (sd-make-basic-buffer "*SD jobs*")
  ;; highlight job entries.
  (save-excursion
    (set-buffer "*SD jobs*")
    (sd-buffer-set-mouse-highlight "^\\(job[0-9][0-9][0-9][0-9][0-9][0-9]\\).*$" 'job)))

;; The sd fix command
(defun sd-fix ()
  "To mark jobs as being fixed by a changelist number, type \\[sd-fix].

'sd fix' marks each named job as being fixed by the changelist
number given with -c.  The changelist may be either pending or,
submitted and the jobs may be still be opened or already closed
 (fixed by another changelist).

If the changelist has already been submitted and the job is still
open then 'sd fix' marks the job closed.  If the changelist has not
been submitted and the job is still open, the job will be marked
closed when the changelist is submitted.  If the job is already
closed, it is left alone.

The -d flag causes the specified fixes to be deleted.  This does not
otherwise affect the named changelist or jobs."
  (interactive)
  (let ((args (sd-make-list-from-string (sd-read-arg-string "sd fix: "
                                nil "job"))))
    (sd-noinput-buffer-action "fix" nil t args)))

;; The sd fixes command
(defun sd-fixes ()
  "To list what changeslists fix what jobs, type \\[sd-fixes].

sd fixes [ -i ] [ -j jobName ] [ -c changelist# ] [ file[revRange] ... ]

'sd fixes' shows all jobs with fix records associated with them,
along with the changelist number of the fix.  Fix records are
created either directly with the 'sd fix' command or via changelist
creation with the 'sd change' and 'sd submit' commands.

The 'sd fixes' command show fixes regardless of whether the
changelists are submitted or still pending.

By default, 'sd fixes' lists all fixes.  This list can be limited in
any of three ways.  If -j jobName is given, only fixes for the named
job are listed.  If -c changelist# is given, only fixes from the
numbered changelist are listed.  If a file (pattern) is given, only
fixes for changelists affecting that file (or set of files) are
listed.  The file pattern may include wildcards and/or a revision
number range.  See 'sd help revisions' for help specifying revisions.

The -i flag also includes any fixes made by changelists integrated
into the specified files."
  (interactive)
  (let ((args 'nil))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string (sd-read-arg-string "sd fixes: "))))
    (sd-noinput-buffer-action "fixes" nil t args)
    (sd-make-basic-buffer "*SD fixes*")))

;; The sd where command
(defun sd-where ()
  "To show how local file names map into depot names, type \\[sd-where].

Optional args [file ...] are passed as prefix arguments.

Where shows how the named files map through the client map
into the depot.  If no file is given, the mapping for '...'
\(all files in the current directory and below\) is shown."
  (interactive)
  (let ((args '()))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd where: "
                    (sd-buffer-file-name-2)))))
    (get-buffer-create sd-output-buffer-name) ;; We do these two lines
    (kill-buffer sd-output-buffer-name)       ;; to ensure no duplicates
    (sd-noinput-buffer-action "where" nil 's args)))


(defun sd-async-process-command (sd-this-command &optional
						 sd-regexp
						 sd-this-buffer
						 sd-out-command
						 sd-in-args
						 sd-out-args)
  "Internal function to call an asynchronous process with a local buffer,
instead of calling an external client editor to run within emacs.

Arguments:
SD-THIS-COMMAND is the command that called this internal function.

SD-REGEXP is the optional regular expression to search for to set the cursor
on.

SD-THIS-BUFFER is the optional buffer to create. (Default is *SD <command>*).

SD-OUT-COMMAND is the optional command that will be used as the command to
be called when `sd-async-call-process' is called.

SD-IN-ARGS is the optional argument passed that will be used as the list of
arguments to the SD-THIS-COMMAND.

SD-OUT-ARGS is the optional argument passed that will be used as the list of
arguments to SD-OUT-COMMAND."
  (if sd-this-buffer
      (set-buffer (get-buffer-create sd-this-buffer))
    (set-buffer (get-buffer-create (concat "*SD " sd-this-command "*"))))
  (setq sd-current-command sd-this-command)
  (if (zerop (apply 'call-process-region (point-min) (point-max)
		    (sd-check-sd-executable) t t nil
		    sd-current-command "-o"
		    sd-in-args))
      (progn
    (goto-char (point-min))
    (insert (concat "# Created using " (sd-emacs-version) ".\n"
            "# Type C-c C-c to submit changes and exit buffer.\n"
            "# Type C-x k to kill current changes.\n"
            "#\n"))
    (if sd-regexp (re-search-forward sd-regexp))
    (indented-text-mode)
    (setq sd-minor-mode t)
    (setq fill-column 79)
    (sd-push-window-config)
    (switch-to-buffer-other-window (current-buffer))
    (if sd-out-command
        (setq sd-current-command sd-out-command))
    (setq sd-current-args sd-out-args)

    (define-key sd-minor-map "\C-c\C-c" 'sd-async-call-process)
    (run-hooks 'sd-async-command-hook)
    (message "C-c C-c to finish editing and exit buffer."))
    (error "%s %s -o failed to complete successfully."
       (sd-check-sd-executable) sd-current-command)))

(defun sd-async-call-process ()
  "Internal function called by `sd-async-process-command' to process the
buffer after editing is done using the minor mode key mapped to `C-c C-c'."
  (interactive)
  (message "sd %s ..." sd-current-command)
  (let ((max (point-max)) msg
	(current-command sd-current-command))
    (goto-char max)
    (if (zerop (apply 'call-process-region (point-min)
              max (sd-check-sd-executable)
              nil '(t t) nil
              current-command "-i"
              sd-current-args))
    (progn
      (goto-char max)
      (setq msg (buffer-substring max (point-max)))
      (delete-region max (point-max))
      (save-excursion
        (set-buffer (get-buffer-create sd-output-buffer-name))
        (delete-region (point-min) (point-max))
        (insert msg))
      (kill-buffer nil)
      (display-buffer sd-output-buffer-name)
      (sd-partial-cache-cleanup sd-current-command)
      (message "sd %s done." sd-current-command)
      (if (equal current-command "submit")
          (progn
        (sd-refresh-files-in-buffers)
        (sd-check-mode-all-buffers)))
      (if (and sd-notify (equal current-command "submit"))
          (sd-notify sd-notify-list)))
      (error "%s %s -i failed to complete successfully."
	     (sd-check-sd-executable)
	     current-command))))

;; The sd change command
(defun sd-change ()
  "To edit the change specification, type \\[sd-change].

Optional args [-d | -o] [ change# ] passed as prefix arguments.

Creates a new change description with no argument or edit the
text description of an existing change if a change number is
given.  To associate or remove files from a pending change use
the open commands (edit, add, delete) or revert.

The -d flag discards a pending change, but only if it has no
opened files and no pending fixes associated with it.  Use 'opened -a'
to report on opened files and 'reopen' to move them to another
change.  Use 'fixes -c change#' to report on pending fixes and
'fix -d -c change# jobs...' to delete pending fixes.  The change
can only be deleted by the user and client who created it.

The -o flag causes the change specification to be written
to the standard output.  The user's editor is not invoked.

The -i flag causes a change specification to be read from the
standard input.  The user's editor is not invoked."

  (interactive)
  (sd-async-process-command "change" "Description:\n\t" "*SD New Change*"))

;; The sd client command
(defun sd-client ()
  "To edit a client specification , type \\[sd-client].

With no argument client creates a new client view specification or
edits an existing client specification. The client name is taken
from the environment variable $SDCLIENT if set, or else from
the current host name.  The specification form is put into a
temporary file and the editor (given by the environment variable
$EDITOR) is invoked.  If a name is given, the specification of
the named client is displayed read-only.

The specification form contains the following fields:

Client:      The client name (read only.)

Date:        The date specification was last modified (read only.)

Description: A short description of the client (optional).

Root:        The root directory of the client file workspace
         (given in local file system syntax), under which all
         client files will be placed.  If you change this, you
         must physically relocate any files as well.

View:        What files you want to see from the depot and how
         they map to locations on the client.  The left hand
         side specifies a depot path, which must begin with
         //depot/.  The right hand side gives the corresponding
         client path, given in canonical SourceDepot file syntax.
         On expansion to an actual local client file name the
         initial //client/ is replaced by the Root value, given
         above.  You may use wildcards:

         ...        matches any characters including
         *      matches any character except /
         %1 to %9   like *, used to associate wild cards

         Wildcarding must be congruent in both the client and
         depot paths.  You may have any number of view entries.
         A new view takes effect on the next 'get'.

Normally, new clients are created with a default view that maps
all depot files onto the client.  The -t flag uses the view from
the named template client as a default instead.

The -d flag causes the named client to be deleted.

The -o flag causes the named client specification to be written
to the standard output.  The user's editor is not invoked.

The -i flag causes a client specification to be read from the
standard input.  The user's editor is not invoked."

  (interactive)
  (let ((args nil)
	(client-buf-name "*SD client*"))
    (if (buffer-live-p (get-buffer client-buf-name))
	(switch-to-buffer-other-window (get-buffer client-buf-name))
      (if current-prefix-arg
	  (setq args (sd-make-list-from-string
		      (sd-read-arg-string "sd client: " nil "client"))))
      (if (memq t (mapcar (lambda (x) (not (not (string-match "^-" x))))
			   args))
	  (sd-noinput-buffer-action "client" nil t args)
	(sd-async-process-command "client" "Description:\n\t"
				  client-buf-name nil args)))))

(defun sd-clients ()
  "To list all clients, type \\[sd-clients].

Reports the list of all clients currently known to the system."
  (interactive)
  (sd-noinput-buffer-action "clients" nil t nil)
  (sd-make-basic-buffer "*SD clients*"))

(defun sd-branch (args)
  "Edit a SD-BRANCH specification using \\[sd-branch]."
  (interactive (list
		(sd-make-list-from-string
		 (sd-read-arg-string "sd branch: " nil "branch"))))
  (if (or (null args) (equal args (list "")))
      (error "Branch must be specified!")
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			 args))
	(sd-noinput-buffer-action "branch" nil t args)
      (sd-async-process-command "branch" "Description:\n\t"
				(concat "*SD Branch: "
					(car (reverse args)) "*")
				"branch" args))))

(defun sd-branches ()
  "To list all branches, type \\[sd-branches].

Reports the list of all branches currently known to the system.
Branches takes no arguments."
  (interactive)
  (sd-noinput-buffer-action "branches" nil t nil)
  (sd-make-basic-buffer "*SD branches*"))

(defun sd-label (args)
  "Edit a SD-label specification using \\[sd-label]."
  (interactive (list
		(sd-make-list-from-string
		 (sd-read-arg-string "sd label: " nil "label"))))
  (if (or (null args) (equal args (list "")))
      (error "label must be specified!")
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			 args))
	(sd-noinput-buffer-action "label" nil t args)
      (sd-async-process-command "label" "Description:\n\t"
				(concat "*SD label: "
					(car (reverse args)) "*")
				"label" args))))

(defun sd-labels ()
  "To display list of defined labels, type \\[sd-labels].

Reports the list of all labels currently known to the system.
Labels takes no arguments."
  (interactive)
  (sd-noinput-buffer-action "labels" nil t nil)
  (sd-make-basic-buffer "*SD labels*"))

;; The sd labelsync command
(defun sd-labelsync ()
  "To synchronize a label with the current client contents, type
\\[sd-labelsync]."
  (interactive)
  (let ((args (sd-make-list-from-string
           (sd-read-arg-string "sd labelsync: "))))
    (sd-noinput-buffer-action "labelsync" nil t args))
  (sd-make-depot-list-buffer "*SD labelsync*"))

;; The sd submit command
(defun sd-submit ()
  "To submit a pending change to the depot, type \\[sd-submit].

Submit commits a pending change with its associated files to the depot.
With no argument submit sends the 'default' change.  With the -c flag
the designated pending change is sent.  Before committing the change
submit locks all associated files not already locked.  If any file
cannot be locked the change is aborted.  If submit is sending the
default change it first provides the user with a dialog similar to
'sd change' so the user can compose a change description.  In this
dialog the user is presented with the list of open files in change
'default'.  Files may be deleted from this list but they cannot be
added.  (Use an open command (open, edit, add, delete) to add
additional files to a change or to move files between changes.)

If the submit fails for any reason the files are left open in
a newly created pending change.

Submit is guaranteed to be atomic.  Either all files will be
updated in the depot as a unit or none will be.

The -i flag causes a change specification to be read from the
standard input.  The user's editor is not invoked."

  (interactive)
  (let ((args nil)
	(submit-buf-name "*SD Submit*"))
    (if (buffer-live-p (get-buffer submit-buf-name))
	(switch-to-buffer-other-window (get-buffer submit-buf-name))
      (if current-prefix-arg
	  (setq args (sd-make-list-from-string
		      (sd-read-arg-string "sd submit: " nil))))
      (save-some-buffers)
      (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
			   args))
	  (progn
	    (sd-noinput-buffer-action "submit" nil t args)
	    (sd-refresh-files-in-buffers))
	(if (or (not (and sd-check-empty-diffs (sd-empty-diff-p)))
		(progn
		  (ding t)
		  (yes-or-no-p
		   "File with empty diff opened for edit. Submit anyway? ")))
	    (sd-async-process-command "change" "Description:\n\t"
				      submit-buf-name "submit" args))))))

;; The sd user command
(defun sd-user ()
  "To create or edit a user specification, type \\[sd-user].

Create a new user specification or edit an existing user
specification. The specification form is put into a temporary
file and the editor (given by the environment variable $EDITOR)
is invoked.

Normally, a user specification is created automatically the
first time the user invokes any client command that can update
the depot.  The 'user' command is generally used to edit the
user's reviewing subscription list for change review.

The user specification form contains the following fields:

User:        The user name (read only).

Email:       The user's email address (user@client default).

Update:      The date the specification was last modified (read only).

Access:      The date the user last issued a client command.

FullName:    The user's real name.

Reviews:     The subscription list for change review.  You may
         use wildcards:
         ...        matches any characters including

         *      matches any character except /
         There may be any number of review lines.

The -d flag deletes the named user, but only if the user is not
the owner of any branches, clients, jobs, labels, or opened files.

The -o flag causes the named user specification to be written
to the standard output.  The user's editor is not invoked.

The -i flag causes a user specification to be read from the
standard input.  The user's editor is not invoked."

  (interactive)
  (let ((args nil))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd user: " nil "user"))))
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
             args))
    (sd-noinput-buffer-action "user" nil t args)
      (sd-async-process-command "user" nil nil nil args))))

;; The sd job command
(defun sd-job ()
  "To create or edit a job, type \\[sd-job].

'sd job' creates and edits job specifications using an ASCII form.
A job is a defect, enhancement, or other unit of intended work.
The 'sd fix' command can associate changelists with jobs.

With no arguments, 'sd job' creates a blank job specification form
and invokes the user's editor.  When the form is saved, a job name
of the form jobNNNNNN is created.  If a jobName is given on the
command line either that named job will be created or, if the job
already exists, the job can be modified.

As jobs are entered or updated, all fields are indexed for
searching by 'sd jobs'.  Text fields are broken into individual
alphanumeric words (punctuation and whitespace are ignored) and
each word is entered, case folded, into the word index.  Date
fields are converted to an internal representation (seconds
since 1970/01/01 00:00:00) and entered into the date index.

The fields of a job are defined by the 'sd jobspec' command.
There is a simple default jobspec that is used if no explicit
one has been defined.

The -d flag deletes the named job and any associated fixes.

The -o flag causes the named job specification to be written
to the standard output.  The user's editor is not invoked.

The -i flag causes a job specification to be read from the
standard input.  The user's editor is not invoked.

The -f flag allows otherwise read-only fields to be set."
  (interactive)
  (let ((args nil))
    (if current-prefix-arg
    (setq args (sd-make-list-from-string
            (sd-read-arg-string "sd job: " nil "job"))))
    (if (memq 't (mapcar (lambda (x) (not (not (string-match "^-" x))))
             args))
	(sd-noinput-buffer-action "job" nil t args)
      (sd-async-process-command "job" "Description:\n\t" nil nil args))))

;; The sd jobspec command
(defun sd-jobspec ()
  "To edit the job template, type \\[sd-jobspec]."
  (interactive)
  (sd-async-process-command "jobspec"))

;; A function to get the current SD client root to be used by various other
;; macros, if needed.

(defun sd-get-client-root (client-name)
  "To get the current value of Client's root type \\[sd-get-client-root].
   This can be used by any other macro that requires this value.
"
  (interactive (list
		(completing-read "Client: " (if sd-my-clients
						sd-my-clients
					      'sd-clients-completion)
				 nil sd-strict-complete (sd-current-client))))
  (if (not client-name)
      nil
    (let (sd-client-root pmin)
      (save-excursion
	(get-buffer-create sd-output-buffer-name)
	(set-buffer sd-output-buffer-name)
	(goto-char (point-max))
	(setq pmin (point))
	(if (zerop (call-process
		    (sd-check-sd-executable)
		    nil t nil "client" "-o" client-name))
	    (progn
	      (save-restriction
		(narrow-to-region pmin (point-max))
		(goto-char pmin))
	      (re-search-forward "^Root:[ \t]+\\(.*\\)$")
	      (setq sd-client-root (match-string 1))
	      ;;(message "Root of %s is %s" client-name sd-client-root)
	      (delete-region pmin (point-max)))))
      sd-client-root)))

(defun sd-get-info (key &optional key-list)
  "To get the value associated with KEY, \\[sd-get-info].
This can be used by any other macro that requires this value.

KEY can be any one of

User name       Client name      Client root    Current directory
Client address  Server address   Server root    Server version
Server license  NULL

If KEY is `NULL', and KEY-LIST is defined as a list of KEYS \(strings\),
like:

\(sd-get-info \"NULL\" \(list \"Client name\" \"Client root\"
             \"Server address\"\)\)

then, a corresponding list of values is returned."
  (interactive "sKey: ")
  (let (sd-this-info pmin)
    (save-excursion
      (get-buffer-create sd-output-buffer-name)
      (set-buffer sd-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (if (zerop (call-process (sd-check-sd-executable) nil t nil "info"))
	  (save-restriction
	    (narrow-to-region pmin (point-max))
	    (let ((all-keys (if (equal key "NULL")
				(if key-list key-list nil)
			      (list key)))
		  all-vals cur-key)
	      (while all-keys
		(setq cur-key (car all-keys))
		(setq all-keys (cdr all-keys))
		(goto-char pmin)
		(if cur-key
		    (progn
		      ;;(message "Looking for %s" cur-key)
		      (re-search-forward (concat "^" cur-key
						 ":[ \t]+\\(.*\\)$") nil t)
		      (add-to-list 'all-vals (match-string 1)))))
	      (setq sd-this-info (if (= (length all-vals) 1)
				     (car all-vals)
				   (reverse all-vals)))
	      (delete-region pmin (point-max)))))
      sd-this-info)))

;; A function to get the current SD client name
(defun sd-get-client-name ()
  "To get the current value of the environment variable SDCLIENT,
type \\[sd-get-client-name].

This will be the current client that is in use for access through
Emacs SD."

  (interactive)
  (let ((client (if sd-global-config sd-local-client sd-global-clt))
    (global-clt sd-global-clt))
    (message "SDCLIENT [buffer-local: %s], [global: %s]" sd-local-client
         global-clt)
    client))

;; A function to set the current SD client name
(defun sd-set-client-name (sd-new-client-name)
  "To set the current value of SDCLIENT, type \\[sd-set-client-name].

This will change the current client from the previous client to the new
given value.

Setting this value to nil would disable SD Version Checking.

`sd-set-client-name' will complete any client names set using the function
`sd-set-my-clients'. The strictness of completion will depend on the
variable `sd-strict-complete' (default is t).

Argument SD-NEW-CLIENT-NAME The new client to set to. The default value is
the current client."
  (interactive (list
		(completing-read "Change Client to: "
				 (if sd-my-clients
				     sd-my-clients
				   'sd-clients-completion)
				 nil sd-strict-complete (sd-current-client))
		))
  (if (or (null sd-new-client-name) (equal sd-new-client-name "nil"))
      (progn
	(setenv "SDCLIENT"  nil)
	(if (not (getenv "SDCONFIG"))
	    (message
	     "SD Version check disabled. Set a valid client name to enable."
	     )))
    (progn
      (setenv "SDCLIENT"  sd-new-client-name)
      (setq sd-global-clt sd-new-client-name)
      (message  "SDCLIENT changed to %s" sd-new-client-name)
      (run-hooks 'sd-set-client-hooks))))

(defun sd-get-client-config ()
  "To get the current value of the environment variable SDCONFIG,
type \\[sd-get-client-config].

This will be the current configuration that is in use for access through
Emacs SD."

  (interactive)
  (message "SDCONFIG is %s" sd-global-config))

(defun sd-set-client-config (sdconfig)
  "To set the SDCONFIG variable, for use with the current versions of the sd
client.

SDCONFIG is a more flexible mechanism wherein sd will find the current
client automatically by checking the config file found at the root of a
directory \(recursing all the way to the top\).

In this scenario, a SDCLIENT variable need not be explicitly set.
"
  (interactive "sSD Config: ")
  (if (or (null sdconfig) (equal sdconfig ""))
      (message "SDCONFIG not changed.")
    (setenv "SDCONFIG"  sdconfig)
    (setq sd-global-config sdconfig)
    (message "SDCONFIG changed to %s" sd-global-config)))

(defun sd-set-my-clients (client-list)
  "To set the client completion list used by `sd-set-client-name', use
this function in your .emacs (or any lisp interaction buffer).

This will change the current client list from the previous list to the new
given value.

Setting this value to nil would disable client completion by
`sd-set-client-name'.

The strictness of completion will depend on the variable
`sd-strict-complete' (default is t).

Argument CLIENT-LIST is the 'list' of clients.

To set your clients using your .emacs, use the following:

\(load-library \"sd\"\)
\(sd-set-my-clients \'(client1 client2 client3)\)"
  (setq sd-my-clients nil)
  (let ((sd-tmp-client-var nil))
    (while client-list
      (setq sd-tmp-client-var (format "%s" (car client-list)))
      (setq client-list (cdr client-list))
      (setq sd-my-clients (append sd-my-clients
                  (list (list sd-tmp-client-var)))))))

;; A function to get the current SDPORT
(defun sd-get-sd-port ()
  "To get the current value of the environment variable SDPORT, type \
\\[sd-get-sd-port].

This will be the current server/port that is in use for access through Emacs
SD."

  (interactive)
  (message "SDPORT is %s" (getenv "SDPORT")))

;; A function to set the current SDPORT
(defun sd-set-sd-port (sd-new-sd-port)
  "To set the current value of SDPORT, type \\[sd-set-sd-port].

This will change the current server from the previous server to the new
given value.

Argument SD-NEW-SD-PORT The new server:port to set to. The default value is
the current value of SDPORT."
  (interactive (list (let
             ((symbol (read-string "Change server:port to: "
                           sd-global-server-port)))
               (if (equal symbol "")
               sd-global-server-port
             symbol))))
  (if (or (null sd-new-sd-port) (equal sd-new-sd-port "nil"))
      (progn
    (setenv "SDPORT"  nil)
    (message
     "SD Version check disabled. Set a valid client name to enable."))
    (progn
      (setenv "SDPORT"  sd-new-sd-port)
      (setq sd-global-server-port sd-new-sd-port)
      (message  "SDPORT changed to %s" sd-new-sd-port))))

;; The find-file hook for sd.
(defun sd-find-file-hook ()
  "To check while loading the file, if it is a SD version controlled file."
  (if (or sd-global-config sd-global-clt)
      (sd-detect-sd)))

;; The kill-buffer hook for sd.
(defun sd-kill-buffer-hook ()
  "To Remove a file and its associated buffer from out global list of SD
controlled files."
  (if sd-vc-check
      (let ((buffile buffer-file-name)
        (bufname (buffer-name)))
    (sd-refresh-refresh-list buffile bufname))))

(defun sd-refresh-refresh-list (buffile bufname)
  "Refresh the list of files to be refreshed."
  (if sd-all-buffer-files
      (progn
    (setq sd-all-buffer-files (delete (list buffile bufname)
                      sd-all-buffer-files)))
    (progn
      (if (and sd-running-emacs (timerp sd-file-refresh-timer))
      (cancel-timer sd-file-refresh-timer))
      (if (and sd-running-xemacs sd-file-refresh-timer)
      (disable-timeout sd-file-refresh-timer))
      (if sd-file-refresh-timer
      (setq sd-file-refresh-timer nil)))))

;; A function to check if the file being opened is version controlled by sd.
(defun sd-is-vc ()
  "If a file is controlled by SD then return version else return nil."
  (let (filename max version)
    (setq filename buffer-file-name)
    (save-excursion
      (get-buffer-create sd-output-buffer-name)
      (set-buffer sd-output-buffer-name)
      (setq max (point-max))
      (goto-char max)
      (if filename
	  (setq default-directory (file-name-directory filename)))
      (if (and filename
	       (zerop (call-process
		       (sd-check-sd-executable)
		       nil
		       sd-output-buffer-name
		       nil
		       "have" (file-name-nondirectory filename))))
	  (progn
	    (set-buffer sd-output-buffer-name)
	    (goto-char max)
	    (if (re-search-forward "#[0-9]+" (point-max) t)
		(setq version (substring (match-string 0) 1)))))
      (set-buffer sd-output-buffer-name)
      (delete-region max (point-max))
      (if (and (null version)
	       filename
	       (zerop (call-process
		       (sd-check-sd-executable)
		       nil
		       sd-output-buffer-name
		       nil
		       "opened" filename)))
	  (progn
	    (set-buffer sd-output-buffer-name)
	    (goto-char max)
	    (if (re-search-forward "#[0-9]+" (point-max) t)
		(setq version (substring (match-string 0) 1)))
	    (goto-char max)
	    (if (re-search-forward "#[0-9]+ - add" (point-max) t)
		(setq version "Add"))))
      (set-buffer sd-output-buffer-name)
      (delete-region max (point-max)))
    (let ((sd-client-serv-info (sd-get-info "NULL" '("Client name"
						     "Server address"))))
      (setq sd-local-client nil sd-local-server-port nil)
      (if version (sd-assign-values '('sd-local-client 'sd-local-server-port)
				    sd-client-serv-info)))
    version))

;; To assign a list of values to a list of variables.
(defmacro sd-assign-value-macro (var val)
  "Macro to set a given value to a given variable."
  (list 'setq var val))

(defun sd-assign-values (vars vals)
  "Given a list of VARS and a corresponding list of VALS, assign the correct
value to the variable."
  (if (= (length vars) (length vals))
      (let (this-var this-val)
    (while vars
      (setq this-var (car vars))
      (setq vars (cdr vars))
      (setq this-val (car vals))
      (setq vals (cdr vals))
      (eval (macroexpand
         (list 'sd-assign-value-macro (eval this-var) this-val)))))
    (error "Lists don't match in length!")))

;; set keymap. We use the M-s M-d Keymap for all SourceDepot commands

(defvar sd-prefix-key "\M-s\M-d")

(defvar sd-prefix-map (lookup-key global-map sd-prefix-key)
  "The Prefix for SD Library Commands.")
(if (not (keymapp sd-prefix-map))
    (progn
      (setq sd-prefix-map (make-sparse-keymap))
      (define-key global-map sd-prefix-key sd-prefix-map)
      (define-key sd-prefix-map "a" 'sd-add)
      (define-key sd-prefix-map "b" 'sd-bug-report)
      (define-key sd-prefix-map "B" 'sd-branch)
      (define-key sd-prefix-map "c" 'sd-client)
      (define-key sd-prefix-map "C" 'sd-changes)
      (define-key sd-prefix-map "d" 'sd-diff2)
      (define-key sd-prefix-map "D" 'sd-describe)
      (define-key sd-prefix-map "e" 'sd-edit)
      (define-key sd-prefix-map "E" 'sd-reopen)
      (define-key sd-prefix-map "\C-f" 'sd-depot-find-file)
      (define-key sd-prefix-map "f" 'sd-filelog)
      (define-key sd-prefix-map "F" 'sd-files)
      (define-key sd-prefix-map "g" 'sd-get-client-name)
      (define-key sd-prefix-map "G" 'sd-get)
      (define-key sd-prefix-map "h" 'sd-help)
      (define-key sd-prefix-map "H" 'sd-have)
      (define-key sd-prefix-map "i" 'sd-info)
      (define-key sd-prefix-map "I" 'sd-integ)
      (define-key sd-prefix-map "j" 'sd-job)
      (define-key sd-prefix-map "J" 'sd-jobs)
      (define-key sd-prefix-map "l" 'sd-label)
      (define-key sd-prefix-map "L" 'sd-labels)
      (define-key sd-prefix-map "\C-l" 'sd-labelsync)
      (define-key sd-prefix-map "m" 'sd-rename)
      (define-key sd-prefix-map "n" 'sd-notify)
      (define-key sd-prefix-map "o" 'sd-opened)
      (define-key sd-prefix-map "p" 'sd-print)
      (define-key sd-prefix-map "P" 'sd-set-sd-port)
      (define-key sd-prefix-map "q" 'sd-pop-window-config)
      (define-key sd-prefix-map "r" 'sd-revert)
      (define-key sd-prefix-map "R" 'sd-refresh)
      (define-key sd-prefix-map "s" 'sd-set-client-name)
      (define-key sd-prefix-map "S" 'sd-submit)
      (define-key sd-prefix-map "t" 'sd-toggle-vc-mode)
      (define-key sd-prefix-map "u" 'sd-user)
      (define-key sd-prefix-map "U" 'sd-users)
      (define-key sd-prefix-map "v" 'sd-emacs-version)
      (define-key sd-prefix-map "V" 'sd-print-with-rev-history)
      (define-key sd-prefix-map "w" 'sd-where)
      (define-key sd-prefix-map "x" 'sd-delete)
      (define-key sd-prefix-map "X" 'sd-fix)
      (define-key sd-prefix-map "=" 'sd-diff)
      (define-key sd-prefix-map "-" 'sd-ediff)
      (define-key sd-prefix-map "?" 'sd-describe-bindings)))

;; For users interested in notifying a change, a notification list can be
;; set up using this function.
(defun sd-set-notify-list (sd-new-notify-list &optional sd-supress-stat)
  "To set the current value of SDNOTIFY, type \\[sd-set-notify-list].

This will change the current notify list from the existing list to the new
given value.

An empty string will disable notification.

Argument SD-NEW-NOTIFY-LIST is new value of the notification list.
Optional argument SD-SUPRESS-STAT when t will suppress display of the status
message. "

  (interactive (list (let
             ((symbol (read-string
                   "Change Notification List to: "
                   sd-notify-list)))
               (if (equal symbol "")
               nil
             symbol))))
  (setq sd-old-notify-list sd-notify-list)
  (if sd-new-notify-list
      (progn
    (setenv "SDNOTIFY"  sd-new-notify-list)
    (setq sd-notify-list sd-new-notify-list)
    (setq sd-notify t))
    (progn
      (setenv "SDNOTIFY"  nil)
      (setq sd-notify-list nil)
      (setq sd-notify nil)))
  (if (not sd-supress-stat)
      (message  "Notification list changed from '%s' to '%s'"
        sd-old-notify-list sd-notify-list)))

;; To get the current notification list.
(defun sd-get-notify-list ()
  "To get the current value of the environment variable SDNOTIFY,
type \\[sd-get-notify-list].

   This will be the current notification list that is in use for mailing
   change notifications through Emacs SD."

  (interactive)
  (message "SDNOTIFY is %s" sd-notify-list))

(defun sd-notify (users)
  "To notify a list of users of a change submission manually, type
\\[sd-notify].

To do auto-notification, set the notification list with `sd-set-notify-list'
and on each submission, the users in the list will be notified of the
change.

This uses the function in `sd-send-mail-function' to actually send the mail.

Also, it is mandatory to set the user's email address in the variable
`sd-user-email'.

Argument USERS The users to notify to. The default value is the notification
list."
  (interactive (list (let
             ((symbol (read-string "Notify whom? "
                           sd-notify-list)))
               (if (equal symbol "")
               nil
             symbol))))
  (sd-set-notify-list users t)
  (sd-do-notify))


(defun sd-do-send-mail () "\
Default function used by SD to send mail.
Uses the program defined in the variable `sd-sendmail-program'
to do all the work.
See also `sd-send-mail-function'."

  (if (not (and (eq sd-sendmail-program nil)
		(eq sd-user-email nil)))
      (call-process-region (point-min) (point-max)
			   sd-sendmail-program t t nil
			   "-odi" "-oi" sd-notify-list)
    (message "%s"
	     "Please set sd-sendmail-program and sd-user-email variables.")))


(defun sd-do-notify ()
  "This is the internal notification function called by `sd-notify'."
  (save-excursion
    (if (and sd-notify-list (not (equal sd-notify-list "")))
	(progn
	  (save-excursion
	    (set-buffer (get-buffer-create sd-output-buffer-name))
	    (goto-char (point-min))
	    (if (re-search-forward  "[0-9]+.*submitted" (point-max)  t)
		(progn
		  (let ((sd-matched-change 'nil))
		    (setq sd-matched-change (substring (match-string 0) 0 -10))
		    (set-buffer (get-buffer-create "*SD Notify*"))
		    (delete-region (point-min) (point-max))
		    (call-process-region (point-min) (point-max)
					 (sd-check-sd-executable)
					 t t nil "describe" "-s"
					 sd-matched-change)
		    (switch-to-buffer "*SD Notify*")
		    (goto-char (point-min))
		    (let ((sd-chg-desc 'nil))
		      (if (re-search-forward "^Change.*$" (point-max) t)
			  (setq sd-chg-desc (match-string 0))
			(setq sd-chg-desc (concat
					   "Notification of Change "
					   sd-matched-change)))
		      (goto-char (point-min))
		      (if (not (eq sd-user-email nil))
			  (progn
			    (insert
			     "From: " sd-user-email "\n"
			     "To: " sd-notify-list"\n"
			     "Subject: " sd-chg-desc "\n")
			    (funcall sd-send-mail-function))
			(message "%s" "Please set sd-user-email variable."))

		      (kill-buffer nil))))
	      (progn
		(save-excursion
		  (set-buffer (get-buffer-create sd-output-buffer-name))
		  (goto-char (point-max))
		  (insert "\nsd-do-notify: No Change Submissions found."))))))
      (progn
	(save-excursion
	  (set-buffer (get-buffer-create sd-output-buffer-name))
	  (goto-char (point-max))
	  (insert "\nsd-do-notify: Notification list not set."))))))

;; Function to return the current version.
(defun sd-emacs-version ()
  "Return the current Emacs-SD Integration version."
  (interactive)
  (message (concat (cond (sd-running-xemacs "X")) "Emacs-SD Integration v%s")
       sd-emacs-version))

(defun sd-check-sd-executable ()
  "Check if the `sd-executable' is nil, and if so, prompt the user for a
valid `sd-executable'."
  (interactive)
  (if (not sd-executable)
      (call-interactively 'sd-set-sd-executable)
    sd-executable))


;; To set the path to the sd executable
(defun sd-set-sd-executable (sd-exe-name)
  "Set the path to the correct SD Executable.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"sd\"\)
\(sd-set-sd-executable \"/my/path/to/sd\"\)

Argument SD-EXE-NAME The new value of the sd executable, with full path."
  (interactive "fFull path to your SD executable: " )
  (setq sd-executable sd-exe-name))

(defun sd-set-sendmail-program (sd-program)
  "Set the path to the correct sendmail.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"sd\"\)
\(sd-set-sendmail-program \"/my/path/to/sendmail\"\)

Argument SD-PROGRAM The full path to sendmail."
  (interactive "fFull path to the sendmail program: " )
  (setq sd-sendmail-program sd-program))

(defun sd-set-user-email (sd-email-address)

  "Set the correct user e-mail address to be used with the notification
system. This must be set for the notification to take place.

The default value is taken from the variable `user-mail-address', if it
exists. Otherwise, the value defaults to nil.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"sd\"\)
\(sd-set-user-email \"joe_user@somewhere.com\"\)

Argument SD-EMAIL-ADDRESS is the complete email address of the current
user."

  (interactive "sEnter your e-mail address: ")
  (setq sd-user-email sd-email-address))

(defun sd-detect-sd ()
  "Try to recursively go upwards from this directory and see if a file with
the name of the value of SDCONFIG is present. If so, then this is a SD
controlled file. Only check if `sd-use-sdconfig-exclusively' is non-nil."
  (if (not sd-use-sdconfig-exclusively)
      ;; no, always call
      (sd-check-mode)
    ;; yes, use it exclusively
    (and (getenv "SDCONFIG")
	 (let ((sdconfig (getenv "SDCONFIG"))
	       (sd-cfg-dir (cond (buffer-file-name ;; extrapolate from name
				  (file-name-directory
				   (file-truename (buffer-file-name))))
				 (t default-directory) ;; hmm, use default
				 ))
	       (win32 (if (memq system-type '(ms-dos windows-nt)) t nil)))
	   (while (not (or (string-equal sd-cfg-dir (char-to-string directory-sep-char))
			   (if win32 (string-match (concat ".:\\" (char-to-string directory-sep-char) "$") sd-cfg-dir) nil)
			   (file-exists-p (concat sd-cfg-dir sdconfig))))
	     (progn
;	       (message sd-cfg-dir)
;	       (message (concat "[^\\" (char-to-string directory-sep-char) "]*\\" (char-to-string directory-sep-char) "?$"))
	     (setq sd-cfg-dir
		   (substring sd-cfg-dir 0
			      (string-match (concat "[^\\" (char-to-string directory-sep-char) "]*\\" (char-to-string directory-sep-char) "?$") sd-cfg-dir)))
	     ))
	   ;; if we did find a sdconfig file, this is under SD control
	   (if (not (or (string-equal sd-cfg-dir (char-to-string directory-sep-char))
			(if win32 (string-match (concat ".:\\" (char-to-string directory-sep-char) "$") sd-cfg-dir) nil)))
	       (sd-check-mode)
	     nil)))))

(defun sd-check-mode (&optional args)
  "Check to see whether we should export the menu map to this buffer.

Optional argument ARGS Used only by `sd-add', the `sd-mode' variable is set
to this instead of the value returned from `sd-is-vc'.

Turning on SD mode calls the hooks in the variable `sd-mode-hook' with
no args."
  (if sd-do-find-file
      (progn
    (if args
        (setq sd-vc-check args)
      (setq sd-vc-check (sd-is-vc)))
    (if sd-vc-check
        (progn
          (sd-menu-add)
          (setq sd-mode (concat " SD:" sd-vc-check)))
      (setq sd-mode nil))
    (sd-force-mode-line-update)
    (let ((buffile buffer-file-name)
          (bufname (buffer-name)))
      (if (and sd-vc-check (not (member (list buffile bufname)
                        sd-all-buffer-files)))
          (add-to-list 'sd-all-buffer-files (list buffile bufname))))
    (if (not sd-file-refresh-timer)
        (setq sd-file-refresh-timer
          (cond (sd-running-emacs
             (run-at-time nil sd-file-refresh-timer-time
                      'sd-refresh-files-in-buffers))
            (sd-running-xemacs
             (add-timeout sd-file-refresh-timer-time
                      'sd-refresh-files-in-buffers nil
                      sd-file-refresh-timer-time)))))
    ;; run hooks
    (and sd-vc-check (run-hooks 'sd-mode-hook))
    sd-vc-check)))

(defun sd-refresh-files-in-buffers (&optional arg)
  "Check to see if all the files that are under SD version control are
actually up-to-date, if in buffers, or need refreshing."
  (let ((sd-all-my-files sd-all-buffer-files) buffile bufname thiselt)
    (if (not sd-all-my-files)
    (progn
      (if sd-file-refresh-timer
          (cond (sd-running-emacs
             (cancel-timer sd-file-refresh-timer))
            (sd-running-xemacs
             (disable-timeout sd-file-refresh-timer))))
      (setq sd-file-refresh-timer nil))
      (while sd-all-my-files
    (setq thiselt (car sd-all-my-files))
    (setq sd-all-my-files (cdr sd-all-my-files))
    (setq buffile (car thiselt))
    (setq bufname (cadr thiselt))
    (if (buffer-live-p (get-buffer bufname))
        (save-excursion
          (let ((buf (get-buffer bufname)))
        (set-buffer buf)
        (if sd-auto-refresh
            (if (not (buffer-modified-p buf))
            (if (not (verify-visited-file-modtime buf))
                (if (file-readable-p buffile)
                (revert-buffer t t)
                  (sd-check-mode))))
          (if (file-readable-p buffile)
              (find-file-noselect buffile)
            (sd-check-mode)))
        (setq buffer-read-only (not (file-writable-p
                         (buffer-file-name))))))
      (sd-refresh-refresh-list buffile bufname))))))

(defun sd-check-mode-all-buffers ()
  "Call sd-check-mode for all buffers under SD version control"
  (let ((sd-all-my-files sd-all-buffer-files) buffile bufname thiselt)
    (while sd-all-my-files
      (setq thiselt (car sd-all-my-files))
      (setq sd-all-my-files (cdr sd-all-my-files))
      (setq buffile (car thiselt))
      (setq bufname (cadr thiselt))
      (if (buffer-live-p (get-buffer bufname))
      (save-excursion
        (set-buffer (get-buffer bufname))
        (sd-check-mode))
    (sd-refresh-refresh-list buffile bufname)))))

;; Force mode line updation for different Emacs versions
(defun sd-force-mode-line-update ()
  "To Force the mode line update for different flavors of Emacs."
  (cond (sd-running-xemacs
     (redraw-modeline))
    (sd-running-emacs
     (force-mode-line-update))))

;; In case, the SD server is not available, or when operating off-line, the
;; sd-find-file-hook becomes a pain... this functions toggles the use of the
;; hook when opening files.

(defun sd-toggle-vc-mode ()
  "In case, the SD server is not available, or when working off-line, toggle
the VC check on/off when opening files."
  (interactive)
  (setq sd-do-find-file (not sd-do-find-file))
  (message (concat "SD mode check " (if sd-do-find-file
                    "enabled."
                      "disabled."))))

;; Wrap C-x C-q to allow sd-edit/revert and also to ensure that
;; we don't stomp on vc-toggle-read-only.

(defun sd-toggle-read-only (&optional verbose)
  "If sd-mode is non-nil, \\[sd-toggle-read-only] toggles between `sd-edit'
and `sd-revert'.

If the current buffer's file is not under sd, then this function passes on
all the parameters to `vc-toggle-read-only'."
  (interactive "P")
  (if (and (boundp 'sd-mode) (not (eq sd-mode nil)))
      (if buffer-read-only
      (sd-edit verbose)
    (sd-revert verbose))
    (vc-toggle-read-only verbose)))

(defun sd-browse-web-page ()
  "Browse the sd.el web page."
  (interactive)
  (require 'browse-url)
  (browse-url sd-web-page))

;; The menu definition is in the XEmacs format. Emacs parses and converts
;; this definition to its own menu creation commands.

(defalias 'sd-toggle-vc-mode-off 'sd-toggle-vc-mode)
(defalias 'sd-toggle-vc-mode-on 'sd-toggle-vc-mode)

(defvar sd-menu-def
  '(["Add Current to SD" sd-add
     (and buffer-file-name (not sd-mode))]
    ["Check out/Edit"    sd-edit
     (and (sd-buffer-file-name-2) (or (not sd-mode) buffer-read-only))]
    ["Re-open"         sd-reopen
     (and (sd-buffer-file-name-2) (or (not sd-mode) (not buffer-read-only)))]
    ["Revert File"  sd-revert
     (and (sd-buffer-file-name-2) (or (not sd-mode) (not buffer-read-only)))]
    ["Delete File from Depot"  sd-delete
     (and (sd-buffer-file-name-2) (or (not sd-mode) buffer-read-only))]
    ["Rename Depot File" sd-rename
     (and (sd-buffer-file-name-2) (or (not sd-mode) buffer-read-only))]
    ["Submit Changes"  sd-submit t]
    ["--" nil nil]
    ["Find File using Depot Spec" sd-depot-find-file
     sd-do-find-file]
    ["--" nil nil]
    ["Show Opened Files"    sd-opened t]
    ["Filelog" sd-filelog (sd-buffer-file-name-2)]
    ["Changes" sd-changes t]
    ["Describe change" sd-describe t]
    ["--" nil nil]
    ["Diff 2 Versions" sd-diff2 (sd-buffer-file-name-2)]
    ["Diff Current" sd-diff t]
    ["Diff Current with Ediff"   sd-ediff
     (and buffer-file-name (not buffer-read-only) sd-mode)]
    ["--" nil nil]
    ["Print" sd-print (sd-buffer-file-name-2)]
    ["Print with revision history" sd-print-with-rev-history
     (sd-buffer-file-name-2)]
    ["--" nil nil]
    ["Edit a Branch Specification" sd-branch t]
    ["Edit a Label Specification" sd-label t]
    ["Edit a Client Specification" sd-client t]
    ["Edit a User Specification" sd-user t]
    ["--" nil nil]
    ["Show Version" sd-emacs-version t]
    ["Disable SD VC Check"  sd-toggle-vc-mode-off
     sd-do-find-file]
    ["Enable SD VC Check"    sd-toggle-vc-mode-on
     (not sd-do-find-file)]
    ["--" nil nil]
    ["Set SD Config"  sd-set-client-config sd-do-find-file]
    ["Get Current SD Config"  sd-get-client-config
     sd-do-find-file]
    ["--" nil nil]
    ["Set SD Client"  sd-set-client-name sd-do-find-file]
    ["Get Current SD Client"  sd-get-client-name
     sd-do-find-file]
    ["--" nil nil]
    ["Set SD Server/Port"    sd-set-sd-port sd-do-find-file]
    ["Get Current SD Server/Port"    sd-get-sd-port
     sd-do-find-file]
    ["--" nil nil]
    ["Set SD Notification List"  sd-set-notify-list
     sd-mode]
    ["Get SD Notification List"  sd-get-notify-list sd-notify]
    ["--" nil nil]
    ["Check for later versions of sd.el" sd-browse-web-page t]
    ["--" nil nil]
    ["Report Bug in sd.el"  sd-bug-report t])
  "The SD menu definition")

(cond (sd-running-xemacs
       ;; Menu Support for XEmacs
       (require 'easymenu)
       (defun sd-mode-menu (modestr)
     (cons modestr sd-menu-def)))

      (sd-running-emacs
       ;; Menu support for Emacs
       (or (lookup-key global-map [menu-bar])
       (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
       (defvar menu-bar-sd-menu (make-sparse-keymap "SD"))
       (setq menu-bar-final-items (cons 'sd-menu menu-bar-final-items))
       (define-key global-map [menu-bar sd-menu]
     (cons "SD" menu-bar-sd-menu))
       (let ((m (reverse sd-menu-def))
         (separator-number 0))
     (while m
       (let ((menu-text (elt (car m) 0))
         (menu-action (elt (car m) 1))
         (menu-pred (elt (car m) 2)))
         (if menu-action
         (progn
           (define-key menu-bar-sd-menu (vector menu-action)
             (cons menu-text menu-action))
           (put menu-action 'menu-enable menu-pred))
           (define-key menu-bar-sd-menu
         (vector (make-symbol
              (concat "separator-"
                  (int-to-string separator-number))))
         '("--"))
           (setq separator-number (1+ separator-number))))
       (setq m (cdr m))))))

(defun sd-menu-add ()
  "To add the SD menu bar button for files that are already not in
the SD depot or in the current client view.."
  (interactive)
  (cond (sd-running-xemacs
     (if (not (boundp 'sd-mode))
         (setq sd-mode nil))
     (easy-menu-add (sd-mode-menu "SD"))))
  t)

;; issue a message for users trying to use obsolete binding.
(if (not (lookup-key global-map "\C-xP"))
    (define-key global-map "\C-xP"
      `(lambda ()
     (interactive)
     (message
      "Obsolete key binding for SD commands.  Use M-s M-d instead."))))

(defun sd-bug-report ()
  (interactive)
  (if (string-match " 19\\." (emacs-version))
      ;; unfortunately GNU Emacs 19.x doesn't have compose-mail
      (mail nil sd-emacs-maintainer (concat "BUG REPORT: "
                        (sd-emacs-version)))
    (compose-mail sd-emacs-maintainer (concat "BUG REPORT: "
                          (sd-emacs-version))))
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
  ;; Insert warnings for novice users.
  (insert
   "This bug report will be sent to the SD-Emacs Integration Maintainer,\n"
   sd-emacs-maintainer "\n\n")
  (insert (concat (emacs-version) "\n\n"))
  (insert "A brief description of the problem and how to reproduce it:\n")
  (save-excursion
    (let ((message-buf (get-buffer
            (cond (sd-running-xemacs " *Message-Log*")
                  (sd-running-emacs "*Messages*")))))
      (if message-buf
      (let ((beg-pos nil)
        (end-pos (point-max)))
        (save-excursion
          (set-buffer message-buf)
          (goto-char end-pos)
          (forward-line -10)
          (setq beg-pos (point)))
        (insert "\n\nRecent messages:\n")
        (insert-buffer-substring message-buf beg-pos end-pos))))))

(defun sd-describe-bindings ()
  "A function to list the key bindings for the sd prefix map"
  (interactive)
  (save-excursion
    (sd-push-window-config)
    (let ((map (make-sparse-keymap))
      (sd-bindings-buffer "*SD key bindings*"))
      (get-buffer-create sd-bindings-buffer)
      (cond
       (sd-running-xemacs
    (set-buffer sd-bindings-buffer)
    (delete-region (point-min) (point-max))
    (insert "Key Bindings for SD Mode\n------------------------\n")
    (describe-bindings-internal sd-prefix-map))
       (sd-running-emacs
    (kill-buffer sd-bindings-buffer)
    (describe-bindings sd-prefix-key)
    (set-buffer "*Help*")
    (rename-buffer sd-bindings-buffer)))
      (define-key map "q"  'sd-quit-current-buffer)
      (use-local-map map)
      (display-buffer sd-bindings-buffer))))

;; Break up a string into a list of words
;; (sd-make-list-from-string "ab c de  f") -> ("ab" "c" "de" "f")
(defun sd-make-list-from-string (str)
  (let ((lst '()))
    (while (or (string-match "^ *\"\\([^\"]*\\)\"" str)
           (string-match "^ *\'\\([^\']*\\)\'" str)
           (string-match "^ *\\([^ ]+\\)" str))
      (setq lst (append lst (list (substring
                   str
                   (match-beginning 1)
                   (match-end 1)))))
      (setq str (substring str (match-end 0))))
    lst))

;; Return the file name associated with a buffer. If the real buffer file
;; name doesn't exist, try special filename tags set in some of the sd
;; buffers.
(defun sd-buffer-file-name-2 ()
  (cond 
    ((buffer-file-name))
	((get-char-property (point) 'link-client-name))
	((get-char-property (point) 'link-depot-name))
	((get-char-property (point) 'block-client-name))
	((get-char-property (point) 'block-depot-name))
	((if (and (fboundp 'dired-get-filename)
              (dired-get-filename nil t))
	     (dired-get-filename nil t)))))

(defvar sd-depot-filespec-history nil
  "History for sd-depot filespecs.")

(defvar sd-depot-completion-cache nil
  "Cache for `sd-depot-completion'.
It is a list of lists whose car is a filespec and
cdr is the list of anwers")

(defvar sd-branches-history nil
  "History for sd clients.")

(defvar sd-branches-completion-cache nil
  "Cache for `sd-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar sd-clients-history nil
  "History for sd clients.")

(defvar sd-clients-completion-cache nil
  "Cache for `sd-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar sd-jobs-completion-cache nil
  "Cache for `sd-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar sd-labels-history nil
  "History for sd clients.")

(defvar sd-labels-completion-cache nil
  "Cache for `sd-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar sd-users-completion-cache nil
  "Cache for `sd-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar sd-arg-string-history nil
  "History for sd command arguments")

(defun sd-depot-completion-search (filespec cmd)
  "Look into `sd-depot-completion-cache' for filespec.
Filespec is the candidate for completion, so the
exact file specification is \"filespec*\".

If found in cache, return a list whose car is FILESPEC and cdr is the list
of matches.
If not found in cache, return nil.
So the 'no match' answer is different from 'not in cache'."
  (let ((l (cond
        ((equal cmd "branches") sd-branches-completion-cache)
        ((equal cmd "clients") sd-clients-completion-cache)
        ((equal cmd "dirs") sd-depot-completion-cache)
        ((equal cmd "jobs") sd-jobs-completion-cache)
        ((equal cmd "labels") sd-labels-completion-cache)
        ((equal cmd "users") sd-users-completion-cache)))
    dir list)

    (if (and sd-cleanup-cache (not sd-timer))
    (setq sd-timer (cond (sd-running-emacs
                  (run-at-time sd-cleanup-time nil
                       'sd-cache-cleanup))
                 (sd-running-xemacs
                  (add-timeout sd-cleanup-time 'sd-cache-cleanup
                       nil nil)))))

    ;;(message "sd-depot-completion-search '%s'" filespec)
    (while l
      (if (string-match (concat "^" (car (car l)) "[^/]*$") filespec)
      (progn
        ;; filespec is included in cache
        (if (string= (car (car l)) filespec)
        (progn
          ;;(message "return complete list for %s" filespec)
          (setq list (cdr (car l))))
          ;;(message "build list for %s from %s" filespec (car (car l)))
          (setq dir (cdr (car l)))
          (while dir
        (if (string-match (concat "^" filespec) (car dir))
            (setq list (cons (car dir) list)))
        (setq dir (cdr dir))))
        (setq l nil
          list (cons filespec list))))
      (setq l (cdr l)))
    list))

(defun sd-cache-cleanup (&optional arg)
  "Cleanup all the completion caches."
  (message "Cleaning up the sd caches ...")
  (setq sd-branches-completion-cache nil)
  (setq sd-clients-completion-cache nil)
  (setq sd-depot-completion-cache nil)
  (setq sd-jobs-completion-cache nil)
  (setq sd-labels-completion-cache nil)
  (setq sd-users-completion-cache nil)
  (if (and sd-running-emacs (timerp sd-timer)) (cancel-timer sd-timer))
  (if (and sd-running-xemacs sd-timer) (disable-timeout sd-timer))
  (setq sd-timer nil)
  (message "Cleaning up the sd caches ... done."))

(defun sd-partial-cache-cleanup (type)
  "Cleanup a specific completion cache."
  (cond ((string= type "branch")
     (setq sd-branches-completion-cache nil))
    ((string= type "client")
     (setq sd-clients-completion-cache nil))
    ((or (string= type "submit") (string= type "change"))
     (setq sd-depot-completion-cache nil))
    ((string= type "job")
     (setq sd-jobs-completion-cache nil))
    ((string= type "label")
     (setq sd-labels-completion-cache nil))
    ((string= type "user")
     (setq sd-users-completion-cache nil))))

(defun sd-depot-output (command &optional args)
  "Executes sd command inside a buffer.
Returns the buffer."
  (let ((buffer (get-buffer-create sd-output-buffer-name)))
    (sd-exec-sd buffer (cons command args) t)
    buffer))

(defun sd-read-depot-output (buffer &optional regexp)
  "Reads first line of BUFFER and returns it.
Read lines are deleted from buffer.

If optional REGEXP is passed in, return the substring of the first line that
matched the REGEXP."

  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line)

    (let ((line (buffer-substring (point-min) (point))))
      (if (string= line "")
      nil
    (delete-region (point-min) (point))
    (if (and regexp (string-match regexp line))
        (setq line (substring line (match-beginning 1) (match-end 1))))

    ;; remove trailing newline
    (if (equal (substring line (1- (length line)) (length line)) "\n")
        (substring line 0 (1- (length line)))
      line)))))

(defun sd-depot-completion-build (filespec cmd)
  "Ask SourceDepot for a list of files and directories beginning with FILESPEC."
  (let (output-buffer line list)

    (cond
     ((equal cmd "branches")
      (message "Making %s completion list..." cmd)

      ;; List branches
      (setq output-buffer (sd-depot-output cmd))

      (while (setq line (sd-read-depot-output
             output-buffer
             "^Branch \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$"))
    (if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq sd-branches-completion-cache
        (cons (cons filespec list) sd-branches-completion-cache)))

     ((equal cmd "clients")
      (message "Making %s completion list..." cmd)

      ;; List clients
      (setq output-buffer (sd-depot-output cmd))

      (while (setq line (sd-read-depot-output
             output-buffer
             "^Client \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$"))
    (if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq sd-clients-completion-cache
        (cons (cons filespec list) sd-clients-completion-cache)))

     ((equal cmd "dirs")
      (message "Making sd completion list...")

      ;; List dirs
      (setq output-buffer (sd-depot-output cmd
                       (list (concat filespec "*"))))

      (while (setq line (sd-read-depot-output output-buffer))
    (if (not (string-match "no such file" line))
        (setq list (cons (concat line "/") list))))

      ;; List files
      (setq output-buffer (sd-depot-output "files"
                       (list (concat filespec "*"))))

      (while (setq line (sd-read-depot-output output-buffer))
    (if (string-match "^\\(.+\\)#[0-9]+ - " line)
        (setq list (cons (match-string 1 line) list))))

      ;; add to cache -  growing and growing ?
      (setq sd-depot-completion-cache
        (cons (cons filespec list) sd-depot-completion-cache)))

     ((equal cmd "jobs")
      (message "Making %s completion list..." cmd)

      ;; List jobs
      (setq output-buffer (sd-depot-output cmd))
      (while (setq line (sd-read-depot-output
             output-buffer
             "\\([^ \n]*\\) on [0-9][0-9][0-9][0-9]/.*$"))
    (if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq sd-jobs-completion-cache
        (cons (cons filespec list) sd-jobs-completion-cache)))

     ((equal cmd "labels")
      (message "Making %s completion list..." cmd)

      ;; List labels
      (setq output-buffer (sd-depot-output cmd))

      (while (setq line (sd-read-depot-output
             output-buffer
             "^Label \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$"))
    (if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq sd-labels-completion-cache
        (cons (cons filespec list) sd-labels-completion-cache)))

     ((equal cmd "users")
      (message "Making %s completion list..." cmd)

      ;; List users
      (setq output-buffer (sd-depot-output cmd))

      (while (setq line (sd-read-depot-output
             output-buffer
             "^\\([^ ]+\\).*$"))
    (if line (setq list (cons line list))))

      ;; add to cache -  growing and growing ?
      (setq sd-users-completion-cache
        (cons (cons filespec list) sd-users-completion-cache))))

    (message nil)
    (cons filespec list)))

(defun sd-completion-builder (type)
  `(lambda (string predicate action)
     ,(concat "Completion function for SourceDepot " type ".

Using the mouse in completion buffer on a client will select it
and exit, unlike standard selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection.")

     (let (list)
       ,(if (string= type "dirs")
	    ;; when testing for an exact match, remove trailing /
	    `(if (and (eq action 'lambda)
		      (eq (aref string (1- (length string))) ?/))
		 (setq string (substring string 0 (1- (length string))))))

       ;; First, look in cache
       (setq list (sd-depot-completion-search string ,type))

       ;; If not found in cache, build list.
       (if (not list)
       (setq list (sd-depot-completion-build string ,type)))

       (cond
	;; try completion
	((null action)
	 (try-completion string (mapcar 'list (cdr list)) predicate))
	;; all completions
	((eq action t)
	 (let ((lst
		(all-completions string (mapcar 'list (cdr list)) predicate)))
	   ,(if (string= type "dirs")
		`(setq lst (mapcar (lambda (s)
				     (if (string-match ".*/\\(.+\\)" s)
					 (match-string 1 s)
				       s))
				   lst)))
	   lst))
	;; Test for an exact match
	(t
	 (and (>= (length list) 2)
	      (member (car list) (cdr list))))))))

(defalias 'sd-branches-completion (sd-completion-builder "branches"))
(defalias 'sd-clients-completion (sd-completion-builder "clients"))
(defalias 'sd-depot-completion (sd-completion-builder "dirs"))
(defalias 'sd-jobs-completion (sd-completion-builder "jobs"))
(defalias 'sd-labels-completion (sd-completion-builder "labels"))
(defalias 'sd-users-completion (sd-completion-builder "users"))


(defun sd-read-arg-string (prompt &optional initial type)
  (let ((minibuffer-local-completion-map
     (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (completing-read prompt
             (cond ((not type)
                'sd-arg-string-completion)
               ((string= type "branch")
                'sd-branch-string-completion)
               ((string= type "client")
                'sd-client-string-completion)
               ((string= type "label")
                'sd-label-string-completion)
               ((string= type "job")
                'sd-job-string-completion)
               ((string= type "user")
                'sd-user-string-completion))
             nil nil
             initial 'sd-arg-string-history)))

(defun sd-arg-string-completion (string predicate action)
  (let ((first-part "") completion)
    (if (string-match "^\\(.* +\\)\\(.*\\)" string)
    (progn
      (setq first-part (match-string 1 string))
      (setq string (match-string 2 string))))
    (cond ((string-match "-b +$" first-part)
       (setq completion (sd-branches-completion string predicate action)))
      ((string-match "-t +$" first-part)
       (let ((file-types (list "text " "xtext " "binary "
                   "xbinary " "symlink ")))
         (setq completion (sd-list-completion
                   string file-types predicate action))))
      ((string-match "-j +$" first-part)
       (setq completion (sd-jobs-completion string predicate action)))
      ((string-match "-l +$" first-part)
       (setq completion (sd-labels-completion string predicate action)))
      ((string-match "^status=" string)
       (let ((status-types (list "status=open " "status=closed "
                     "status=suspended ")))
         (setq completion (sd-list-completion
                   string status-types predicate action))))
      ((or (string-match "\\(.*@.+,\\)\\(.*\\)" string)
           (string-match "\\(.*@\\)\\(.*\\)" string))
       (setq first-part (concat first-part (match-string 1 string)))
       (setq string (match-string 2 string))
       (setq completion (sd-labels-completion string predicate action)))
      ((string-match "^//" string)
       (setq completion (sd-depot-completion string predicate action)))
      ((string-match "^-" string)
       (setq completion nil))
      (t
       (setq completion (sd-file-name-completion string
                             predicate action))))
    (cond ((null action) ;; try-completion
       (if (stringp completion)
           (concat first-part completion)
         completion))
      ((eq action t) ;; all-completions
       completion)
      (t             ;; exact match
       completion))))

(defun sd-list-completion (string lst predicate action)
  (let ((collection (mapcar 'list lst)))
    (cond ((not action)
       (try-completion string collection predicate))
      ((eq action t)
       (all-completions string collection predicate))
      (t
       (eq (try-completion string collection predicate) t)))))

(defun sd-file-name-completion (string predicate action)
  (if (string-match "//\\(.*\\)" string)
      (setq string (concat "/" (match-string 1 string))))
  (setq string (substitute-in-file-name string))
  (setq string (expand-file-name string))
  (let ((dir-path "") completion)
    (if (string-match "^\\(.*[/\\]\\)\\(.*\\)" string)
    (progn
      (setq dir-path (match-string 1 string))
      (setq string (match-string 2 string))))
    (cond ((not action)
       (setq completion (file-name-completion string dir-path))
       (if (stringp completion)
           (concat dir-path completion)
         completion))
      ((eq action t)
       (file-name-all-completions string dir-path))
      (t
       (eq (file-name-completion string dir-path) t)))))

(defun sd-string-completion-builder (completion-function)
  `(lambda (string predicate action)
     (let ((first-part "") completion)
       (if (string-match "^\\(.* +\\)\\(.*\\)" string)
       (progn
         (setq first-part (match-string 1 string))
         (setq string (match-string 2 string))))
       (cond ((string-match "^-" string)
          (setq completion nil))
         (t
          (setq completion
            (,completion-function string predicate action))))
       (cond ((null action);; try-completion
          (if (stringp completion)
          (concat first-part completion)
        completion))
         ((eq action t);; all-completions
          completion)
         (t;; exact match
          completion)))))

(defalias 'sd-branch-string-completion (sd-string-completion-builder
					'sd-branches-completion))

(defalias 'sd-client-string-completion (sd-string-completion-builder
					'sd-clients-completion))

(defalias 'sd-job-string-completion (sd-string-completion-builder
				     'sd-jobs-completion))

(defalias 'sd-label-string-completion (sd-string-completion-builder
				       'sd-labels-completion))

(defalias 'sd-user-string-completion (sd-string-completion-builder
				      'sd-users-completion))

(defvar sd-server-version-cache nil)


(defun sd-get-server-version ()
  "To get the version number of the sd server."
  (let ((sd-port (sd-current-server-port))
	ser-ver pmin)
    (setq ser-ver (cdr (assoc sd-port sd-server-version-cache)))
    (if (not ser-ver)
	(save-excursion
	  (get-buffer-create sd-output-buffer-name)
	  (set-buffer sd-output-buffer-name)
	  (goto-char (point-max))
	  (setq pmin (point))
	  (if (zerop (call-process (sd-check-sd-executable) nil t nil "info"))
	      (progn
		(goto-char pmin)
;;		(re-search-forward
;;		 "^Server version: .*\/.*\/\\(\\([0-9]+\\)\.[0-9]+\\)\/.*(.*)$")
;;		(setq ser-ver (string-to-number (match-string 2)))
		(setq ser-ver 99)
		(setq sd-server-version-cache (cons (cons sd-port ser-ver)
						    sd-server-version-cache))
		(delete-region pmin (point-max))))))
    ser-ver))

(defun sd-map-depot-file (filespec &optional rmap)
  "Map a file in the depot on the current client.  If RMAP is t then the
depot mapping is returned, else the client mapping is returned."
  (interactive (list (completing-read "Enter filespec: "
				      'sd-depot-completion
				      nil nil
				      "//depot/"
				      'sd-depot-filespec-history)))
  (let (files sd-depot-buffer sd-server-version
	      (sd-client-root (sd-get-client-root (sd-current-client))))
    (if (not sd-client-root)
	nil
      (setq sd-server-version (sd-get-server-version))
      (if (memq system-type '(ms-dos windows-nt))
	  ;; For Windows, since the client root will be terminated with a \ as
	  ;; in c:\ or drive:\foo\bar\, we need to strip the trailing \ .
	  (let ((sd-clt-root-len (length sd-client-root)))
	    (setq sd-clt-root-len (1- sd-clt-root-len))
	    (setq sd-client-root (substring sd-client-root 0 sd-clt-root-len))
	    ))
      (setq sd-depot-buffer sd-output-buffer-name)
      (get-buffer-create sd-depot-buffer);; We do these two lines
      (kill-buffer sd-depot-buffer);; to ensure no duplicates
      (get-buffer-create sd-depot-buffer)
      (set-buffer sd-output-buffer-name)
      (delete-region (point-min) (point-max))
      (apply 'call-process
	     (sd-check-sd-executable) nil t nil "where" (list filespec))
      (goto-char (point-min))
      (if (< sd-server-version 98)
	  (progn
	    (while (re-search-forward
		    (concat "^\\([^ ]+\\) //" (sd-current-client)
			    "\\(.*\\)$") nil t)
	      (setq files (cons
			   (cons
			    (match-string 1)
			    (concat sd-client-root (match-string 2)))
			   files))))
	(progn
	  (while (re-search-forward
		  (concat "^\\([^ ]+\\) //\\([^ ]+\\) \\(.*\\)$") nil t)
	    (setq files (cons
			 (cons
			  (match-string 1)  (match-string 3)) files)))))
      (if files
	  (if rmap
	      (cdr (car files))
	    (car (car files)))))))

(defun sd-depot-find-file (file)
  (interactive (list (completing-read "Enter filespec: "
				      'sd-depot-completion
				      nil nil
				      "//depot/"
				      'sd-depot-filespec-history)))
  (let ((lfile (if file (sd-map-depot-file file t))))
    (if lfile (find-file lfile)
      (if (get-file-buffer file)
	  (switch-to-buffer-other-window file)
	(progn
	  (get-buffer-create file)
	  (set-buffer file)
	  (sd-noinput-buffer-action
	   "print" nil t (list  "-q" file))
	  (sd-activate-print-buffer file))))))

(defun sd-current-client ()
  "Get the current local client, or the global client, if that."
  (if sd-local-client sd-local-client sd-global-clt))


(defun sd-empty-diff-p ()
 "Return t if there exists a file opened for edit with an empty diff"
 (interactive)
 (let ((buffer (get-buffer-create " sd-edp-buf")))
   (save-excursion
     (set-buffer buffer)
     (erase-buffer)
     (sd-exec-sd buffer (list "diff" "-sr") t)
     (goto-char (point-min))
     (search-forward-regexp "^[a-zA-Z]:\\\\" nil t))))

(defun sd-current-server-port ()
  "Get the current local server:port address, or the global server:port, if
that."
  (if sd-local-server-port sd-local-server-port sd-global-server-port))


;;;###autoload
(if (where-is-internal 'vc-toggle-read-only)
    (substitute-key-definition 'vc-toggle-read-only 'sd-toggle-read-only
			       global-map))

(provide 'sd)

;;; sd.el ends here


