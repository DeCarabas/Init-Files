;;; tads-mode.el --- TADS mode for GNU Emacs v19 or later

;; Author: Stephen Granade <sgranade@phy.duke.edu>
;; Created: 3 Apr 1999
;; Version: 1.2
;; Keywords: languages

;; LCD Archive Entry:
;; tads2-mode|Stephen Granade|sgranade@phy.duke.edu|
;; Major mode for editing TADS programs|
;; 5-Nov-1999|1.2|~/modes/tads2-mode.el.Z|

;;; Copyright:

;; Copyright (c) by Stephen Granade 1999
;; Portions of this code were adapted from GNU Emacs C-mode, and are
;; copyright (c) 1985, 1986, 1987, 1992 by Free Software Foundation, Inc.
;; Other portions of this code were adapted from an earlier TADS mode
;; by Darin Johnson, and are copyright (c) 1994 by Darin Johnson.
;;
;; tads-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; tads-mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; TADS is an adventure programming language which can be found via
;; anonymous FTP at
;; /ftp.gmd.de:/if-archive/programming/tads/
;;
;; This major mode is based heavily on the standard EMACS C-mode.
;; Type `C-h m' within TADS mode for details.
;;
;; Special thanks to Matthew Amster-Burton and Dan Shiovitz, who tested
;; early versions of this mode, and to Dan Schmidt, who got filled
;; strings and imenu support working.
;;

;; Put this file somewhere on your load-path, and the following code in
;; your .emacs file:
;;
;;  (autoload 'tads2-mode "tads2-mode" "TADS 2 editing mode." t)
;;  (setq auto-mode-alist
;;        (append (list (cons "\\.t$" 'tads2-mode))
;;                auto-mode-alist))

;;; Code:


;;; General variables: --------------------------------------------------------

(defconst tads-mode-version "1.2")

(defvar tads-startup-message t
  "*Non-nil means display a message when TADS 2 mode is loaded.")

(defvar tads-no-c++-comments nil
  "*If t, C++-style comments \(//\) are not fontified or treated as comments.")

(defvar tads-strip-trailing-whitespace t
  "*If t (the default), delete any trailing whitespace when ENTER is pressed.")

(defvar tads-mode-abbrev-table nil)

(defvar tads-mode-map nil
  "Keymap used in TADS 2 mode.")

(if tads-mode-map nil
  (let ((map (make-sparse-keymap "TADS")))
    (setq tads-mode-map (make-sparse-keymap))
    (define-key tads-mode-map "\M-n" 'tads-next-object)
    (define-key tads-mode-map "\M-p" 'tads-prev-object)
    (define-key tads-mode-map "{" 'electric-tads-brace)
    (define-key tads-mode-map "}" 'electric-tads-brace)
    (define-key tads-mode-map ";" 'electric-tads-semi)
    (define-key tads-mode-map "#" 'electric-tads-sharp-sign)
    (define-key tads-mode-map "*" 'electric-tads-splat)
    (define-key tads-mode-map "\r" 'electric-tads-enter)
    (define-key tads-mode-map "\M-t" 'tads-inside-block-comment)
    (define-key tads-mode-map "\177" 'backward-delete-char-untabify)
    (define-key tads-mode-map "\t" 'tads-indent-command)
    (define-key tads-mode-map [menu-bar] (make-sparse-keymap))
    (define-key tads-mode-map [menu-bar tads] (cons "TADS" map))
    (define-key map [next-object] '("Next object" . tads-next-object))
    (define-key map [prev-object] '("Previous object" . tads-prev-object))
    (define-key map [separator1] '("--" . nil))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (put 'comment-region 'menu-enable 'mark-active)
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (put 'indent-region 'menu-enable 'mark-active)
    (define-key map [indent-line] '("Indent Line" . indent-for-tab-command))))


;;; Indentation parameters: ---------------------------------------------------

(defvar tads-indent-level 4
  "*Indentation of lines of block relative to first line of block.")

(defvar tads-label-offset -2
  "*Indentation of label relative to where it should be.")

(defvar tads-indent-continued-string t
  "*If t (the default), strings continued from the previous line
are indented.")

(defvar tads-continued-string-offset 1
  "*How much to indent continued strings by compared to the first line
of the string. This is only used if `tads-indent-continued-string' is
true.")

(defvar tads-continued-string-offset-from-line 2
  "*How much to indent continued strings by compared to the first line
of the command containing the string, if that command is not purely
the string itself. This is only used if `tads-indent-continued-string'
is false.")

(defvar tads-brace-imaginary-offset 0
  "*Imagined indentation of a TADS open brace that actually follows
a statement.")

(defvar tads-brace-offset 0
  "*Extra indentation of braces compared to other text in the same context.")

(defvar tads-continued-statement-offset 4
  "*Extra indentation for lines which do not begin new statements.")

(defvar tads-continued-brace-offset -4
  "*Extra indentation for substatements which begin with an open brace.
This is in addition to `tads-continued-statement-offset'.")

(defvar tads-indent-cont-statement 4
  "*Indentation of continuation relative to start of statement.")

(defvar tads-auto-indent-after-newline t
  "*If t (the default), automatically indent the next line after
RETURN is pressed.")

(defvar tads-tab-always-indent t
  "*If t (the default), always indent the current line when tab is pressed.")

(defvar tads-auto-newline nil
  "*If t, automatically add newlines before and after braces,
and after semicolons in TADS code. If you don't want a leading
newline before braces then use:
  (define-key tads-mode-map \"{\" 'electric-tads-semi)")


;;; Syntax variables: ---------------------------------------------------------

(defvar tads-mode-syntax-table nil
  "Syntax table used in TADS mode.")

(if tads-mode-syntax-table
    nil
  (setq tads-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" tads-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" tads-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" tads-mode-syntax-table)
  (modify-syntax-entry ?+ "." tads-mode-syntax-table)
  (modify-syntax-entry ?- "." tads-mode-syntax-table)
  (modify-syntax-entry ?= "." tads-mode-syntax-table)
  (modify-syntax-entry ?% "." tads-mode-syntax-table)
  (modify-syntax-entry ?< "." tads-mode-syntax-table)
  (modify-syntax-entry ?> "." tads-mode-syntax-table)
  (modify-syntax-entry ?& "." tads-mode-syntax-table)
  (modify-syntax-entry ?| "." tads-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" tads-mode-syntax-table)
  ;; any reason NOT to have _ as a word constituent?  Makes things simpler.
  (modify-syntax-entry ?_ "w" tads-mode-syntax-table)
  ;; C++ style comments
  (if tads-no-c++-comments
      ()
    (modify-syntax-entry ?/ ". 124" tads-mode-syntax-table)
    (modify-syntax-entry ?* ". 23b" tads-mode-syntax-table)
    (modify-syntax-entry ?\n ">" tads-mode-syntax-table)))

;(defvar tads-functions-list
;  '("addword" "askdo" "askfile" "askio" "caps" "car" "cdr"
;    "clearscreen" "cvtnum" "cvtstr" "datatype" "defined" "delword"
;    "endTurn" "execCommand" "fclose" "find" "firstobj" "firstsc" "fopen"
;    "fseek" "fseekof" "fwrite" "getarg" "getfuse" "gettime" "getwords"
;    "incturn" "input" "inputdialog" "inputevent" "inputkey" "inputline"
;    "isclass" "length" "logging" "lower" "nextobj" "nocaps" "notify"
;    "objwords" "outcapture" "parseAskobjIndirect" "parseNounList"
;    "parserDictLookup" "parserGetMe" "parserGetObj" "parserGetTokTypes"
;    "parserReplaceCommand" "parserResolveObjects" "parserSetMe"
;    "parserTokenize" "postAction" "preCommand" "proptype" "quit" "rand"
;    "randomize" "reGetGroup" "remdaemon" "remfuse" "reSearch"
;    "resourceExists" "restart" "restore" "rundaemons" "runfuses" "save"
;    "say" "setdaemon" "setfuse" "setit" "setOutputFilter" "setscore"
;    "setversion" "skipturn" "substr" "systemInfo" "timeDelay" "undo"
;    "unnotify" "upper" "verbInfo" "yorn")
;  "List of TADS built-in functions.")

;; A function to aid my own failing memory of how to print objects
;(defun tads-make-regexp ()
;  (interactive)
;  (insert (make-regexp tads-functions-list)))

;(defvar tads-keywords-list
;  '("abort" "argcount" "break" "continue" "delete" "do" "else" "exit" 
;    "exitobj" "for" "goto" "if" "inherited" "local" "modify" "new" "nil"
;    "pass" "replace" "return" "self" "switch" "true" "while")
;  "List of TADS keywords.")

;; The following regexps were made from the above commented lists using
;; Simon Marshall's make-regexp package (thanks, Gareth!).

(eval-and-compile

  (defvar tads-functions-regexp
    "\\<\\(a\\(ddword\\|sk\\(do\\|file\\|io\\)\\)\\|c\\(a\\(ps\\|r\\)\\|dr\\|learscreen\\|vt\\(num\\|str\\)\\)\\|d\\(atatype\\|e\\(fined\\|lword\\)\\)\\|e\\(ndTurn\\|xecCommand\\)\\|f\\(close\\|i\\(nd\\|rst\\(obj\\|sc\\)\\)\\|open\\|seek\\(\\|of\\)\\|write\\)\\|get\\(arg\\|fuse\\|time\\|words\\)\\|i\\(n\\(cturn\\|put\\(\\|dialog\\|event\\|key\\|line\\)\\)\\|sclass\\)\\|l\\(ength\\|o\\(gging\\|wer\\)\\)\\|n\\(extobj\\|o\\(caps\\|tify\\)\\)\\|o\\(bjwords\\|utcapture\\)\\|p\\(arse\\(AskobjIndirect\\|NounList\\|r\\(DictLookup\\|Get\\(Me\\|Obj\\|TokTypes\\)\\|Re\\(placeCommand\\|solveObjects\\)\\|SetMe\\|Tokenize\\)\\)\\|ostAction\\|r\\(eCommand\\|optype\\)\\)\\|quit\\|r\\(and\\(\\|omize\\)\\|e\\(GetGroup\\|Search\\|m\\(daemon\\|fuse\\)\\|s\\(ourceExists\\|t\\(art\\|ore\\)\\)\\)\\|un\\(daemons\\|fuses\\)\\)\\|s\\(a\\(ve\\|y\\)\\|et\\(OutputFilter\\|daemon\\|fuse\\|it\\|score\\|version\\)\\|kipturn\\|ubstr\\|ystemInfo\\)\\|timeDelay\\|u\\(n\\(do\\|notify\\)\\|pper\\)\\|verbInfo\\|yorn\\)\\>"
    "Regular expression matching a TADS function")

  (defvar tads-keywords-regexp
    "\\<\\(a\\(bort\\|rgcount\\)\\|break\\|continue\\|d\\(elete\\|o\\)\\|e\\(lse\\|xit\\(\\|obj\\)\\)\\|for\\|goto\\|i\\(f\\|nherited\\)\\|local\\|modify\\|n\\(ew\\|il\\)\\|pass\\|re\\(place\\|turn\\)\\|s\\(elf\\|witch\\)\\|true\\|while\\)\\>"
    "Regular expression matching a TADS reserved word"))

;; A note: tads-label-regexp and tads-modified-regexp will NOT match
;; function definitions with returns between the label name and colon, like
;; bedroom_door
;;             : doorway
;; I don't know of anyone who uses this syntax, but someone might. If you
;; do, remove the '\n' from tads-label-regexp and tads-modified-regexp.

;; Regexp for finding a label or class name followed by a colon
;; Note that this should *not* match "default:", nor should it match
;; ":=" (for those of you still using the Pascal-style assignment operator)
(defvar tads-label-regexp "^[ \t]*\\(class \\)?\\([^:;\"!*(\n ]+ *\\):\\($\\|[^=]\\)")

;; Regexp for finding a modified object
(defvar tads-modified-regexp "^[ \t]*\\(modify\\|replace\\)\\s-+\\([^:;\"!*(\n ]+\\)")

;; Regexp for some TADS special words
(defvar tads-specials-regexp
  "^[ \t]*\\(compoundWord\\b\\|formatstring\\b\\|specialWords\\b\\)")

;; A combination of the above three regexps
(defvar tads-defun-regexp
  (concat
   "\\("
    tads-label-regexp
    "\\|"
    tads-modified-regexp
    "\\|"
    tads-specials-regexp
    "\\)"))

;; Regexp used internally to recognize labels in switch statements.
(defconst tads-switch-label-regexp "\\(case[ \t'/\(][^:]+\\|default[ \t]*\\):")


;;; Font-lock keywords: -------------------------------------------------------

(defvar tads-font-lock-defaults
  '(tads-font-lock-keywords nil t ((?_ . "w")) tads-prev-object)
  "Font Lock defaults for TADS mode.")

(defvar tads-font-lock-keywords
  (eval-when-compile
    (list

     ;; preprocessor directives as comments.
     '("^#[ \t]*[a-z]+" . font-lock-comment-face)
     '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)"
       1 font-lock-string-face)

     ;; objects and non-TADS functions
     '("^\\(\\w+[ \t]+\\)*\\(\\w+\\) *: *\\w+"
       2 font-lock-function-name-face)
     '("^[ \t]*modify \\(\\w+\\)"
       1 font-lock-function-name-face)

     ;; TADS keywords.
     (cons (concat "\\(" tads-keywords-regexp "\\)")
	   'font-lock-keyword-face)

     ;; TADS functions.
     (cons (concat "\\(" tads-functions-regexp "\\)")
	   'font-lock-builtin-face)
  ))
  "Expressions to fontify in TADS mode.")


;;; TADS mode: ----------------------------------------------------------------

(defun tads-mode ()
  "Major mode for editing TADS programs.

* TADS syntax:

  Type \\[indent-for-tab-command] to indent the current line.
  Type \\[indent-region] to indent the region.

* Navigating in a file:

  Type \\[tads-prev-object] to go to the previous object/class declaration.
  Type \\[tads-next-object] to go to the next one.

* Font-lock support:

  Put \(add-hook 'tads-mode-hook 'turn-on-font-lock) in your .emacs file.

*Key definitions:

\\{tads-mode-map}
* Miscellaneous user options:

  tads-startup-message
    Set to nil to inhibit the message printed the first time TADS
    mode is used.

  tads-auto-newline
    If true, automatically insert a newline before and after
    braces, and after colons and semicolons.

  tads-no-c++-comments
    Set to true to not treat C++-style comments \(//\) as comments.

  tads-strip-trailing-whitespace
    If true (the default), all whitespace at the end of a line will
    be removed when RETURN is pressed.

  tads-mode-hook
    The hook that is run after entering TADS mode.

* User options controlling indentation style:

  Values in parentheses are the default indentation style.

  tads-indent-level \(4\)
    Indentation of code inside an object relative to the first
    line of the block.

  tads-brace-offset \(0\)
    Extra indentation for a brace as compared to text in the same
    context.

  tads-brace-imaginary-offset \(0\)
    Imagined indentation for an open brace that follows a statement.

  tads-indent-cont-statement \(4\)
    Indentation of continuation relative to start of statement.

  tads-continued-statement-offset \(4\)
    Extra indentation for lines which do not begin new statements

  tads-continued-brace-offset \(-4\)
    Extra indentation for substatements which start with an open brace.
    This is in addition to `tads-continued-statement-offset'.

  tads-label-offset \(-2\)
    Extra indentation for line that is a label, or case or default.

  tads-indent-continued-string \(t\)
    If true, strings which span more than one line are all indented
    the same amount.

  tads-continued-string-offset \(1\)
    How much to indent continued strings by compared to the first line
    of the string. This is only used if `tads-indent-continued-string'
    is true.

  tads-continued-string-offset-from-line \(2\)
    How much to indent continued strings by compared to the first line
    of the command containing the string, if that command is not purely
    the string itself. This is only used if `tads-indent-continued-string'
    is false.

  tads-auto-indent-after-newline \(t\)
    If true, then pressing RETURN also indents the new line that is
    created.

  tads-tab-always-indents \(t\)
    If true, TAB always indents the current line when pressed.

  tads-auto-newline \(nil\)
    If true, automatically add newlines before and after braces,
    and after semicolons in TADS code. If you don't want a leading
    newline before braces then use:
      (define-key tads-mode-map \"{\" 'electric-tads-semi)"
  (interactive)
  (if tads-startup-message
      (message "Emacs TADS mode version %s by Stephen Granade."
             tads-mode-version))
  (kill-all-local-variables)
  (use-local-map tads-mode-map)
  (setq major-mode 'tads-mode)
  (setq mode-name "TADS")
  (setq local-abbrev-table tads-mode-abbrev-table)
  (set-syntax-table tads-mode-syntax-table)

  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function) 'tads-indent-line)
  (set (make-local-variable 'indent-region-function) 'tads-indent-region)
  (set (make-local-variable 'fill-paragraph-function) 'tads-fill-paragraph)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'tads-imenu-extract-name)
  (set (make-local-variable 'imenu-prev-index-position-function)
       'tads-prev-object)
  (set (make-local-variable 'require-final-newline) t)
  ;; The block mode comments are default
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|// *")
  (set (make-local-variable 'comment-indent-function) 'tads-comment-indent)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults) tads-font-lock-defaults)
  (run-hooks 'tads-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in C code
;; based on its context.
(defun tads-comment-indent ()
  (if (looking-at "^\\(/\\*\\|//\\)")
      0				;Existing comment at bol stays there.
    (let ((opoint (point)))
      (save-excursion
	(beginning-of-line)
	(cond ((looking-at "[ \t]*}[ \t]*\\($\\|/\\*\\|//\\)")
	       ;; A comment following a solitary close-brace
	       ;; should have only one space.
	       (search-forward "}")
	       (1+ (current-column)))
	      ((or (looking-at "^#[ \t]*endif[ \t]*")
		   (looking-at "^#[ \t]*else[ \t]*"))
	       7)			;2 spaces after #endif
	      ((progn
		 (goto-char opoint)
		 (skip-chars-backward " \t")
		 (and (= comment-column 0) (bolp)))
	       ;; If comment-column is 0, and nothing but space
	       ;; before the comment, align it at 0 rather than 1.
	       0)
	      (t
	       (max (1+ (current-column))   ;Else indent at comment column
		    comment-column)))))))   ; except leave at least one space.

(defun tads-indent-command (&optional whole-exp)
  "Indent current line as TADS code, or in some cases insert a tab character."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as TADS
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (tads-indent-line))
            beg end)
        (save-excursion
          (if tads-tab-always-indent
              (beginning-of-line))
          ;; Find beginning of following line.
          (save-excursion
            (forward-line 1) (setq beg (point)))
          ;; Find first beginning-of-sexp for sexp extending past this line.
          (while (< (point) beg)
            (forward-sexp 1)
            (setq end (point))
            (skip-chars-forward " \t\n")))
        (if (> end beg)
            (indent-code-rigidly beg end shift-amt "#")))
    ;; else just indent the one line
    (if (and (not tads-tab-always-indent)
             (save-excursion
               (skip-chars-backward " \t")
               (not (bolp))))
        (insert-tab)
      (tads-indent-line))))

(defun tads-indent-region (start end)
  (save-restriction
    (let ((endline (progn (goto-char (max end start))
			  (or (bolp) (end-of-line))
			  (point)))
	  linestart)
      (narrow-to-region (point-min) endline)
      (goto-char (min start end))
      (forward-line 0)
      (while (not (eobp))
	(tads-indent-line)
	(forward-line 1)))))

(defun tads-non-indented-string-indentation ()
  "Return indentation for the current string."
  (save-excursion
    (let ((start (1+ (re-search-backward "[^\\]\""))))
      (goto-char start)
      (+ (current-indentation)
         (if (progn (skip-chars-backward " \t")
                    (bolp))
             0
           tads-continued-string-offset-from-line)))))

(defun tads-indent-line ()
  "Indent current line as TADS code.
Return the amount the indentation changed by."
  (let ((indent (calculate-tads-indent nil))
        beg shift-amt
        (case-fold-search nil)
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   ;; string
	   (setq indent
		 (calculate-tads-indent-within-string)
		 ))
          ((eq indent t)
	   ;; comment
           (setq indent (calculate-tads-indent-within-comment)))
          ((looking-at "[ \t]*#")
	   ;; directive
           (setq indent 0))
          (t
           (if (listp indent)
	       (setq indent (car indent))
	     ;; Check special cases (don't do this if indent was a list,
	     ;; since that means we were at the top level, and these
	     ;; cases are only for C-style code)
	     (skip-chars-forward " \t")
	     (cond ((or (looking-at tads-switch-label-regexp)
			(and (looking-at "[A-Za-z]")
			     (save-excursion
			       (forward-sexp 1)
			       (looking-at ":"))))
		    (setq indent (max 1 (+ indent tads-label-offset))))
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (tads-backward-to-start-of-if)
				 (current-indentation))))
		 ((and (looking-at "}[ \t]*else\\b")
		       (not (looking-at "}[ \t]*else\\s_")))
		  (setq indent (save-excursion
				 (forward-char)
				 (backward-sexp)
				 (tads-backward-to-start-of-if)
				 (current-indentation))))
		 ((and (looking-at "while\\b")
		       (not (looking-at "while\\s_"))
		       (save-excursion
			 (tads-backward-to-start-of-do)))
		  ;; This is a `while' that ends a do-while.
		  (setq indent (save-excursion
				 (tads-backward-to-start-of-do)
				 (current-indentation))))
		 ((= (following-char) ?})
		  (setq indent (- indent tads-indent-level)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent tads-brace-offset)))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
        (if (> (- (point-max) pos) (point))
            (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))
    shift-amt))

;; quite different from the C-mode version
(defun calculate-tads-indent (&optional parse-start)
  "Return appropriate indentation for current line as TADS code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.
If indent is returned inside a list, this means we are at the top
level rather than being C-style code in a function body."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          (case-fold-search nil)
          state
          containing-sexp
	  next-char)
      (if parse-start
          (goto-char parse-start)
        (tads-beginning-of-defun)
	(setq parse-start (point)))
      (while (< (point) indent-point)
        (setq parse-start (point))
        (setq state (parse-partial-sexp (point) indent-point 0))
        (setq containing-sexp (car (cdr state))))
      ;; Now we've got some info, figure out what's up
      ;; State is: (paren-depth inner-list-start last-sexp instring incomment
      ;;            after-quote min-paren-depth)
      (cond ((or (nth 3 state) (nth 4 state))
	     (nth 4 state))		; Comment or string
	    ((null containing-sexp)
	     ;; We're at the top level.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     ;; returning a list, to flag us as top-level
	     (setq next-char (following-char))
	     (list
	      (cond ((or (= next-char ?\;) ; end of object def
			 (tads-looking-at-defun))
		     0)
		    ((progn
		       (tads-backward-to-noncomment parse-start)
		       (= (preceding-char) ?=)) ; continued property def
		     (+ (current-indentation)
			(if (= next-char ?{)
			    0		; starting a method
			    tads-continued-statement-offset))) ; continued propy
		    ;; check for start of function def (already checked
		    ;; if we're a continued property def)
		    ((= next-char ?{)
		     0)			; start of function body
		    ((and (= (current-indentation) 0)
			  (memq (preceding-char) '(?\; ?})))
		     ;; just after obj def or func def
		     0)
		    ((save-excursion
		       (beginning-of-line)
		       (tads-looking-at-defun)) ; first line after def'n
		     tads-indent-level)
		    (t
		     ;; Normal, non continued line (we hope)
		     ;; so use indentation of prev line (watching out
		     ;; for things that could span multiple lines)
		     (if (memq (preceding-char) '(?\} ?\" ?\'))
			 (progn
			   (backward-sexp 1)
			   (skip-chars-backward " \t\n")))
		     (current-indentation)))))
	    
	    ;; Not at top level - so we go back to doing C stuff
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement (i.e., we're
	     ;; inside parens or square brackets, not curlies),
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; We're part of a statement.  Continuation or new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (tads-backward-to-noncomment containing-sexp)
	     (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  tads-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (tads-backward-to-start-of-continued-exp containing-sexp)
		   (+ tads-continued-statement-offset (current-column)
                      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  tads-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		;; If no, find that first statement and indent like it.
		(save-excursion
		  (forward-char 1)
		  (while (progn (skip-chars-forward " \t\n")
				(looking-at
				 (concat
				  "#\\|/\\*\\|//"
				  "\\|case[ \t].*:"
				  "\\|[a-zA-Z0-9_$]*:")))
		    ;; Skip over comments and labels following openbrace.
		    (cond ((= (following-char) ?\#)
			   (forward-line 1))
			  ((looking-at "/\\*")
			   (forward-char 2)
			   (search-forward "*/" nil 'move))
			  ((looking-at "//")
			   (forward-line 1))
			  (t
			   (search-forward ":"))))
		  ;; The first following code counts
		  ;; if it is before the line we want to indent.
		  (and (< (point) indent-point)
		       (current-column)))
		;; If no previous statement,
		;; indent it relative to line brace is on.
		;; For open brace in column zero, don't let statement
		;; start there too.  If tads-indent-offset is zero,
		;; use tads-brace-offset + tads-continued-statement-offset
		;; instead.
		;; For open-braces not the first thing in a line,
		;; add in tads-brace-imaginary-offset.
		(+ (if (and (bolp) (zerop tads-indent-level))
		       (+ tads-brace-offset tads-continued-statement-offset)
		     tads-indent-level)
		   ;; Move back over whitespace before the openbrace.
		   ;; If openbrace is not first nonwhite thing on the line,
		   ;; add the tads-brace-imaginary-offset.
		   (progn (skip-chars-backward " \t")
			  (if (bolp) 0 tads-brace-imaginary-offset))
		   ;; If the openbrace is preceded by a parenthesized exp,
		   ;; move to the beginning of that;
		   ;; possibly a different line
		   (progn
		     (if (eq (preceding-char) ?\))
			 (forward-sexp -1))
		     ;; Get initial indentation of the line we are on.
		     (current-indentation))))))))))

(defun calculate-tads-indent-within-comment (&optional after-star)
  "Return the indentation amount for line inside a block comment.
Optional arg AFTER-STAR means, if lines in the comment have a leading star,
return the indentation of the text that would follow this star."
  (let (end star-start two-star)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*)
	    two-star (looking-at "\\*\\*"))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if after-star
	  (and (looking-at "\\*")
	       (re-search-forward "\\*[ \t]*")))
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (not after-star)
	   (goto-char (1+ (match-beginning 0)))
	   (if two-star
	       (backward-char))
	   (sit-for 1))
      (if (and (looking-at "[ \t]*$") (= (preceding-char) ?\*))
	  (1+ (current-column))
	(current-column)))))

(defun calculate-tads-indent-within-string ()
  "Return the indentation amount for line inside a string."
  (if (not tads-indent-continued-string)
      (tads-non-indented-string-indentation)
    (save-excursion
      (let ((beg-point (point))
	    parse-start)
	(tads-beginning-of-defun)
	(setq parse-start (point))
	(goto-char beg-point)
	;; now keep searching backwards until start of string
	;; (ugly)
	(while (nth 3
		    (parse-partial-sexp parse-start (point) nil))
	  (re-search-backward "\\s\"" nil t))
	(+ (current-column) tads-continued-string-offset)))))

(defun tads-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) '(?\) ?\"))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun tads-backward-to-start-of-if (&optional limit)
  "Move to the start of the last \"unbalanced\" `if'."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (and (not (bobp)) (not (zerop if-level)))
      (backward-sexp 1)
      (cond ((and (looking-at "else\\b")
		  (not (looking-at "else\\s_")))
	     (setq if-level (1+ if-level)))
	    ((and (looking-at "if\\b")
		  (not (looking-at "if\\s_")))
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun tads-backward-to-start-of-do (&optional limit)
  "If point follows a `do' statement, move to beginning of it and return t.
Otherwise return nil and don't move point."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((first t)
	(startpos (point))
	(done nil))
    (while (not done)
      (let ((next-start (point)))
	(condition-case nil
	    ;; Move back one token or one brace or paren group.
	    (backward-sexp 1)
	  ;; If we find an open-brace, we lose.
	  (error (setq done 'fail)))
	(if done
	    nil
	  ;; If we reached a `do', we win.
	  (if (looking-at "do\\b")
	      (setq done 'succeed)
	    ;; Otherwise, if we skipped a semicolon, we lose.
	    ;; (Exception: we can skip one semicolon before getting
	    ;; to a the last token of the statement, unless that token
	    ;; is a close brace.)
	    (if (save-excursion
		  (forward-sexp 1)
		  (or (and (not first) (= (preceding-char) ?}))
		      (search-forward ";" next-start t
				      (if (and first
					       (/= (preceding-char) ?}))
					  2 1))))
		(setq done 'fail)
	      (setq first nil)
	      ;; If we go too far back in the buffer, we lose.
	      (if (< (point) limit)
		  (setq done 'fail)))))))
    (if (eq done 'succeed)
	t
      (goto-char startpos)
      nil)))

(defun tads-beginning-of-defun ()
  (interactive)
  "Move either to what we think is start of TADS function or object, or,
if not found, to the start of the buffer."
  (beginning-of-line)
  (while (not (or (tads-looking-at-defun) (= (point) (point-min))))
    (and (re-search-backward (concat "^" tads-defun-regexp) nil 'move)
	 (goto-char (match-beginning 0)))))

(defun tads-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (setq opoint (point))
      (cond ((and (>= (point) (+ 2 lim))
		  (save-excursion
		    (forward-char -2)
		    (looking-at "\\*/")))
	     (search-backward "/*" lim 'move))
	    ((search-backward "//" (max lim (save-excursion
					      (beginning-of-line)
					      (point)))
			      'move))
	    (t (beginning-of-line)
	       (skip-chars-forward " \t")
	       (if (looking-at "#")
		   (setq stop (<= (point) lim))
		 (setq stop t)
		 (goto-char opoint)))))))

;; tells if we're at top level (or inside braces)
(defun tads-top-level ()
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  state)
      (tads-beginning-of-defun)
      (while (< (point) opoint)
	(setq state (parse-partial-sexp (point) opoint 0)))
      (null (car (cdr state))))))

;; fill a comment or a string
(defun tads-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handle C comments.
If any of the current line is a comment or within a comment,
fill the comment or the paragraph of it that point is in,
preserving the comment indentation or line-starting decorations."
  (interactive "P")
  (let* (comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t\n")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point))))))
    (if (save-excursion
	  (beginning-of-line)
	  (looking-at ".*//")) ;; handle c++-style comments
	(let (fill-prefix
	      (paragraph-start
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next to.
	       (concat
		paragraph-start
		"\\|[ \t]*/\\*[ \t]*$\\|[ \t]*\\*/[ \t]*$\\|[ \t/*]*$"))
	      (paragraph-separate
	       (concat
		paragraph-separate
		"\\|[ \t]*/\\*[ \t]*$\\|[ \t]*\\*/[ \t]*$\\|[ \t/*]*$")))
	  (save-excursion
	    (beginning-of-line)
	    ;; Move up to first line of this comment.
	    (while (and (not (bobp)) (looking-at "[ \t]*//"))
	      (forward-line -1))
	    (if (not (looking-at ".*//"))
		(forward-line 1))
	    ;; Find the comment start in this line.
	    (re-search-forward "[ \t]*//[ \t]*")
	    ;; Set the fill-prefix to be what all lines except the first
	    ;; should start with.
	    (let ((endcol (current-column)))
	      (skip-chars-backward " \t")
	      (setq fill-prefix
		    (concat (make-string (- (current-column) 2) ?\ )
			    "//"
			    (make-string (- endcol (current-column)) ?\ ))))
	    (save-restriction
	      ;; Narrow down to just the lines of this comment.
	      (narrow-to-region (point)
				(save-excursion
				  (forward-line 1)
				  (while (looking-at "[ \t]*//")
				    (forward-line 1))
				  (point)))
	      (insert fill-prefix)
	      (fill-paragraph arg)
	      (delete-region (point-min)
			     (+ (point-min) (length fill-prefix))))))
      (if (or first-line
	      ;; t if we enter a comment between start of function and
	      ;; this line.
	      (eq (calculate-tads-indent) t)
	      ;; t if this line contains a comment starter.
	      (setq first-line
		    (save-excursion
		      (beginning-of-line)
		      (prog1
			  (re-search-forward comment-start-skip
					     (save-excursion (end-of-line)
							     (point))
					     t)
			(setq comment-start-place (point))))))
	  ;; Inside a comment: fill one comment paragraph.
	  (let ((fill-prefix
		 ;; The prefix for each line of this paragraph
		 ;; is the appropriate part of the start of this line,
		 ;; up to the column at which text should be indented.
		 (save-excursion
		   (beginning-of-line)
		   (if (looking-at "[ \t]*/\\*.*\\*/")
		       (progn (re-search-forward comment-start-skip)
			      (make-string (current-column) ?\ ))
		     (if first-line (forward-line 1))

		     (let ((line-width (progn (end-of-line) (current-column))))
		       (beginning-of-line)
		       (prog1
			   (buffer-substring
			    (point)

			    ;; How shall we decide where the end of the
			    ;; fill-prefix is?
			    ;; calculate-tads-indent-within-comment
			    ;; bases its value on the indentation of
			    ;; previous lines; if they're indented
			    ;; specially, it could return a column
			    ;; that's well into the current line's
			    ;; text.  So we'll take at most that many
			    ;; space, tab, or * characters, and use
			    ;; that as our fill prefix.
			    (let ((max-prefix-end
				   (progn
				     (move-to-column
				      (calculate-tads-indent-within-comment t)
				      t)
				     (point))))
			      (beginning-of-line)
			      (skip-chars-forward " \t*" max-prefix-end)
			      ;; Don't include part of comment terminator
			      ;; in the fill-prefix.
			      (and (eq (following-char) ?/)
				   (eq (preceding-char) ?*)
				   (backward-char 1))
			      (point)))

			 ;; If the comment is only one line followed
			 ;; by a blank line, calling move-to-column
			 ;; above may have added some spaces and tabs
			 ;; to the end of the line; the fill-paragraph
			 ;; function will then delete it and the
			 ;; newline following it, so we'll lose a
			 ;; blank line when we shouldn't.  So delete
			 ;; anything move-to-column added to the end
			 ;; of the line.  We record the line width
			 ;; instead of the position of the old line
			 ;; end because move-to-column might break a
			 ;; tab into spaces, and the new characters
			 ;; introduced there shouldn't be deleted.

			 ;; If you can see a better way to do this,
			 ;; please make the change.  This seems very
			 ;; messy to me.
			 (delete-region (progn (move-to-column line-width)
					       (point))
					(progn (end-of-line) (point))))))))

		(paragraph-start
		 ;; Lines containing just a comment start or just an end
		 ;; should not be filled into paragraphs they are next to.
		 (concat
		  paragraph-start
		  "\\|[ \t]*/\\*[ \t]*$\\|[ \t]*\\*/[ \t]*$\\|[ \t/*]*$"))
		(paragraph-separate
		 (concat
		  paragraph-separate
		  "\\|[ \t]*/\\*[ \t]*$\\|[ \t]*\\*/[ \t]*$\\|[ \t/*]*$"))
		(chars-to-delete 0))
	    (save-restriction
	      ;; Don't fill the comment together with the code
	      ;; following it.  So temporarily exclude everything
	      ;; before the comment start, and everything after the
	      ;; line where the comment ends.  If comment-start-place
	      ;; is non-nil, the comment starter is there.  Otherwise,
	      ;; point is inside the comment.
	      (narrow-to-region (save-excursion
				  (if comment-start-place
				      (goto-char comment-start-place)
				    (search-backward "/*"))
				  ;; Protect text before the comment start
				  ;; by excluding it.  Add spaces to bring back
				  ;; proper indentation of that point.
				  (let ((column (current-column)))
				    (prog1 (point)
				      (setq chars-to-delete column)
				      (insert-char ?\  column))))
				(save-excursion
				  (if comment-start-place
				      (goto-char (+ comment-start-place 2)))
				  (search-forward "*/" nil 'move)
				  (forward-line 1)
				  (point)))
	      (save-excursion
		(goto-char (point-max))
		(forward-line -1)
		;; And comment terminator was on a separate line before,
		;; keep it that way.
		;; This also avoids another problem:
		;; if the fill-prefix ends in a *, it could eat up
		;; the * of the comment terminator.
		(if (looking-at "[ \t]*\\*/")
		    (narrow-to-region (point-min) (point))))
	      (fill-paragraph arg)
	      (save-excursion
		;; Delete the chars we inserted to avoid clobbering
		;; the stuff before the comment start.
		(goto-char (point-min))
		(if (> chars-to-delete 0)
		    (delete-region (point) (+ (point) chars-to-delete)))
		;; Find the comment ender (should be on last line of buffer,
		;; given the narrowing) and don't leave it on its own line.
		;; Do this with a fill command, so as to preserve sentence
		;; boundaries.
		(goto-char (point-max))
		(forward-line -1)
		(search-forward "*/" nil 'move)
		(beginning-of-line)
		(if (looking-at "[ \t]*\\*/")
		    (let ((fill-column (+ fill-column 9999)))
		      (forward-line -1)
		      (fill-region-as-paragraph (point) (point-max)))))))
	;; Outside of comments: do ordinary filling.
        (tads-fill-string-paragraph arg)))
    t))

;; To do : don't kill off double spacing
;;         don't get rid of returns before/after '\n' or '\b'
;;         calculate-tads-indent can get fooled by backslashes
;;
;; Largely hacked from Gareth Rees' inform-fill-paragraph by
;; Dan Schmidt
;;
(defun tads-fill-string-paragraph (&optional arg)
  "Fill a string according to our standards for string indentation."
  (interactive "P")
  (let* ((case-fold-search t)
         indent-type)
    (insert ?\n)
    (setq indent-type (calculate-tads-indent))
    (delete-backward-char 1)
    (if (eq indent-type nil)
        ;; string
        (let* ((indent-col (prog2
			       (insert ?\n)
			       (calculate-tads-indent-within-string)
			     (delete-backward-char 1)))
               (start (1+ (re-search-backward "[^\\]\"")))
               (end (progn (forward-char 1) (re-search-forward "[^\\]\"")))
               (fill-column (- fill-column 2))
               linebeg)
          (save-restriction
            (narrow-to-region (point-min) end)

            ;; Fold all the lines together, removing multiple spaces
            ;; as we go.
            (subst-char-in-region start end ?\n ? )
            (subst-char-in-region start end ?\t ? )
            (goto-char start)
            (while (re-search-forward "  +" end t)
              (delete-region (match-beginning 0) (1- (match-end 0))))

            ;; Split this line; reindent after first split,
            ;; otherwise indent to point where first split ended
            ;; up.
            (goto-char start)
            (setq linebeg start)
            (while (not (eobp))
              (move-to-column (1+ fill-column))
              (if (eobp)
                  nil
                (skip-chars-backward "^ " linebeg)
                (if (eq (point) linebeg)
                    (progn
                      (skip-chars-forward "^ ")
                      (skip-chars-forward " "))
		  (while (= (preceding-char) ?\ ) ; Get rid of any
		    (delete-backward-char 1)))    ; trailing spaces
                (insert ?\n)
                (indent-to-column indent-col 1)
                (setq linebeg (point)))))

          ;; Return T so that `fill-paragaph' doesn't try anything.
          t))))


;;; Miscellaneous: ------------------------------------------------------------

(defun tads-next-object (&optional arg)
  "Go to the next object or class declaration in the file.
With a prefix arg, go forward that many declarations.
With a negative prefix arg, search backwards."
  (interactive "P")
  (let* ((fun 're-search-forward)
	(errstring "more")
	(n (prefix-numeric-value arg))
	(forward-or-backward (if (< n 0) -1 1))
	(additional-n (if (and
			   (tads-looking-at-defun)
			   (not (< n 0))) 1 0))
        success                         ; did re-search-forward actually work?
	flag)
    (if (< n 0)
	   (setq fun 're-search-backward errstring "previous" n (- n)))
    ; loop until we're looking at a label which is *not* part of a switch
    (while
	(and
         ;; do the actual move, and put the cursor at column 0
         (setq success
               (prog1
                   (funcall fun tads-defun-regexp nil 'move (+ n additional-n))
                 (forward-line 0)))
         (looking-at (concat "^[ \t]*" tads-switch-label-regexp)))
      ;; This was really a switch label, keep going
      (forward-line forward-or-backward)
      (setq additional-n 0))
    ;; Return whether we succeeded
    success))

;; This function doubles as an `imenu-prev-name' function, so when
;; called noninteractively it must return non-NIL if it was successful and
;; NIL if not.  Argument NIL must correspond to moving backwards by 1.

(defun tads-prev-object (&optional arg)
  "Go to the previous object or class declaration in the file.
With a prefix arg, go back many declarations.
With a negative prefix arg, go forwards."
  (interactive "P")
  (tads-next-object (- (prefix-numeric-value arg))))

(defun tads-imenu-extract-name ()
  (cond ((looking-at "^\\(\\w+\\)\\s-*:\\s-*function\\(;\\)?")
         (if (not (match-string 2)) ; If it's a forward declaration, don't bite
             (concat "Function "
                     (buffer-substring-no-properties (match-beginning 1)
                                                     (match-end 1)))))
        ((looking-at "^\\(\\w+\\)\\s-*:")
         (buffer-substring-no-properties (match-beginning 1)
                                         (match-end 1)))
        ((looking-at "^\\(modify\\|replace\\)\\s-+\\(\\w+\\)")
         (concat (buffer-substring-no-properties (match-beginning 2)
                                                 (match-end 2))
                 "*"))
        ((looking-at "^class\\s-+\\(\\w+\\)\\s-*:")
         (concat "Class "
                 (buffer-substring-no-properties (match-beginning 1)
                                                 (match-end 1))))))

(defun tads-inside-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  state)
      (tads-beginning-of-defun)
      (while (< (point) opoint)
	(setq state (parse-partial-sexp (point) opoint)))
      (nth 4 state))))

(defun tads-inside-parens-p ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (beginning-of-defun) (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))

;; This function exists because it's very hard to come up with a regexp
;; which means, "match any label except 'default:'".
(defun tads-looking-at-defun ()
  (and (looking-at tads-defun-regexp)
       (not (looking-at "[ \t]*default[ \t]*:"))))


;;; Electric commands: --------------------------------------------------------

(defun electric-tads-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if tads-auto-newline
		     (progn (tads-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (tads-indent-line)
	  (if tads-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(tads-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-tads-splat (arg)
  "Insert character and correct line's indentation, if in a comment."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (tads-inside-comment)
      (tads-indent-line)))

(defun electric-tads-sharp-sign (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (save-excursion
	(skip-chars-backward " \t")
	(bolp))
      (let ((tads-auto-newline nil))
	(electric-tads-terminator arg))
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-tads-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if tads-auto-newline
      (electric-tads-terminator arg)
    (self-insert-command (prefix-numeric-value arg))
    (if (tads-top-level) (tads-indent-line))))

(defun electric-tads-enter (arg)
  (interactive "P")
  (if tads-strip-trailing-whitespace
      (delete-backward-char (- (save-excursion
				 (skip-chars-backward " \t")))))
  (newline)
  (if tads-auto-indent-after-newline (tads-indent-line)))

(defun electric-tads-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg) (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or (= (following-char) ?#)
			;; Colon is special only after a label, or case ....
			;; So quickly rule out most other uses of colon
			;; and do no indentation for them.
			(and (eq last-command-char ?:)
			     (not (looking-at tads-switch-label-regexp))
			     (save-excursion
			       (skip-chars-forward "a-zA-Z0-9_$")
			       (skip-chars-forward " \t")
			       (< (point) end)))
			(progn
			  (tads-beginning-of-defun)
			  (let ((pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (tads-indent-line)
	  (and tads-auto-newline
	       (not (tads-inside-parens-p))
	       (progn
		 (newline)
		 ;; (newline) may have done auto-fill
		 (setq insertpos (- (point) 2))
		 (tads-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

;;; tads-mode.el ends here
