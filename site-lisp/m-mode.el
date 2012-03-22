;;; js2.el -- an improved JavaScript editing mode
;;;
;;; This file was auto-generated on Thu Apr 24 03:14:31 2008 from files:
;;;  js2-vars.el
;;;  js2-util.el
;;;  js2-scan.el
;;;  js2-messages.el
;;;  js2-ast.el
;;;  js2-highlight.el
;;;  js2-browse.el
;;;  js2-parse.el
;;;  js2-indent.el
;;;  js2-mode.el

;;; js2-mode.el --- an improved JavaScript editing mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Version: 20080424
;; Keywords:  javascript languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This JavaScript editing mode supports:
;;
;;  - the full JavaScript language through version 1.7
;;  - accurate syntax highlighting using a recursive-descent parser
;;  - syntax-error and strict-mode warning reporting
;;  - "bouncing" line indentation to choose among alternate indentation points
;;  - smart line-wrapping within comments (Emacs 22+) and strings
;;  - code folding:
;;    - show some or all function bodies as {...}
;;    - show some or all block comments as /*...*/
;;  - context-sensitive menu bar and popup menus
;;  - code browsing using the `imenu' package
;;  - typing helpers (e.g. inserting matching braces/parens)
;;  - many customization options
;;
;; It is only compatible with GNU Emacs versions 21 and higher (not XEmacs).
;;
;; Installation:
;;
;;  - put `js2.el' somewhere in your emacs load path
;;  - M-x byte-compile-file RET <path-to-js2.el> RET
;;    Note:  it will refuse to run unless byte-compiled
;;  - add these lines to your .emacs file:
;;    (autoload 'js2-mode "js2" nil t)
;;    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;
;; To customize how it works:
;;   M-x customize-group RET js2-mode RET
;;
;; The variable `js2-mode-version' is a date stamp.  When you upgrade
;; to a newer version, you must byte-compile the file again.
;;
;; Notes:
;;
;; This mode is different in many ways from standard Emacs language editing
;; modes, inasmuch as it attempts to be more like an IDE.  If this drives
;; you crazy, it IS possible to customize it to be more like other Emacs
;; editing modes.  Please customize the group `js2-mode' to see all of the
;; configuration options.
;;
;; Some of the functionality does not work in Emacs 21 -- upgrading to
;; Emacs 22 or higher will get you better results.  If you byte-compiled
;; js2.el with Emacs 21, you should re-compile it for Emacs 22.
;;
;; Unlike cc-engine based language modes, js2-mode's line-indentation is not
;; customizable.  It is a surprising amount of work to support customizable
;; indentation.  The current compromise is that the tab key lets you cycle among
;; various likely indentation points, similar to the behavior of python-mode.
;;
;; This mode does not yet work with mmm-mode ("multiple major modes" mode),
;; although it could possibly be made to do so with some effort.
;;
;; This code is part of a larger project, in progress, to enable writing
;; Emacs customizations in JavaScript.
;;
;; Please email bug reports and suggestions to the author, or submit them
;; at http://code.google.com/p/js2-mode/issues

;; TODO:
;;  - set a text prop on autoinserted delimiters and don't biff user-entered ones
;;  - clean up xml member-expr parsing
;;  - add in remaining Ecma strict-mode warnings
;;  - get more use out of the symbol table:
;;    - jump to declaration (put hyperlinks on all non-decl var usages?)
;;    - rename variable/function
;;    - warn on unused var
;;  - add some dabbrev-expansions for built-in keywords like finally, function
;;  - add at least some completion support, e.g. for built-ins
;;  - code formatting

;;; Code:
;;; js2-vars.el -- byte-compiler support for js2-mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;;; Code:
