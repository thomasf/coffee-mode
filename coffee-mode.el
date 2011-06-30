;;; coffee-mode.el --- Major mode to edit CoffeeScript files in Emacs

;; Copyright (C) 2010 Chris Wanstrath

;; Version 0.3.0
;; Keywords: CoffeeScript major mode
;; Author: Chris Wanstrath <chris@ozmm.org>
;; URL: http://github.com/defunkt/coffee-script

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; For commentary please see the README.md or
;; http://github.com/defunkt/coffee-mode#readme

;;; Installation

;; In your shell:

;;     $ cd ~/.emacs.d/vendor
;;     $ git clone git://github.com/defunkt/coffee-mode.git

;; In your emacs config:

;;     (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
;;     (require 'coffee-mode)

;;; Thanks

;; Major thanks to http://xahlee.org/emacs/elisp_syntax_coloring.html
;; the instructions.

;; Also thanks to Jason Blevins's markdown-mode.el and Steve Yegge's
;; js2-mode for guidance.

;; TODO:
;; - Execute {buffer,region,line} and show output in new buffer
;; - Make prototype accessor assignments like `String::length: -> 10` pretty.
;; - mirror-mode - close brackets and parens automatically

;;; Code:

(require 'comint)
(require 'easymenu)
(require 'font-lock)

(eval-when-compile
  (require 'cl))

;;
;; Customizable Variables
;;

(defconst coffee-mode-version "0.3.0"
  "The version of this `coffee-mode'.")

(defgroup coffee nil
  "A CoffeeScript major mode."
  :group 'languages)

(defcustom coffee-debug-mode nil
  "Whether to run in debug mode or not. Logs to `*Messages*'."
  :type 'boolean
  :group 'coffee-mode)

(defcustom coffee-js-mode 'js2-mode
  "The mode to use when viewing compiled JavaScript."
  :type 'string
  :group 'coffee)

(defcustom coffee-cleanup-whitespace t
  "Should we `delete-trailing-whitespace' on save? Probably."
  :type 'boolean
  :group 'coffee)

(defcustom coffee-command "coffee"
  "The CoffeeScript command used for evaluating code. Must be in your
path."
  :type 'string
  :group 'coffee)

(defcustom coffee-args-repl '("-i")
  "The command line arguments to pass to `coffee-command' to start a REPL."
  :type 'list
  :group 'coffee)

(defcustom coffee-args-compile-file '("-c")
  "The command line arguments to pass to `coffee-command' when compiling a file."
  :type 'list
  :group 'coffee)

(defcustom coffee-args-compile-region '("-spb")
  "The command line arguments to pass to `coffee-command' when compiling a region."
  :type 'list
  :group 'coffee)

(defcustom coffee-compiled-buffer-name "*coffee-compiled*"
  "The name of the scratch buffer used when compiling CoffeeScript."
  :type 'string
  :group 'coffee)

(defcustom coffee-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'coffee)

(defvar coffee-mode-hook nil
  "A hook for you to run your own code when the mode is loaded.")

(defvar coffee-mode-map (make-keymap)
  "Keymap for CoffeeScript major mode.")

;;
;; Macros
;;

(defmacro setd (var val)
  "Like setq but optionally logs the variable's value using `coffee-debug'."
  (if (and (boundp 'coffee-debug-mode) coffee-debug-mode)
      `(progn
         (coffee-debug "%s: %s" ',var ,val)
         (setq ,var ,val))
    `(setq ,var ,val)))

(defun coffee-debug (string &rest args)
  "Print a message when in debug mode."
  (when coffee-debug-mode
      (apply 'message (append (list string) args))))

(defmacro coffee-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))

;;
;; Commands
;;

(defun coffee-repl ()
  "Launch a CoffeeScript REPL using `coffee-command' as an inferior mode."
  (interactive)

  (unless (comint-check-proc "*CoffeeREPL*")
    (set-buffer
     (apply 'make-comint "CoffeeREPL"
            coffee-command nil coffee-args-repl)))

  (pop-to-buffer "*CoffeeREPL*")
  (ansi-color-for-comint-mode-on))

(defun coffee-compile-file ()
  "Compiles and saves the current file to disk. Doesn't open in a buffer.."
  (interactive)
  (let ((compiler-output (shell-command-to-string (coffee-command-compile (buffer-file-name)))))
    (if (string= compiler-output "")
        (message "Compiled and saved %s" (concat (substring (buffer-file-name) 0 -6) "js"))
      (message (car (split-string compiler-output "[\n\r]+"))))))

(defun coffee-compile-buffer ()
  "Compiles the current buffer and displays the JS in another buffer."
  (interactive)
  (save-excursion
    (coffee-compile-region (point-min) (point-max))))

(defun coffee-compile-region (start end)
  "Compiles a region and displays the JS in another buffer."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (apply 'call-process-region
         start end coffee-command nil
         (get-buffer-create coffee-compiled-buffer-name)
         nil
         coffee-args-compile-region)
  (switch-to-buffer (get-buffer coffee-compiled-buffer-name))
  (funcall coffee-js-mode)
  (goto-char (point-min)))

(defun coffee-show-version ()
  "Prints the `coffee-mode' version."
  (interactive)
  (message (concat "coffee-mode v" coffee-mode-version)))

(defun coffee-open-reference ()
  "Open browser to CoffeeScript reference."
  (interactive)
  (browse-url "http://jashkenas.github.com/coffee-script/"))

(defun coffee-open-node-reference ()
  "Open browser to node.js documentation."
  (interactive)
  (browse-url "http://nodejs.org/docs/"))

(defun coffee-open-github ()
  "Open browser to `coffee-mode' project on GithHub."
  (interactive)
  (browse-url "http://github.com/defunkt/coffee-mode"))

;;
;; Menubar
;;

(easy-menu-define coffee-mode-menu coffee-mode-map
  "Menu for CoffeeScript mode"
  '("CoffeeScript"
    ["Compile File" coffee-compile-file]
    ["Compile Buffer" coffee-compile-buffer]
    ["Compile Region" coffee-compile-region]
    ["REPL" coffee-repl]
    "---"
    ["CoffeeScript Reference" coffee-open-reference]
    ["node.js Reference" coffee-open-node-reference]
    ["coffee-mode on GitHub" coffee-open-github]
    ["Version" coffee-show-version]
    ))

;;
;; Define Language Syntax
;;

;; String literals
(defvar coffee-string-regexp "\"\\([^\\]\\|\\\\.\\)*?\"\\|'\\([^\\]\\|\\\\.\\)*?'")

;; Instance variables (implicit this)
(defvar coffee-this-regexp "@\\(\\w\\|_\\)*\\|this")

;; Prototype::access
(defvar coffee-prototype-regexp "\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\)::\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\):")

;; Assignment
(defvar coffee-assign-regexp "\\(\\(\\w\\|\\.\\|_\\|$\\)+?\s*\\)[:=]")

;; Lambda
(defvar coffee-lambda-regexp "\\(->\\|=>\\)")

;; Namespaces
(defvar coffee-namespace-regexp "\\b\\(class\\s +\\(\\S +\\)\\)\\b")

;; Booleans
(defvar coffee-boolean-regexp "\\b\\(true\\|false\\|yes\\|no\\|on\\|off\\|null\\)\\b")

;; Constant (just a convention)
(defvar coffee-constant-regexp "\\b\\([[:upper:]_]+\\)\\b")

;; Constructor
(defvar coffee-constructor-regexp "\\b\\<[[:upper:]]\\w*\\b")

;; Regular Expressions
(defvar coffee-regexp-regexp "\\/\\(\\\\.\\|\\[\\(\\\\.\\|.\\)+?\\]\\|[^/]\\)+?\\/")

;; JavaScript Keywords
(defvar coffee-js-keywords
      '("if" "else" "new" "return" "try" "catch"
        "finally" "throw" "break" "continue" "for" "in" "while"
        "delete" "instanceof" "typeof" "switch" "super" "extends"
        "class" "until" "loop"))

;; Reserved keywords either by JS or CS.
(defvar coffee-js-reserved
      '("case" "default" "do" "function" "var" "void" "with"
        "const" "let" "debugger" "enum" "export" "import" "native"
        "__extends" "__hasProp"))

;; CoffeeScript keywords.
(defvar coffee-cs-keywords
      '("then" "unless" "and" "or" "is"
        "isnt" "not" "of" "by" "where" "when"))

;; Regular expression combining the above three lists.
(defvar coffee-keywords-regexp (regexp-opt
                                (append
                                 coffee-js-reserved
                                 coffee-js-keywords
                                 coffee-cs-keywords) 'words))


;; Create the list for font-lock. Each class of keyword is given a
;; particular face.
(defvar coffee-font-lock-keywords ;; TODO: string interpolation, see ruby-mode
  ;; *Note*: order below matters. `coffee-keywords-regexp' goes last
  ;; because otherwise the keyword "state" in the function
  ;; "state_entry" would be highlighted.
  `((,coffee-string-regexp . font-lock-string-face)
    (,coffee-this-regexp . font-lock-variable-name-face)
    (,coffee-prototype-regexp . font-lock-variable-name-face)
    (,coffee-constant-regexp . font-lock-constant-face)
    (,coffee-assign-regexp . font-lock-variable-name-face)
    (,coffee-regexp-regexp . font-lock-string-face)
    (,coffee-boolean-regexp . font-lock-constant-face)
    (,coffee-keywords-regexp . font-lock-keyword-face)
    (,coffee-constructor-regexp . font-lock-type-face)
    (,coffee-lambda-regexp . font-lock-function-name-face)))

;;
;; Helper Functions
;;

(defun coffee-before-save ()
  "Hook run before file is saved. Deletes whitespace if
`coffee-cleanup-whitespace' is non-nil."
  (when coffee-cleanup-whitespace
    (delete-trailing-whitespace)))

(defun coffee-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))

(defun coffee-command-compile (file-name)
  "The `coffee-command' with args to compile a file."
  (mapconcat 'identity (append (list coffee-command) coffee-args-compile-file (list file-name)) " "))

;;
;; imenu support
;;

;; This is a pretty naive but workable way of doing it. First we look
;; for any lines that starting with `coffee-assign-regexp' that include
;; `coffee-lambda-regexp' then add those tokens to the list.
;;
;; Should cover cases like these:
;;
;; minus: (x, y) -> x - y
;; String::length: -> 10
;; block: ->
;;   print('potion')
;;
;; Next we look for any line that starts with `class' or
;; `coffee-assign-regexp' followed by `{` and drop into a
;; namespace. This means we search one indentation level deeper for
;; more assignments and add them to the alist prefixed with the
;; namespace name.
;;
;; Should cover cases like these:
;;
;; class Person
;;   print: ->
;;     print 'My name is ' + this.name + '.'
;;
;; class Policeman extends Person
;;   constructor: (rank) ->
;;     @rank: rank
;;   print: ->
;;     print 'My name is ' + this.name + " and I'm a " + this.rank + '.'
;;
;; TODO:
;; app = {
;;   window:  {width: 200, height: 200}
;;   para:    -> 'Welcome.'
;;   button:  -> 'OK'
;; }

(defun coffee-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (interactive)

  ;; This function is called within a `save-excursion' so we're safe.
  (goto-char (point-min))

  (let ((index-alist '()) assign pos indent ns-name ns-indent)
    ;; Go through every assignment that includes -> or => on the same
    ;; line or starts with `class'.
    (while (re-search-forward
            (concat "^\\(\\s *\\)"
                    "\\("
                      coffee-assign-regexp
                      ".+?"
                      coffee-lambda-regexp
                    "\\|"
                      coffee-namespace-regexp
                    "\\)")
            (point-max)
            t)

      (coffee-debug "Match: %s" (match-string 0))

      ;; If this is the start of a new namespace, save the namespace's
      ;; indentation level and name.
      (when (match-string 8)
        ;; Set the name.
        (setq ns-name (match-string 8))

        ;; If this is a class declaration, add :: to the namespace.
        (setq ns-name (concat ns-name "::"))

        ;; Save the indentation level.
        (setq ns-indent (length (match-string 1)))

        ;; Debug
        (coffee-debug "ns: Found %s with indent %s" ns-name ns-indent))

      ;; If this is an assignment, save the token being
      ;; assigned. `Please.print:` will be `Please.print`, `block:`
      ;; will be `block`, etc.
      (when (setq assign (match-string 3))
          ;; The position of the match in the buffer.
          (setq pos (match-beginning 3))

          ;; The indent level of this match
          (setq indent (length (match-string 1)))

          ;; If we're within the context of a namespace, add that to the
          ;; front of the assign, e.g.
          ;; constructor: => Policeman::constructor
          (when (and ns-name (> indent ns-indent))
            (setq assign (concat ns-name assign)))

          (coffee-debug "=: Found %s with indent %s" assign indent)

          ;; Clear the namespace if we're no longer indented deeper
          ;; than it.
          (when (and ns-name (<= indent ns-indent))
            (coffee-debug "ns: Clearing %s" ns-name)
            (setq ns-name nil)
            (setq ns-indent nil))

          ;; Add this to the alist. Done.
          (push (cons assign pos) index-alist)))

    ;; Return the alist.
    index-alist))

;;
;; Navigation
;;

;; same as in haml-mode
(defun coffee-forward-through-whitespace (&optional backward)
  "Move the point forward through any whitespace.
The point will move forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If BACKWARD is non-nil, move the point backward instead."

  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

;;
;; Indentation
;;

;; Indenters help determine whether the current line should be
;; indented further based on the content of the previous line. If a
;; line starts with `class', for instance, you're probably going to
;; want to indent the next line.

(defvar coffee-indenters-bol
  '("class" "for" "if" "unless" "else" "while" "until" "loop" "switch"
    "when" "try" "catch" "finally")
  "Keywords or syntax whose presence at the start of a line means the
next line should probably be indented.")

(defun coffee-indenters-bol-regexp ()
  "Builds a regexp out of `coffee-indenters-bol' words."
  (concat "^\\s-*" (regexp-opt coffee-indenters-bol 'words)))

(defvar coffee-indenters-eol '(?> ?{ ?\[)
  "Single characters at the end of a line that mean the next line
should probably be indented.")

(defun coffee-previous-line-is-comment ()
  "Returns `t' if the previous line is a CoffeeScript comment."
  (save-excursion
    (forward-line -1)
    (coffee-line-is-comment)))

(defun coffee-line-is-comment ()
  "Returns `t' if the current line is a CoffeeScript comment."
  (save-excursion
    (backward-to-indentation 0)
    (= (char-after) (string-to-char "#"))))

(defun coffee-indent-p ()
  "Returns t if the current line can have lines nested beneath it."

  (save-excursion
    (let ((indenter-at-bol) (indenter-at-eol))
      (when (looking-at (coffee-indenters-bol-regexp))
        (setd indenter-at-bol t))
      (when (not indenter-at-bol)
        (end-of-line)
        (when (some (lambda (char)
                        (= (char-before) char))
                      coffee-indenters-eol)
          (setd indenter-at-eol t)))
      (or indenter-at-bol indenter-at-eol))))

;; based on haml-mode
(defun coffee-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."

  (save-excursion
    (beginning-of-line)
    (if (bobp) (list 0 nil)
      (coffee-forward-through-whitespace t)
      (let ((indent (coffee-indent-p)))
        (cond
         ((consp indent) indent)
         ((integerp indent) (list indent t))
         (indent (list (+ (current-indentation) coffee-indent-offset) nil))
         (t (list (current-indentation) nil)))))))

;; based on haml-mode
(defun coffee-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`coffee-compute-indentation' and preserving the relative
indentation of the rest of the region.  START and END specify the
region to indent.

If this command is used multiple times in a row, it will cycle
between possible indentations."

  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) coffee-indent-offset) coffee-indent-offset)
             (car (coffee-compute-indentation)))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil))
  (error "")) ;; Terrible hack

;; based on haml-mode
(defun coffee-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `coffee-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")

  (let ((ci (current-indentation))
        (cc (current-column)))
    (destructuring-bind (need strict) (coffee-compute-indentation)
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (if (and (not strict) (equal last-command this-command) (/= ci 0))
            (indent-to (* (/ (- ci 1) coffee-indent-offset) coffee-indent-offset))
          (indent-to need))))
    (when (< (current-column) (current-indentation))
      (forward-to-indentation 0))))

;;
;; Define Major Mode
;;

(define-derived-mode coffee-mode fundamental-mode
  "coffee-mode"
  "Major mode for editing CoffeeScript..."

  ;; key bindings
  (define-key coffee-mode-map "\C-cb" 'coffee-compile-buffer)
  (define-key coffee-mode-map "\C-cr" 'coffee-compile-region)
  (define-key coffee-mode-map [remap comment-dwim] 'coffee-comment-dwim)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; perl style comment: "# ..."
  (modify-syntax-entry ?# "< b" coffee-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" coffee-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ;; single quote strings
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'coffee-indent-line)
  (set (make-local-variable 'indent-region-function) 'coffee-indent-region)
  (setq indent-tabs-mode nil)

  ;; imenu
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'coffee-imenu-create-index)

  ;; no tabs
  (setq indent-tabs-mode nil)

  ;; hooks
  (set (make-local-variable 'before-save-hook) 'coffee-before-save))

(provide 'coffee-mode)

;;
;; On Load
;;

;; Run coffee-mode for files ending in .coffee.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
