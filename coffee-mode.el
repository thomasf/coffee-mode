;;; coffee-mode.el --- Major mode to edit CoffeeScript files in Emacs

;; ~~~~~~~~~~ coffee-mode 0.4.0 ~~~~~~~~~~~~
;; Copyright (C) 2010 Chris Wanstrath
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~ python-mode 6.0.3 ~~~~~~~~~~~~
;; Copyright (C) 1992,1993,1994  Tim Peters
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Version: 0.4.0
;; Keywords: CoffeeScript major mode
;; Author: Yasuaki Mitani <dr.ikros@gmail.com>
;;         (coffee-mode) Chris Wanstrath <chris@ozmm.org> 
;;         (python-mode) 2003-2011 https://launchpad.net/python-mode
;;         (python-mode) 1995-2002 Barry A. Warsaw 
;;         (python-mode) 1992-1994 Tim Peters
;; URL: http://github.com/defunkt/coffee-script

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; python-quate-syntax, py-electric-backspace, py-electric-delete 
;; are powerd by "python-mode"

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

(defcustom coffee-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :group 'coffee)

(defcustom coffee-command "coffee"
  "The CoffeeScript command used for evaluating code. Must be in your
path."
  :type 'string
  :group 'coffee)

(defcustom js2coffee-command "js2coffee"
  "The js2coffee command used for evaluating code. Must be in your
path."
  :type 'string
  :group 'coffee)


(defcustom coffee-args-repl '("-i")
  "The command line arguments to pass to `coffee-command' to start a REPL."
  :type 'list
  :group 'coffee)

(defcustom coffee-args-compile '("-c")
  "The command line arguments to pass to `coffee-command' when compiling a file."
  :type 'list
  :group 'coffee)

(defcustom coffee-cygwin-mode t
  "For Windows systems, add support for Cygwin-style absolute paths."
  :type 'boolean
  :group 'coffee)

(defcustom coffee-cygwin-prefix "/cygdrive/C"
  "The prefix with which to replace the drive-letter for your Windows partition, e.g. 'C:' would be replaced by '/c/cygdrive'."
  :type 'string
  :group 'coffee)

(defcustom coffee-compiled-buffer-name "*coffee-compiled*"
  "The name of the scratch buffer used when compiling CoffeeScript."
  :type 'string
  :group 'coffee)

(defcustom coffee-compile-jump-to-error t
  "Whether to jump to the first error if compilation fails.
Please note that the coffee compiler doesn't always give a line
number for the issue and in that case it is not possible to jump
to the error, of course."
  :type 'boolean
  :group 'coffee)

(defcustom coffee-watch-buffer-name "*coffee-watch*"
  "The name of the scratch buffer used when using the --watch flag with  CoffeeScript."
  :type 'string
  :group 'coffee)

(defvar coffee-mode-hook nil
  "A hook for you to run your own code when the mode is loaded.")

(defvar coffee-mode-map (make-keymap)
  "Keymap for CoffeeScript major mode.")

;;
;; Compat
;;

(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called."
    (lexical-let ((fun fun) (args1 args))
      (lambda (&rest args2) (apply fun (append args1 args2))))))

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

  (pop-to-buffer "*CoffeeREPL*"))

(defun coffee-compiled-file-name (&optional filename)
  "Returns the name of the JavaScript file compiled from a CoffeeScript file.
If FILENAME is omitted, the current buffer's file name is used."
  (concat (file-name-sans-extension (or filename (buffer-file-name))) ".js"))

(defun coffee-compile-file ()
  "Compiles and saves the current file to disk. Doesn't open in a buffer.."
  (interactive)
  (let ((compiler-output (shell-command-to-string (coffee-command-compile (buffer-file-name)))))
    (if (string= compiler-output "")
        (message "Compiled and saved %s" (coffee-compiled-file-name))
      (let* ((msg (car (split-string compiler-output "[\n\r]+")))
	     (line (and (string-match "on line \\([0-9]+\\)" msg)
			(string-to-number (match-string 1 msg)))))
	(message msg)
	(when (and coffee-compile-jump-to-error line (> line 0))
	  (goto-char (point-min))
	  (forward-line (1- line)))))))

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

  (apply (apply-partially 'call-process-region start end coffee-command nil
                          (get-buffer-create coffee-compiled-buffer-name)
                          nil)
         (append coffee-args-compile (list "-s" "-p")))
  (switch-to-buffer (get-buffer coffee-compiled-buffer-name))
  (funcall coffee-js-mode)
  (goto-char (point-min)))

(defun coffee-js2coffee-replace-region (start end)
  "Replace JS to coffee in current buffer."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (call-process-region start end 
                       js2coffee-command nil
                       (current-buffer)
                       )
  (delete-region start end)
  )

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

(defun coffee-watch (dir-or-file)
  "Run `coffee-run-cmd' with the --watch flag enabled for a directory or file"
  (interactive "fDirectory or File: ")
  (let ((coffee-compiled-buffer-name coffee-watch-buffer-name)
        (args (mapconcat 'identity (append coffee-args-compile (list "--watch" (coffee-universal-path dir-or-file))) " ")))
    (coffee-run-cmd args)))

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
(defvar coffee-assign-regexp "\\(\\(\\w\\|\\.\\|_\\|$\\)+?\s*\\):")

;; Lambda
(defvar coffee-lambda-regexp "\\((.+)\\)?\\s *\\(->\\|=>\\)")

;; Namespaces
(defvar coffee-namespace-regexp "\\b\\(class\\s +\\(\\S +\\)\\)\\b")

;; Booleans
(defvar coffee-boolean-regexp "\\b\\(true\\|false\\|yes\\|no\\|on\\|off\\|null\\|undefined\\)\\b")

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
;(defvar coffee-font-lock-keywords
;  ;; *Note*: order below matters. `coffee-keywords-regexp' goes last
;  ;; because otherwise the keyword "state" in the function
;  ;; "state_entry" would be highlighted.
(defvar coffee-font-lock-keywords
  (let ((keywords (mapconcat 'identity
							 '("if" "else" "new" "return" "try" "catch"
							   "finally" "throw" "break" "continue" "for" "in" "while"
							   "delete" "instanceof" "typeof" "switch" "super" "extends"
							   "class" "until" "loop" "then" "unless" "and" "or" "is"
							   "isnt" "not" "of" "by" "where" "when"
							   )
							 "\\|"))
		(builtin-func (mapconcat 'identity
								 '("escape" "unescape" "encodeURI" "decodeURI" "encodeURIComponent" "decodeURIComponent"
								   "eval" "parseInt" "parseFloat" "isNaN" "isFinite"
								   "setTimeout" "clearTimeout" "setInterval" "clearInterval")
								 "\\|"))
		(builtin-obj (mapconcat 'identity
								'("Number" "String" "Boolean" "Object" "Array" "Function" "RegExp" "Date" "Math" 
								  "window" "global" "export")
								"\\|"))
		(builtin-prop (mapconcat 'identity
								 '("prototype" "constructor" "toString" "valueOf" "toLocaleString" 
								   "hasOwnProperty" "propertyIsEnumerable" "isPropertyOf" "__proto__")
								 "\\|"))
		
		)
    (list
	 ; keywords
     (cons (concat "\\(^\\|[ \t\n]\\)\\(" keywords "\\)\\>") 2)
	 ; "this"
     '("\\(@\\(\\w\\|_\\)*\\|this\\)" 1 font-lock-variable-name-face)
	 ; prototype
     '("\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\)::\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\)" 1 font-lock-variable-name-face)
	 ; object attr
     '("\\(\\(\\w\\|\\.\\|_\\|$\\)+?\s*\\):" 1 font-lock-type-face)
	 ; constant
     '("\\b\\(true\\|false\\|yes\\|no\\|on\\|off\\|null\\|undefined\\)\\b" 1  font-lock-constant-face)
	 ; lambda function
     '("\\(\\(([^()]*)\\)?\\s *\\(->\\|=>\\)\\)" 1  font-lock-function-name-face)
	 ; number
     '("\\<\\(\\(0x\\)?[0-9]+\\(\\.[0-9]+\\(\\(+|-\\)?\\(e\\|E\\)[0-9]+\\)?\\)?\\)\\>" 1 font-lock-builtin-face)
	 ; builtin-func
     (list(concat "\\<\\(" builtin-func "\\)\\>[ \n\t(]") 1 'font-lock-builtin-face)
	 ; builtin-obj
     (list(concat "\\<\\(" builtin-obj "\\)\\>[ \n\t(.]") 1 'font-lock-builtin-face)
	 ; builtin-prop
	 (list(concat "\\<\\(" builtin-prop "\\)\\>[ \n\t(.]") 1 'font-lock-builtin-face)
     )
    )
  )

(unless (functionp 'string-to-syntax)
    (defun string-to-syntax (s)
      (cond
       ((equal s "|") '(15))
       ((equal s "_") '(3))
       (t (error "Unhandled string: %s" s))))
	)

(defconst coffee-font-lock-syntactic-keywords
  '(("[^\\]\\\\\\(?:\\\\\\\\\\)*\\(\\s\"\\)\\1\\(\\1\\)"
     (2
      (7)))
    ("\\(\\)\\(\\s\"\\)\\2\\(\\2\\)"
     (1
      (python-quote-syntax 1))
     (2
      (python-quote-syntax 2))
     (3
      (python-quote-syntax 3)))))

(defun python-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.) We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.
  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((font-lock-syntactic-keywords nil)
             (syntax (if (featurep 'xemacs)
                         (parse-partial-sexp (point-min) (point))
                       (syntax-ppss))))
        (when (eq t (nth 3 syntax))     ; after unclosed fence
          (goto-char (nth 8 syntax))    ; fence position
          ;; Is it a matching sequence?
          (if (eq (char-after) (char-after (match-beginning 2)))
              (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)                  ; leading quote (not prefix)
               (= (match-beginning 1) (match-end 1))) ; prefix is null
          (and (= n 1)                  ; prefix
               (/= (match-beginning 1) (match-end 1)))) ; non-empty
      (let ((font-lock-syntactic-keywords nil))
        (unless (eq 'string (syntax-ppss-context (if (featurep 'xemacs)
                                                     (parse-partial-sexp (point-min) (point))
                                                   (syntax-ppss))))
          ;; (eval-when-compile (string-to-syntax "|"))
          (eval-when-compile (string-to-syntax "|")))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

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

(defun coffee-cygwin-path (expanded-file-name)
  "Given an expanded file name, derive the absolute Cygwin path based on `coffee-cygwin-prefix'."
  (replace-regexp-in-string "^[a-zA-Z]:" coffee-cygwin-prefix expanded-file-name t))

(defun coffee-universal-path (file-name)
  "Handle different paths for different OS configurations for CoffeeScript"
  (let ((full-file-name (expand-file-name file-name)))
    (if (and (equal system-type 'windows-nt)
             coffee-cygwin-mode)
        (coffee-cygwin-path full-file-name)
      full-file-name)))

(defun coffee-command-compile (file-name)
  "The `coffee-command' with args to compile a file."
  (let ((full-file-name (coffee-universal-path file-name)))
    (mapconcat 'identity (append (list coffee-command) coffee-args-compile (list full-file-name)) " ")))

(defun coffee-run-cmd (args)
  "Given an arbitrary set of arguments for the `coffee-command', compile the command and show output in a custom compilation buffer."
  (interactive "sArguments: ")
  (let ((compilation-buffer-name-function (lambda (this-mode)
                                            (generate-new-buffer-name coffee-compiled-buffer-name))))
    (compile (concat coffee-command " " args))))

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
;; Indentation
;;

;;; The theory is explained in the README.

(defun coffee-indent-line ()
  "Indent current line as CoffeeScript."
  (interactive)

  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent 0) (cur-indent 0))
        ;; Figure out the indentation of the previous line
        (setd prev-indent (coffee-previous-indent))

        ;; Figure out the current line's indentation
        (setd cur-indent (current-indentation))

        ;; Shift one column to the left
        (beginning-of-line)
        (insert-tab)

        (coffee-debug "point: %s" (point))
        (coffee-debug "point-at-bol: %s" (point-at-bol))

        (when (= (point-at-bol) (point))
          (forward-char coffee-tab-width))

        (coffee-debug "New indent: %s" (current-indentation))

        ;; We're too far, remove all indentation.
        (when (> (- (current-indentation) prev-indent) coffee-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))

(defun coffee-previous-indent ()
  "Return the indentation level of the previous non-blank line."

  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (and (coffee-line-empty-p) (not (bobp))) (forward-line -1))
        (current-indentation)))))

(defun coffee-line-empty-p ()
  "Is this line empty? Returns non-nil if so, nil if not."
  (or (bobp)
   (string-match "^\\s *$" (coffee-line-as-string))))

(defun coffee-newline-and-indent ()
  "Inserts a newline and indents it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level,
  ;; insert a newline, and indent the newline to the same
  ;; level as the previous line.
  (let ((prev-indent (current-indentation)) (indent-next nil))
    (delete-horizontal-space t)
    (newline)
    (insert-tab (/ prev-indent coffee-tab-width))

    ;; We need to insert an additional tab because the last line was special.
    (when (coffee-line-wants-indent)
      (insert-tab))
	)

  ;; Last line was a comment so this one should probably be,
  ;; too. Makes it easy to write multi-line comments (like the one I'm
  ;; writing right now).
  (when (coffee-previous-line-is-comment)
    (insert "# ")))

;; Indenters help determine whether the current line should be
;; indented further based on the content of the previous line. If a
;; line starts with `class', for instance, you're probably going to
;; want to indent the next line.

(defvar coffee-indenters-bol '("class" "for" "while" "if" "else" "try" "catch" "finally" "switch" "when")
  "Keywords or syntax whose presence at the start of a line means the
next line should probably be indented.")

(defun coffee-indenters-bol-regexp ()
  "Builds a regexp out of `coffee-indenters-bol' words."
  (regexp-opt coffee-indenters-bol 'words))

(defvar coffee-indenters-eol '(?> ?{ ?\[ ?()
  "Single characters at the end of a line that mean the next line
should probably be indented.")

(defvar coffee-back-indenters-bol '(?} ?\] ?))
  "")

;; Electric deletion
(defun py-electric-backspace (&optional arg)
  "Delete preceding character or level of indentation.
With ARG do that ARG times. "
  (interactive "*p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (if (looking-back "^[ \t]+")
          (let* ((remains (% (current-column) coffee-tab-width)))
            (if (< 0 remains)
                (delete-char (- remains))
              (indent-line-to (- (current-indentation) coffee-tab-width))))
        (delete-char (- 1))))))

(defun py-electric-delete (&optional arg)
  "Delete preceding or following character or levels of whitespace.

With ARG do that ARG times. "
  (interactive "*p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (if (and (or (bolp)(looking-back "^[ \t]+")) (looking-at "[ \t]+"))
          (let* ((remains (% (+ (current-column) (- (match-end 0)(match-beginning 0))) coffee-tab-width)))
            (if (< 0 remains)
                (delete-char remains)
              (delete-char coffee-tab-width)))
        (delete-char 1)))))


(defun coffee-back-indent ()
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string coffee-tab-width ?\ )))
        (replace-match "")))))

(defun coffee-back-indent-region (start end)
  (interactive "r")
  (indent-rigidly start end (- coffee-tab-width))
  )

(defun coffee-line-wants-indent ()
  "Does the current line want to be indented deeper than the previous
line? Returns `t' or `nil'. See the README for more details."
  (interactive)
  
  (save-excursion
    (let ((indenter-at-bol) (indenter-at-eol))
      ;; Go back a line and to the first character.
      (forward-line -1)
      (backward-to-indentation 0)

      ;; If the next few characters match one of our magic indenter
      ;; keywords, we want to indent the line we were on originally.
      (when (looking-at (coffee-indenters-bol-regexp))
        (setd indenter-at-bol t))

      ;; If that didn't match, go to the back of the line and check to
      ;; see if the last character matches one of our indenter
      ;; characters.
      (when (not indenter-at-bol)
        (end-of-line)

        ;; Optimized for speed - checks only the last character.
        (when (some (lambda (char)
                        (= (char-before) char))
                      coffee-indenters-eol)
          (setd indenter-at-eol t)))

      ;; If we found an indenter, return `t'.
      (or indenter-at-bol indenter-at-eol))))



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

(defun set-coffee-tab-width (width)
  (interactive)
  (setq coffee-tab-width width)
  (set (make-local-variable 'tab-width) width)
  )

;;
;; Define Major Mode
;;

;;;###autoload
(define-derived-mode coffee-mode fundamental-mode
  "Coffee"
  "Major mode for editing CoffeeScript."

  ;; key bindings
  (define-key coffee-mode-map (kbd "A-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "A-R") 'coffee-compile-region)
  (define-key coffee-mode-map (kbd "A-M-r") 'coffee-repl)
  (define-key coffee-mode-map (kbd "<S-tab>") 'coffee-back-indent)
  (define-key coffee-mode-map [remap comment-dwim] 'coffee-comment-dwim)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent)
  (define-key coffee-mode-map "\C-c\C-o\C-s" 'coffee-cos-mode)
  (define-key coffee-mode-map [(delete)] 'py-electric-delete)
  (define-key coffee-mode-map [(backspace)] 'py-electric-backspace)
  (define-key coffee-mode-map [(control c)(<)] 'coffee-back-indent-region)
  (define-key coffee-mode-map [(control c)(>)] 'indent-region)

  ;; code for syntax highlighting
;  (setq font-lock-defaults '((coffee-font-lock-keywords)))
  (set (make-local-variable 'font-lock-defaults) 
	   '(coffee-font-lock-keywords nil nil nil nil 
									(font-lock-syntactic-keywords
									 . coffee-font-lock-syntactic-keywords)))
;  (set (make-local-variable 'font-lock-defaults)
;		 '(coffee-font-lock-keywords nil nil nil nil
;								 (font-lock-syntactic-keywords
;								  . coffee-font-lock-syntactic-keywords)))
;  (setq font-lock-syntactic-keywords '((coffee-font-lock-syntactic-keywords)))
  ;; perl style comment: "# ..."
  (modify-syntax-entry ?# "< b" coffee-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" coffee-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ;; single quote strings
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)
  (modify-syntax-entry ?/ "\"" coffee-mode-syntax-table)

  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'coffee-indent-line)
  (set (make-local-variable 'tab-width) coffee-tab-width)

  ;; imenu
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'coffee-imenu-create-index)

  ;; no tabs
  (setq indent-tabs-mode nil)

  ;; hooks
  (set (make-local-variable 'before-save-hook) 'coffee-before-save))

;;
;; Compile-on-Save minor mode
;;

(defvar coffee-cos-mode-line " CoS")
(make-variable-buffer-local 'coffee-cos-mode-line)

(define-minor-mode coffee-cos-mode
  "Toggle compile-on-save for coffee-mode."
  :group 'coffee-cos :lighter coffee-cos-mode-line
  (cond
   (coffee-cos-mode
    (add-hook 'after-save-hook 'coffee-compile-file nil t))
   (t
    (remove-hook 'after-save-hook 'coffee-compile-file t))))

(provide 'coffee-mode)

;;
;; On Load
;;

;; Run coffee-mode for files ending in .coffee.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
;;; coffee-mode.el ends here
