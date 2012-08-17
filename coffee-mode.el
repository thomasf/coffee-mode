;;; coffee-mode.el --- Major mode to edit CoffeeScript files in Emacs

;; Copyright (C) 2010 Chris Wanstrath

;; Version: -1
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

(defconst coffee-mode-version "-1"
  "The version of `coffee-mode'.")

(defgroup coffee nil
  "A CoffeeScript major mode."
  :group 'languages)

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

(defcustom coffee-mode-hook nil
  "Hook called by `coffee-mode'."
  :type 'hook
  :group 'coffee)

(defcustom coffee-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'coffee)

(defvar coffee-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "A-r") 'coffee-compile-buffer)
    (define-key map (kbd "A-R") 'coffee-compile-region)
    (define-key map (kbd "A-M-r") 'coffee-repl)
    (define-key map [remap comment-dwim] 'coffee-comment-dwim)
    (define-key map "\C-c\C-o\C-s" 'coffee-cos-mode)
    map)
  "Keymap for CoffeeScript major mode.")

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
  (let ((buffer-file-name "tmp.js")) (set-auto-mode))
  (goto-char (point-min)))

(defun coffee-js2coffee-replace-region (start end)
  "Replace JS to coffee in current buffer."
  (interactive "r")
  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))
  (call-process-region start end
                       js2coffee-command nil
                       (current-buffer))
  (delete-region start end))

(defun coffee-show-version ()
  "Prints the `coffee-mode' version."
  (interactive)
  (message (concat "coffee-mode v" coffee-mode-version)))

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
    ["Version" coffee-show-version]))

;;
;; Define Language Syntax
;;

;; String literals
(defvar coffee-string-regexp "\"\\([^\\]\\|\\\\.\\)*?\"\\|'\\([^\\]\\|\\\\.\\)*?'")

;; Instance variables (implicit this)
(defvar coffee-this-regexp "@\\(\\w\\|_\\)*\\|this")

;; Prototype::access
(defvar coffee-prototype-regexp
  "\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\)::\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\):")

;; Assignment
(defvar coffee-assign-regexp "\\(\\(\\w\\|\\.\\|_\\|$\\)+?\s*\\)[:=]")

;; Lambda
;;(defvar coffee-lambda-regexp "\\(\\(([^()]*)\\)?\\s *\\(->\\|=>\\)\\)")
(defvar coffee-lambda-regexp "\\(->\\|=>\\)")

;; Namespaces
(defvar coffee-namespace-regexp "\\b\\(class\\s +\\(\\S +\\)\\)\\b")

;; Booleans
(defvar coffee-boolean-regexp "\\b\\(true\\|false\\|yes\\|no\\|on\\|off\\|null\\|undefined\\)\\b")

;; Constant (just a convention)
(defvar coffee-constant-regexp "\\b\\([[:upper:]_]+\\)\\b")

;; Regular Expressions
(defvar coffee-regexp-regexp "\\/\\(\\\\.\\|\\[\\(\\\\.\\|.\\)+?\\]\\|[^/
]\\)+?\\/")

;; JavaScript Keywords
(defvar coffee-js-keywords
  '("if" "else" "new" "return" "try" "catch"
    "finally" "throw" "break" "continue" "for" "in" "while"
    "delete" "instanceof" "typeof" "switch" "super" "extends"
    "class" "until" "loop"))

;; Constructor
(defvar coffee-constructor-regexp "\\b\\<[[:upper:]]\\w*\\b")

;; Reserved keywords either by JS or CS.
(defvar coffee-js-reserved
  '("case" "default" "do" "function" "var" "void" "with"
    "const" "let" "debugger" "enum" "export" "import" "native"
    "__extends" "__hasProp"))

;; CoffeeScript keywords.
(defvar coffee-cs-keywords
  '("then" "unless" "and" "or" "is"
    "isnt" "not" "of" "by" "when" "own"))

;; Regular expression combining the above three lists.
(defvar coffee-keywords-regexp (regexp-opt
                                (append
                                 coffee-js-reserved
                                 coffee-js-keywords
                                 coffee-cs-keywords) 'words))

;; Create the list for font-lock. Each class of keyword is given a
;; particular face.
(defvar coffee-font-lock-keywords
  ;; *Note*: order below matters. `coffee-keywords-regexp' goes last
  ;; because otherwise the keyword "state" in the function
  ;; "state_entry" would be highlighted.
  `((,coffee-string-regexp . font-lock-string-face)
    (,coffee-this-regexp . font-lock-variable-name-face)
    (,coffee-prototype-regexp . font-lock-variable-name-face)
    (,coffee-constant-regexp . font-lock-constant-face)
    (,coffee-assign-regexp . font-lock-variable-name-face)
    (,coffee-regexp-regexp . font-lock-constant-face)
    (,coffee-boolean-regexp . font-lock-constant-face)
    (,coffee-keywords-regexp . font-lock-keyword-face)
    (,coffee-lambda-regexp . font-lock-function-name-face)
    (,coffee-constructor-regexp . font-lock-type-face)))

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

      ;; If this is the start of a new namespace, save the namespace's
      ;; indentation level and name.
      (when (match-string 8)
        ;; Set the name.
        (setq ns-name (match-string 8))
        ;; If this is a class declaration, add :: to the namespace.
        (setq ns-name (concat ns-name "::"))
        ;; Save the indentation level.
        (setq ns-indent (length (match-string 1))))
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
        ;; Clear the namespace if we're no longer indented deeper
        ;; than it.
        (when (and ns-name (<= indent ns-indent))
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

(defvar coffee-indenters-eol '(?> ?{ ?\[ ?=)
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
        (setq indenter-at-bol t))
      (when (not indenter-at-bol)
        (end-of-line)
        (when (some (lambda (char)
                      (= (char-before) char))
                    coffee-indenters-eol)
          (setq indenter-at-eol t)))
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
  (setq deactivate-mark nil))

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


(defun coffee-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; From python-mode...
  ;;
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.)  We also have to sort
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
             (syntax (syntax-ppss)))
        (when (eq t (nth 3 syntax))   ; after unclosed fence
          (goto-char (nth 8 syntax))  ; fence position
          ;; (skip-chars-forward "uUrR") ; skip any prefix
          ;; Is it a matching sequence?
          (if (eq (char-after) (char-after (match-beginning 2)))
              (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)        ; leading quote (not prefix)
               (not (match-end 1)))     ; prefix is null
          (and (= n 1)       ; prefix
               (match-end 1)))          ; non-empty
      (let ((font-lock-syntactic-keywords nil))
        (unless (eq 'string (syntax-ppss-context (syntax-ppss)))
          (eval-when-compile (string-to-syntax "|")))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

;;
;; Define Major Mode
;;

;;;###autoload
(define-derived-mode coffee-mode prog-mode "Coffee"
  "Major mode for editing CoffeeScript.

\\{coffee-mode-map}
Entry to this mode call the value of `coffee-mode-hook`
if that value is non-nil."

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; treat "_" as part of a word
  (modify-syntax-entry ?_ "w" coffee-mode-syntax-table)

  ;; perl style comment: "# ..."
  (modify-syntax-entry ?# "< b" coffee-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" coffee-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ;; single quote strings
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)

  (setq font-lock-syntactic-keywords
        ;; Make outer chars of matching triple-quote sequences into generic
        ;; string delimiters.
        ;; First avoid a sequence preceded by an odd number of backslashes.
        `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
                    "\\(?:\\('\\)\\('\\)\\('\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\)")
           (1 (coffee-quote-syntax 1) nil lax)
           (2 (coffee-quote-syntax 2))
           (3 (coffee-quote-syntax 3)))))

  ;; indentation
  (set (make-local-variable 'tab-width) coffee-tab-width)
  (set (make-local-variable 'indent-line-function) 'coffee-indent-line)
  (set (make-local-variable 'indent-region-function) 'coffee-indent-region)

  ;; imenu
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'coffee-imenu-create-index))

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
