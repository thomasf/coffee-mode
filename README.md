CoffeeScript Major Mode
=======================

An Emacs major mode for [CoffeeScript][cs], unfancy JavaScript.

Provides syntax highlighting, indentation support, imenu support,
a menu bar, and a few cute commands.

![Screenshot](http://img.skitch.com/20100308-fcr622c95ibey4m474d5m1m1qt.png)

## Installation

In your shell:

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/defunkt/coffee-mode.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
    (require 'coffee-mode)

If `coffee-mode` is not enabled automatically for any files ending in
".coffee" or named "Cakefile", add this to your emacs config as well:

    (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
    (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

## imenu

If you're using imenu, `coffee-mode` should work just fine. This
means users of [textmate.el][tm] will find that `⇧⌘T`
(`textmate-go-to-symbol`) mostly works as expected.

If you're not using imenu check out [this page][im] or textmate.el for
a really awesome way to jump quickly to a function's definition in a
file.

## Commands

If you have `easymenu` you can get to any of these commands from the
menu bar:

![coffee-mode menu bar](http://img.skitch.com/20100308-tt5yn51h2jww2pmjqaawed6eq8.png)

### coffee-compile-file

Compiles the current file as a JavaScript file. Doesn't open it or
anything special for you.

Operating on "basic.coffee" and running this command will save a
"basic.js" in the same directory. Subsequent runs will overwrite the
file.

If there are compilation errors and we the compiler have returned a
line number to us for the first error, the point is moved to that
line, so you can investigate.  If this annoys you, you can set
`coffee-compile-jump-to-error` to `nil`.

### coffee-compile-buffer

Compiles the current buffer to JavaScript using the command specified
by the `coffee-command` variable and opens the contents in a new
buffer using your JavaScript mode of choice. The JavaScript mode is
determined by the `coffee-js-mode` variable and defaults to `js2-mode`.

Bind it:

    (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

### coffee-compile-region

Compiles the selected region to JavaScript using the same
configuration variables as `coffee-compile-buffer`.

Bind it:

    (define-key coffee-mode-map [(meta R)] 'coffee-compile-region)

### Compile-on-save

Hitting the key sequence `C-c C-o C-s` turns on (toggles) the
compile-on-save minor mode in `coffee-mode`.  To enable it by default:

    (add-hook 'coffee-mode-hook '(lambda () (coffee-cos-mode t)))

### coffee-repl

Starts a repl in a new buffer using `coffee-command`.

## Hooks

### coffee-mode-hook

Naturally. Example:

    (defun coffee-custom ()
      "coffee-mode-hook"

      ;; CoffeeScript uses two spaces.
      (make-local-variable 'tab-width)
      (set 'tab-width 2)

      ;; If you don't want your compiled files to be wrapped
      (setq coffee-args-compile '("-c" "--bare"))

      ;; Emacs key binding
      (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

      ;; Riding edge.
      (setq coffee-command "~/dev/coffee")

      ;; Compile '.coffee' files on every save
      (and (file-exists-p (buffer-file-name))
           (file-exists-p (coffee-compiled-file-name))
           (coffee-cos-mode t)))

    (add-hook 'coffee-mode-hook 'coffee-custom)

## Configuration

You can customize any of the following options using `M-x
customize-group` with "coffee" as the group.

You can also customize then with `coffee-mode-hook`, as demonstrated
above.

### coffee-tab-width

The tab width to use when indenting.

Default: `tab-width`

### coffee-command

The CoffeeScript command used for evaluating code. Must be in your
path.

Default: `"coffee"`

### coffee-args-repl

The command line arguments to pass to `coffee-command' to start a
REPL.

Default: `'("-i")`

### coffee-args-compile

The command line arguments to pass to `coffee-command' when compiling a file.

Default: `'("-c")`

### coffee-compiled-buffer-name

The name of the scratch buffer used when compiling CoffeeScript.

Default: `"*coffee-compiled*"`

### coffee-compile-jump-to-error

Whether to jump to the first error if compilation fails.  Please note
that the coffee compiler doesn't always give a line number for the
issue and in that case it is not possible to jump to the error, of
course.

Default: `t`

## Thanks

* Jeremy Ashkenas for CoffeeScript
* <http://xahlee.org/emacs/elisp_syntax_coloring.html> for instructions.
* Jason Blevins for the guidance his markdown-mode.el gave.
* Steve Yegge for js2

## Bugs

Prototype accessor assignments like `String::length: -> 10` don't look
great.

It's tested on Aquamacs 1.9 (Emacs 22) for OS X Snow Leopard so it may
not work on your environment. Please file a bug at
<http://github.com/defunkt/coffee-mode/issues> and maybe we can fix
the problem.

This is the author's first major mode, so there are probably more
bugs.

[cs]: http://jashkenas.github.com/coffee-script/
[tm]: http://github.com/defunkt/textmate.el
[im]: http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
