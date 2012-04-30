CoffeeScript Major Mode (Python-Mode Mixin Edition)
==========================================================

## OverView
This is CoffeeScript Major Mode marging [defunct's coffee-mode][cm] and 
[python-mode][pm]'s heardocs highlighting and smart delete.

basic information:<https://github.com/defunkt/coffee-mode>

## Improvements
### Indentation

#### Configuring
We can set our　`tab-width` to two using `set-coffee-tab-width` in the `coffee-mode-hook`:

    (defun coffee-custom ()
      "coffee-mode-hook"
     (set-coffee-tab-width 2))

    (add-hook 'coffee-mode-hook
      '(lambda() (coffee-custom)))

If we use `set-coffee-tab-width`, we maybe avoid incorrect indentation bugs.

### Indenters

`else`, `switch`, `when`, `try`, `catch`, `finally` also cause the next line
to be indented a level deeper automatically.

And, this applies to lines ending in `(` additionally.

### Syntax Highlighting

#### Hear Documents
![Screenshot](https://github.com/downloads/torimaru/coffee-mode/heardoc.png)


Powered by python-mode's highlight heardocs solution, it highlight heardocs correctly.


#### Patterns
![Screenshot](https://github.com/downloads/torimaru/coffee-mode/highlight.png)


Highlight lambda function, built-in function(escape, parseInt, Date...),
Object's property(constructor, hasOwnProperty, toString...), and number literal.

### Edit
#### Delete and Backspace
When you press `backspace` once:

    line1()
	    line2()
		^

delete spaces corresponding to one tab:

    line1()
	line2()
	^

`delete` is the same.
This feature is introduced from python-mode.

#### Un-Indent
When you press `Shift-TAB`:

    line1()
	    line2()
		      ^

we un-indent the line:

    line1()
	line2()
	      ^

#### Region Indent
When you press `C-c <`, we un-indent region.
And press `C-c >`, we indent region.

## Thanks

* Jeremy Ashkenas for CoffeeScript
* Chris Wanstrath for coffee-mode
* Barry A. Warsaw, Tim Peters and <https://launchpad.net/python-mode> for python-mode


[cs]: http://jashkenas.github.com/coffee-script/
[cm]: https://github.com/defunkt/coffee-mode
[pm]: https://launchpad.net/python-mode
