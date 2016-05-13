---
title: Variables
---

## Local Variables

Local variables behave like in any other language: they are normal lexically
scoped variables.

Variables are declared with the `let` special operator:

```lisp
(let ((str "Hello, world"!))
  (string-upcase str))

;; => "HELLO, WORLD!"
```

You can define multiple variables:

```lisp
(let ((x 1)
      (y 5))
  (+ x y))

;; => 6
```

To define variables whose initial values depend on previous variables in the
same form, use `let*`:

```lisp
(let* ((x 1)
       (y (+ x 1)))
  y)

;; => 2
```

## Dynamic Variables

Dynamic variables are sort of like global variables, but more useful: they are
dynamically scoped. You define them either with `defvar` or `defparameter`, the
differences being:

1. `defparameter` requires an initial value, `defvar` does not.
2. `defparameter` variables are changed when code is reloaded with a new initial
   value, `defvar` variables are not.

What does dynamic scoping mean? It means:

```lisp
(defparameter *string* "I'm global")

(defun print-variable ()
  (print *string*))

(print-variable) ;; Prints "I'm global"

(let ((*string* "I have dynamic extent")) ;; Binds *string* to a new value
  (print-variable)) ;; Prints "I have dynamic extent"
;; The old value is restored

(print-variable) ;; Prints "I'm global"
```

In other words, when you redefine the value of a dynamic variable using `let`,
the variable is bound to the new value inside the body of the `let`, and the old
value is 'restored' afterwards.
