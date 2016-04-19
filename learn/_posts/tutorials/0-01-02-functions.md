---
title: Functions
---

## Named Functions

You define functions using the [`defun`][defun] macro:

```lisp
(defun fib (n)
  "Return the nth Fibonacci number."
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

And call them like you call anything else:

```lisp
CL-USER> (fib 30)
832040
```

## Anonymous Functions

## Application

Functions can be called indirectly using [`funcall`][funcall]:

```lisp
CL-USER> (funcall #'fib 30)
832040
```

Or with [`apply`][apply]:

```lisp
CL-USER> (apply #'fib (list 30))
832040
```

## Multiple Return Values

```lisp
(defun many (n)
  (values n (* n 2) (* n 3)))
```

```lisp
CL-USER> (multiple-value-list (many 2))
(2 4 6)

CL-USER> (nth-value 1 (many 2))
4
```

We can also use [`multiple-value-bind`][m-v-b] to assign each return value to a
variable:

```lisp
CL-USER> (multiple-value-bind (first second third)
             (many 2)
           (list first second third))
(2 4 6)
```

[defun]: http://www.lispworks.com/documentation/lw50/CLHS/Body/m_defun.htm
[funcall]: http://www.lispworks.com/documentation/lw70/CLHS/Body/f_funcal.htm
[apply]: http://www.lispworks.com/documentation/lw50/CLHS/Body/f_apply.htm
[m-v-b]: http://www.lispworks.com/documentation/HyperSpec/Body/m_multip.htm
