---
title: Macros
---

As an example, Common Lisp has no `while` loop, rather, there's a `loop` macro
directive for iterating while a condition is true. For brevity, we can define:

```lisp
(defmacro while (condition &body body)
  `(loop while ,condition do (progn ,@body)))
```

And use it like this:

```lisp
(while (some-condition)
  (do-something)
  (do-something-else))
```

This expands to:

```lisp
(loop while (some-condition) do
  (progn
    (do-something)
    (do-something-else)))
```
