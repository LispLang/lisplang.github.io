---
title: Reader Macros
layout: wiki
---

# Examples

## Decorators

The [`cl-annot`](https://github.com/arielnetworks/cl-annot) library implements
the decorator pattern (Calling them "annotations") in Common Lisp.

## Hash Table Literals

Taken from [here](http://frank.kank.net/essays/hash.html).

~~~lisp
(defun read-separator (str)
 (let
  ((*readtable* (copy-readtable *readtable* nil)))
  (set-macro-character #\, (lambda (stream char)
                            (declare (ignore char) (ignore stream))
                            'break))
  (read str nil)))

(set-macro-character #\{
 (lambda (str char)
  (declare (ignore char))
  (let
   ((*readtable* (copy-readtable *readtable* nil)))
   (set-macro-character #\} (lambda (stream char)
                             (declare (ignore char) (ignore stream))
                             'end))

   (let
    ((pairs (loop for key = (read str nil nil t)
                  for sep = (read str nil nil t)
                  for value = (read str nil nil t)
                  for end? = (read-separator str)
                  do (when (not (eql '=> sep)) (error "Expected =>, did not get"))
                  do (when (not (or (eql 'end end?) (eql 'break end?))) (error "Expected , or }"))
                  collect (list key value)
                  while (not (eql 'end end?))))
     (retn (gensym)))
    `(let
      ((,retn (make-hash-table :test #'equal)))
      ,@(mapcar
         (lambda (pair)
          `(setf (gethash ,(car pair) ,retn) ,(cadr pair)))
         pairs)
      ,retn)))))
~~~

# See Also

- [Reader Macros in Common Lisp](https://gist.github.com/chaitanyagupta/9324402)
- [Common Lisp Reader Macros: A Simple Introduction](http://dorophone.blogspot.com/2008/03/common-lisp-reader-macros-simple.html)
- [*Read-Macros*](http://dunsmor.com/lisp/onlisp/onlisp_21.html) chapter from
  Paul Graham's *On Lisp*.
- [*Read Macros*](http://letoverlambda.com/index.cl/guest/chap4.html) chapter
  from Doug Hoyte's *Let Over Lambda*.
