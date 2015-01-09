---
title: Reader Macros
layout: wiki
---

# Examples

## Decorators

The [`cl-annot`](https://github.com/arielnetworks/cl-annot) library implements
the decorator pattern (Calling them "annotations") in Common Lisp.

You can use it, for instance, to export symbols outside the package definition
and document them, e.g:

~~~lisp
(defpackage my-package
  (:use :cl :cl-annot.doc))
(in-package :my-package)
(annot:enable-annot-syntax)

@doc "Add two numbers."
@export
(defun add (x y)
  (+ x y))
~~~

Note that you have to put `(annot:enable-annot-syntax)` at the beginning of any
file that uses annotations.

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

## List Comprehensions

From [here](http://lisp-univ-etc.blogspot.com/2013/01/real-list-comprehensions-in-lisp.html).

~~~lisp
(defun read-listcomp (stream char)
  (declare (ignore char))
  (let (rezs srcs conds state)
    (dolist (item (read-delimited-list #\} stream))
      (if (eql '|| item)
          (setf state (if state :cond :src))
          (case state
            (:src (push item srcs))
            (:cond (push item conds))
            (otherwise (push item rezs)))))
    (setf rezs (reverse rezs)
          srcs (reverse srcs)
          conds (reverse conds))
    (let ((binds (mapcar (lambda (group) (cons (first group) (third group)))
                         (group 3 srcs))))
      `(mapcan (lambda ,(mapcar #'car binds)
                 (when (and ,@conds)
                   (list ,(if (rest rezs)
                              (cons 'list rezs)
                              (first rezs)))))
               ,@(mapcar #'cdr binds)))))

(set-macro-character #\{ #'read-listcomp)
(set-macro-character #\} (get-macro-character #\)))
~~~

This uses the `group` utility function defined in Paul Graham's *On Lisp*:

~~~lisp
(defun group (n list)
  "Split LIST into a list of lists of length N."
  (declare (integer n))
  (when (zerop n)
    (error "Group length N shouldn't be zero."))
  (labels ((rec (src acc)
             (let ((rest (nthcdr n src)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when list
      (rec list nil))))
~~~

# See Also

- [Reader Macros in Common Lisp](https://gist.github.com/chaitanyagupta/9324402)
- [Common Lisp Reader Macros: A Simple Introduction](http://dorophone.blogspot.com/2008/03/common-lisp-reader-macros-simple.html)
- [*Read-Macros*](http://dunsmor.com/lisp/onlisp/onlisp_21.html) chapter from
  Paul Graham's *On Lisp*.
- [*Read Macros*](http://letoverlambda.com/index.cl/guest/chap4.html) chapter
  from Doug Hoyte's *Let Over Lambda*.
