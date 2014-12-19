---
title: Common Lisp Object System
layout: wiki
---

The Common Lisp Object System is Common Lisp's object-oriented programming
system. It was originally implemented as macros over Common Lisp, but merged
into the languge as it was standarized.

# Benefits

According to Peter Norvig, with CLOS "16 of 23 patterns have qualitatively
simpler implementation in Lisp or Dylan than in C++"[^norvig]. The
circle-ellipse problem, for example, is solved immediately because objects can
change class.

# Examples

## Simple Example

~~~lisp
(defclass <ship> ()
  ((name :reader name   ;; Only reader is implemented, so you can't rename a
                        ;; ship
         :initarg :name
         :type string)
   (kind :accessor kind
         :initarg :kind)
   (tonnage :accessor tonnage
            :initarg :tonnage
            :type float)))

(defparameter *my-ship*
              (make-instance '<ship> :name "USS Walter Mondale"
                                     :kind :laundry-ship
                                     :tonnage 345))

(class-of *my-ship*) ;; => #<standard-class <ship>>
~~~

## Generic Functions

Adapted from the Corvus [source code](https://github.com/eudoxia0/corvus/blob/32f17fb0f4a6c8c913e13317168be8b4b1acb86a/compiler/bootstrap/types.lisp):

~~~lisp
(defclass <i8> () ())
(defclass <i16> () ())
(defclass <i32> () ())
(defclass <i64> () ())
(defclass <i128> () ())

(defgeneric size (type)
  (:documentation "Size in bytes of an integer type.")
  (:method ((int <i8>)) 8)
  (:method ((int <i16>)) 16)
  (:method ((int <i32>)) 32)
  (:method ((int <i64>)) 64)
  (:method ((int <i128>)) 128))
~~~

# Metaobject Protocol

The Metaobject Protocol (MOP) provides mechanisms for extending CLOS by
customizing its behaviour on certain classes. Basically, the MOP sees a class as
being an instance of a metaclass (Think of it as a kind of 'higher-order' object
system, or a 'two tier' object system).

# References

[^norvig]: [*Design Patterns in Dynamic Programming*][norvig].

[norvig]: http://norvig.com/design-patterns/design-patterns.pdf
