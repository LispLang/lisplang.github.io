---
title: Style Guide
---

This page is largely based on Google's [Common Lisp Style Guide][google] and
Ariel Networks' own [guide][an].

# Syntax

## Variables

**Special variables** (Mutable globals) should be surrounded by asterisks. These
  are called earmuffs.

For example:

~~~lisp
(defparameter *positions* (make-array ...))

(defparameter *db* (make-hash-table))
~~~

**Constants** should be surrounded with plus signs. For example:

~~~lisp
(defparameter +golden-ratio+ 1.6180339)

(defparameter +allowed-operators '(+ - * / expt))
~~~

# Packages

## One Package Per File

Unless it makes sense to have one package cover multiple files.

[google]: https://google-styleguide.googlecode.com/svn/trunk/lispguide.xml
[an]: http://labs.ariel-networks.com/cl-style-guide.html
