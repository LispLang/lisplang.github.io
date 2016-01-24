---
title: Writing Libraries
---

# Ecosystem Overview

In Common Lisp, the build system and the package manager are two separate
things.

ASDF
: ASDF is the build system. It lets you define projects -- called *systems* --
along with their metadata, dependencies, their source code files, and the order
in which those files are loaded.

Quicklisp
: Quicklisp is the package manager. It uses ASDF to extract package's
dependencies and downloads them from a central repository. Unlike most package
managers, Quicklisp isn't a command-line application: you run it from the REPL
like every other Lisp tool, so you can get up and running very easily.

# Defining Systems

A typical system definition looks like this (from
[this](https://github.com/CommonDoc/common-doc) project):

~~~lisp
(defsystem common-doc
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.2"
  :homepage "https://github.com/CommonDoc/common-doc"
  :bug-tracker "https://github.com/CommonDoc/common-doc/issues"
  :source-control (:git "git@github.com:CommonDoc/common-doc.git")
  :depends-on (:trivial-types
               :local-time
               :quri
               :anaphora
               :alexandria
               :closer-mop)
  :components ((:module "src"
                :serial t
                :components
                ((:file "packages")
                 (:file "define")
                 (:file "error")
                 (:file "file")
                 (:file "classes")
                 (:file "metadata")
                 (:file "constructors")
                 (:file "macros")
                 (:file "format")
                 (:file "util")
                 (:module "operations"
                  :serial t
                  :components
                  ((:file "traverse")
                   (:file "figures")
                   (:file "tables")
                   (:file "links")
                   (:file "text")
                   (:file "unique-ref")
                   (:file "toc")
                   (:file "equality")))
                 (:file "print"))))
  :description "A framework for representing and manipulating documents as CLOS
  objects."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op common-doc-test))))
~~~

Breaking it down,
