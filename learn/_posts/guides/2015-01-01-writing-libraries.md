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
  :description "A framework for representing and manipulating documents as CLOS objects."
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
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op common-doc-test))))
~~~

Breaking it down, the first line defines the name of the system, `common-doc`.

Then, we have the metadata:

~~~lisp
:author "Fernando Borretti <eudoxiahp@gmail.com>"
:maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
:license "MIT"
:version "0.2"
:homepage "https://github.com/CommonDoc/common-doc"
:bug-tracker "https://github.com/CommonDoc/common-doc/issues"
:source-control (:git "git@github.com:CommonDoc/common-doc.git")
:description "A framework for representing and manipulating documents as CLOS objects."
~~~

ASDF allows us to supply author contact information (name and email address),
license information, the version string, some useful links, and a one-line
description.

Then, we have the list of dependencies:

~~~lisp
:depends-on (:trivial-types
             :local-time
             :quri
             :anaphora
             :alexandria
             :closer-mop)
~~~

This is simply a list of systems this system depends on. You can specify
versions here, but typically version management is done externally -- see below.

This is followed by the components -- basically a description of the source
tree. Modules are directories, and files are Lisp files.

~~~lisp
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
~~~

The `:serial t` option basically says "the files in this directory should be
loaded in the order they appear here. ASDF also allows us to be more
sophisticated, and manually specify which files depend on which, and just let
ASDF figure out a total order in which to load them. But for most cases, just
using `:serial t` is good enough.

Then, we use the `:long-description` option and read-time execution to embed the
`README.md` file into the system definition:

~~~lisp
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
~~~

This option is used by tools like [Quickdocs][qd] to display information about a
system.

Finally, we tell ASDF about the associated test system -- this is the system
that loads a testing framework and runs the separate tests. We'll cover that in
the guide about unit testing.

~~~lisp
  :in-order-to ((test-op (test-op common-doc-test)))
~~~

# Configuring ASDF

Before you can load a system, you have to tell ASDF where to find them. This is
specified in the `~/.config/common-lisp/source-registry.conf` file.

For instance, the following configuration:

~~~lisp
(:source-registry
  (:tree (:home "code"))
  :inherit-configuration)
~~~

Tells ASDF to find your system be searching recursively through the `~/code/`
directory, and to inherit the configuration from Quicklisp so you can load
Quicklisp's systems.

# Loading

After you have configured ASDF and created your system, you can load it using
either Quicklisp or ASDF:

~~~lisp
cl-user> (asdf:load-system :my-system)
cl-user> (ql:quickload :my-system)
~~~
