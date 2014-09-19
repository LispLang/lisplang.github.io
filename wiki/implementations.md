---
title: Common Lisp Implementations
---

# Current Implementations

These implementations are active maintained, improved, and used in production,
and are all an excellent choice for application development.

## SBCL

[SBCL](http://sbcl.org/) (*Steel Bank Common Lisp*) is a high-performance native
code compiler written in Common Lisp and C. The code is descended from CMUCL. It
features native threading support, a type inference engine (Useful with gradual
typing) and an optimizing compiler.

Steel Bank Common Lisp is named, in honour of CMUCL, after Andrew Carnegie and
Andrew Mellon, who made their fortunes in steel and banking, respectively.

## CCL

[CCL](http://ccl.clozure.com/) (*Clozure Common Lisp*) is a native code Common
Lisp implementation. It supports many platforms, all of which are at relative
feature parity.

## CLISP

[GNU CLISP](http://www.clisp.org/) is an implementation that uses a bytecode
compiler rather than a native code compiler. By not compiling to machine code,
CLISP is easily portable and runs across a wide array of systems.

## ABCL

[ABCL](http://abcl.org/) (*Armed Bear Common Lisp*) is an implementation that
targets the JVM.

# Domain-Specific Implementations

These implimentations target a specific niche and should not be considered
general purpose.

## XCL

[XCL](https://github.com/gnooth/xcl) is a GPL-licensed implementation with a
kernel written in C++, and an optimizing compiler written in Common Lisp with
x86 and x86_64. It was developed by the author of ABCL. The latest version can
compile SBCL, and the tests of cl-ppcre, Ironclad and Alexandria.

## Movitz

# New Implementations

The following implementations are either new or under development.

## SICL

## Clasp

# Obsolete Implementations

This section documents implementations that are obsolete, historical, or
otherwise not recommended for use.

## MKCL
