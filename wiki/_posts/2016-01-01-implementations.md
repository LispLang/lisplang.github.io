---
title: Implementations
---

Notes for beginners:

* Just use SBCL
* Just use SBCL
* **Just use SBCL**

# Current Implementations

These implementations are active maintained, improved, and used in production,
and are all an excellent choice for application development.

## SBCL

_Main Article: [SBCL](/wiki/article/sbcl/)_

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

## ECL

[ECL](http://ecls.sourceforge.net/) (*Embeddable Common Lisp*) is an
implementation that compiles Common Lisp to C, and is useful for bringing Common
Lisp to new platforms and to embedded environments.

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

## Movitz

[Movitz](http://common-lisp.net/project/movitz/) is a Common Lisp implementation
that runs "on the metal" on the x86 architecture. It's intended to be the basis
of a Common Lisp-based operating system or for embedded development.

See [this fork](https://github.com/PuercoPop/Movitz) for building.

## XCL

[XCL](https://github.com/gnooth/xcl) is a GPL-licensed implementation with a
kernel written in C++, and an optimizing compiler written in Common Lisp with
x86 and x86_64. It was developed by the author of ABCL. The latest version can
compile SBCL, and the tests of cl-ppcre, Ironclad and Alexandria.

# New Implementations

The following implementations are either new or under development.

## Clasp

[Clasp][claspgh] is a new Common Lisp implementation that targets LLVM.

See the [Hacker News][clasphn] and [Reddit][claspreddit] discussions on the
announcement.

[claspgh]: https://github.com/drmeister/clasp
[clasphn]: https://news.ycombinator.com/item?id=8367404
[claspreddit]: http://www.reddit.com/r/programming/comments/2hflzk/announcing_clasp_a_common_lisp_implementation/

# Obsolete Implementations

This section documents implementations that are obsolete, historical, or
otherwise not recommended for use.

## MKCL

[ManKai Common Lisp](http://common-lisp.net/project/mkcl/) started as a fork of
ECL.
