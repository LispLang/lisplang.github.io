---
title: Getting Started
layout: learn
permalink: /learn/getting-started/
---

This article describes what you need to do to get up and running with Common
Lisp.

For the fastest solution, see [Portacle](https://shinmera.github.io/portacle/). It is a portable and multiplatform development environment, with no installation needed.

## Linux & OS X

Linux and OS X differ only in how we install things, so we've grouped both in
the same section.

### Install SBCL

On both Linux and OS X, we'll use [SBCL][sbcl] as the Common Lisp
implementation.

#### Ubuntu/Debian

To install SBCL on either, just run:

```bash
$ sudo apt-get install sbcl
```

#### Arch Linux

Since SBCL is available from the official repositories, you can install it with:

```bash
$ sudo pacman -S sbcl
```

#### OS X

To install SBCL on OS X, just do:

```bash
$ brew install sbcl
```

### Install Quicklisp

Next, we set up [Quicklisp][ql], the package manager. This is similarly easy:

```bash
$ curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
$ sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

This will install Quicklisp to the `~/.quicklisp/` directory.

### Installing Emacs and SLIME

[SLIME][slime] is a Common Lisp IDE built on Emacs. You can install it with
Quicklisp using:

```
$ sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

Then, add this to your `~/.emacs.d/init.el`:

```elisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
```

### Running SLIME

Now that you've installed SLIME, you can run it by running Emacs and typing `M-x
slime`. That is: the `Alt` key along with the `x` key, then type `slime` in the
little buffer at the bottom. Press enter, and a REPL will start.

## Windows

On Windows, you can use [Lispstick][ls].

[sbcl]: http://sbcl.sourceforge.net/
[ql]: https://www.quicklisp.org/beta/
[slime]: https://common-lisp.net/project/slime/
[ls]: http://www.iqool.de/lispstick.html
