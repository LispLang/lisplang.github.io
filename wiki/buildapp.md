---
title: Buildapp
layout: wiki
---

[Buildapp](http://www.xach.com/lisp/buildapp/) is a program that facilitates the
creation of standalone Common Lisp binaries using {{ site.l.sbcl }} or
{{ site.l.ccl }}.

# Example Usage

For an application with a system named `my-app`, and the entry function
`my-app:main`, the following will produece a binary named `myapp`.

~~~
buildapp --output myapp \
         --asdf-path . \
         --asdf-tree ~/quicklisp/dists \
         --load-system my-app \
         --entry my-app:main
~~~

# Users

* [cmacro](https://github.com/eudoxia0/cmacro)
* [pgloader](https://github.com/dimitri/pgloader)
