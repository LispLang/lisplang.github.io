---
title: StumpWM
layout: wiki
---

StumpWM is a tiling window manager written in pure Common Lisp using the CLX
library.

# Overview

From the [website](http://www.nongnu.org/stumpwm/):

>Stumpwm is a tiling, keyboard driven X11 Window Manager written entirely in
>Common Lisp.
>
>If you're tired of flipping through themes like channel-surfing, and going from
>one perfect-except-for-just-one-thing window manager to another
>even-more-broken-in-some-other-way then perhaps Stumpwm can help.
>
>Stumpwm attempts to be customizable yet visually minimal. There are no window
>decorations, no icons, and no buttons. It does have various hooks to attach
>your personal customizations, and variables to tweak.

# Building

Assuming {{ site.l.sbcl }}:

~~~bash
$ git clone https://github.com/stumpwm/stumpwm.git
$ cd stumpwm
$ sbcl --eval "(ql:quickload '(:clx :cl-ppcre))" --quit
$ autoconf
$ ./configure
$ make
$ sudo make install
~~~

# Multihead Setup

Most tiling window managers allow you to have multiple workspaces, each with its
own set of windows, but differ in how these are handled in multi-screen
environments. In Xmonad, for example, each screen shows a different
workspace. So, for example, your laptop screen shows workspace one and your
external monitor shows workspace two. If you mouse over to workspace one and use
the key combination to switch to workspace two, those get flipped: The external
monitor shows the first workspace and the laptop screen shows the second one.

Stump's multi-head defaults are saner: Every monitor just shows a different
frame of the same workspace (In Stump's vocabulary, the same group). When you
switch groups, every screen shows a different frame of the group you switched
to.

# Links

- [GitHub repo](https://github.com/stumpwm/stumpwm)
- [Contrib repo](https://github.com/stumpwm/stumpwm-contrib)
- [Manual](http://stumpwm.github.io/)
- [Screenshots](http://www.nongnu.org/stumpwm/screenshot.html)
