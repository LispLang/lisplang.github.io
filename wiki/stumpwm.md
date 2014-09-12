---
title: StumpWM
layout: wiki
---

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
