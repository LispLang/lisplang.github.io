---
title: Applications using Common Lisp
layout: wiki
---

# StumpWM

Full article: [StumpWM](stumpwm.html)

[StumpWM](https://github.com/stumpwm/stumpwm) is a tiling window manager written
entirely in Common Lisp, using the CLX library. It's not a minimalistic window
manager: It's goal is to be a hackable, Emacs-like,
everything-and-the-kitchen-sink (Maximalist, but not in the way GNOME and KDE)
are window manager.

The [`stumpwm-contrib`](https://github.com/stumpwm/stumpwm-contrib) repository
has community-contributed loadable modules.

{% include fig uri="stumpwm.jpg" desc="StumpWM screenshot, using useless gaps." %}

# Opusmodus

[Opusmodus](http://opusmodus.com/) is a music composition application, built
with Clozure Common Lisp for OS X.

{% include fig uri="opusmodus.jpg" desc="Screenshot of the composer" %}

# Quickdocs

[Quickdocs](http://quickdocs.org/) is a web application written using the
Caveman2 web framework that hosts searchable documentation of the Common Lisp
libraries available on [Quicklisp](quicklisp.html).

# cmacro

[cmacro](https://github.com/eudoxia0/cmacro) implements a
[sweet.js](http://sweetjs.org/)-like macro system for C and languages with a
C-like syntax. It is implemented in Common Lisp, and built using SBCL and
{{ site.l.buildapp }}.

# Maxima

[Maxima](http://maxima.sourceforge.net/) is a computer algebra system with a
long history. It's a descendant of [Macsyma][macsyma].

Maxima can also be run on [Android](platforms/android.html) using
[Maxima on Android][maximadroid], which uses ECL.

[macsyma]: http://en.wikipedia.org/wiki/Macsyma
[maximadroid]: https://sites.google.com/site/maximaonandroid/

# DART

[DART][dart], the Dynamic Analysis and Replanning Tool, is a Common Lisp
military logistics AI program. It's use in the Gulf War saved more funds than
DARPA had spent in AI research in the previous 30 years.

[dart]: http://en.wikipedia.org/wiki/Dynamic_Analysis_and_Replanning_Tool

# Cyc

# Mirai

# Parasol

[Parasol][parasol] is a painting application written using
[CommonQt](common-qt.html).

[parasol]: https://github.com/Shinmera/parasol

# pgloader

[pgloader][pgloader] is an application for rapidly loading data from various
formats into PostgreSQL. It's specially well suited to performing
transformations on the data while it's loaded and handling the many kinds of
corrupted or poorly formatted data.

The author originally developed pgloader in Tcl, then ported it to Python, and
finally to Common Lisp.

See the [GitHub repo][pgloadergh].

[pgloader]: http://pgloader.io/
[pgloadergh]: https://github.com/dimitri/pgloader

# pgcharts

[pgcharts][pgcharts] is a Common Lisp web application for generating charts from
queries to PostgreSQL databases.

[pgcharts]: https://github.com/dimitri/pgcharts

# Raytheon

http://www.lispworks.com/success-stories/raytheon-siglab.html

# Softscan

[Fractal Concept][fractal] used LispWorks to develop [Softscan][softscan],
"reliable and flexible software to drive automated non destructive testing
installations.".

>Fractal Concept offers a wide range of software and electronic products, from
>non-destructive testing ultrasonic devices to Internet applications. Founded in
>1999, the company now works for several European bluechip companies. It helps
>clients gain competitive edge by employing only the most powerful and reliable
>software technologies.
>
>LispWorks has been used by Fractal Concept to develop SoftScan, software that
>drives automated, non-destructive testing applications - 'completely written in
>Lisp, hence its reliability and robustness'.

[fractal]: http://www.fractalconcept.com/asp/RaZ7/sdataQ0hycOvgCeWYDM==/sdataQuEY-NQ=
[softscan]: http://www.lispworks.com/success-stories/fractalconcept-softscan.html

# pgEdit

[pgEdit][pgedit] is an SQL editor and development environment built for
PostgreSQL.

{% include fig uri="pgedit.png" desc="pgEdit screenshot" %}

[pgedit]: http://www.lispworks.com/success-stories/pgedit.html

# CAGED and BEST

http://www.lispworks.com/success-stories/hms-caged-best.html

# Routific

[Routific](https://routific.com/) is a route planning and scheduling
application, and its algorithm is written in Common Lisp.
