---
title: Recommended Libraries
layout: wiki
---

# Cryptography

- [Ironclad](http://method-combination.net/lisp/ironclad/): A complete library
  with a flexible API.
- [crypto-shortcuts](https://github.com/Shinmera/crypto-shortcuts): A simpler
  interface to Ironclad.

# Databases

## Relational

- [sxql](https://github.com/fukamachi/sxql): DSL for generating SQL.
- [datafly](https://github.com/fukamachi/datafly)

### ORMs

- [Crane](http://eudoxia0.github.io/crane/): Supports Postgres, MySQL and
  SQLite3, provides automatic migrations.
- [Integral](https://github.com/fukamachi/integral): Another ORM.
- [Postmodern](http://marijnhaverbeke.nl/postmodern/): A Postgres-only ORM.

### Drivers

- [cl-dbi](http://8arrow.org/cl-dbi/): Backend-agnostic database interface.

## NoSQL

- [cl-redis](https://github.com/vseloved/cl-redis)
- [cl-mongo](https://github.com/fons/cl-mongo)
- [cl-rethinkdb](https://github.com/orthecreedence/cl-rethinkdb)
- [ClouchDb](http://common-lisp.net/project/clouchdb/)
- [cl-memcached](https://github.com/quasi/cl-memcached)
- [clache](https://github.com/m2ym/clache)

# FFI

- [CFFI](http://common-lisp.net/project/cffi/)
- [cl-autowrap](https://github.com/rpav/cl-autowrap)

# Game Development

- [Xelf](http://xelf.me/reference-plain.html)

# Graphics

- [CLinch](https://github.com/BradWBeer/CLinch)
- [Varjo](https://github.com/cbaggers/varjo): Compiles Lisp to GSLL.
- [cl-cairo2](https://github.com/rpav/cl-cairo2)
- [Vecto](http://www.xach.com/lisp/vecto/): Generates vector graphics.
- [zpng](http://www.xach.com/lisp/zpng/)
- [cl-svg](https://code.google.com/p/cl-svg/): Library for generating SVGs.

# GUI

- Austral
- [CommonQt](http://common-lisp.net/project/commonqt/): Qt binding.
- [cl-cffi-gtk](http://www.crategus.com/books/cl-cffi-gtk/): GTK+ binding.
- [cl-xul](https://github.com/mmontone/cl-xul): Binding to Mozilla's XUL engine.

# Console

# Parsing

- [esrap](https://github.com/nikodemus/esrap): A packrat parser generator.
- [cl-parsec](https://github.com/vseloved/cl-parsec): Parser combinators.

# Audio

# Java

- [CL+J](http://common-lisp.net/project/cl-plus-j/)
- [Foil](http://foil.sourceforge.net/)

# Image Manipulation

# Data Formats

- [CXML](http://common-lisp.net/project/cxml/)
- [plump](https://github.com/Shinmera/plump)
- [lquery](https://github.com/Shinmera/lquery): A jQuery-like library.
- [http-parse](https://github.com/orthecreedence/http-parse): Parsing HTTP
  requests.

## JSON

- [cl-json](http://common-lisp.net/project/cl-json/)
- [jsown](https://github.com/madnificent/jsown): Parse JSON as fast as possible.

(Sabra Crolleton
[compared](https://sites.google.com/site/sabraonthehill/home/json-libraries) the
different libraries here)

## Data Validation

- [ratify](https://github.com/Shinmera/ratify): Validate data (Email, dates,
  URLs).

# Network

- [Drakma](http://weitz.de/drakma/): HTTP client.

# Science

- [GSLL](http://common-lisp.net/project/gsll/): The GNU Scientific Library for
  Lisp.

# Mathematics

- [Napa-FFT3](https://github.com/pkhuong/Napa-FFT3)
- [common-lisp-stat](https://github.com/blindglobe/common-lisp-stat/)
- [lisp-matrix](https://github.com/blindglobe/lisp-matrix)

# Machine Learning

- [NLP](https://github.com/vseloved/cl-nlp)

# Web Development

- [Clack](https://github.com/fukamachi/clack): The equivalent of Ruby's Rack or
  Python's WSGI.

## Web Frameworks

- [Lucerne](): A [Flask](http://flask.pocoo.org/)-inspired, Clack-based
  microframework.
- [radiance](https://github.com/Shinmera/radiance): An extensible CMS.
- [Caveman2](http://8arrow.org/caveman/): A larger framework built on Clack.
- [Ningle](http://8arrow.org/ningle/): Microframework built on Clack.

### Modules

- [clack-errors](https://github.com/eudoxia0/clack-errors): Better error pages
- [hermetic](https://github.com/eudoxia0/hermetic): Authentication.

## Template Engines

- [eco](https://github.com/eudoxia0/eco): Fast, designer-friendly templates.
- [cl-closure-template](https://github.com/archimag/cl-closure-template):
  Google's Closure template engine on Common Lisp.
- [djula](https://github.com/mmontone/djula): Port of the Django template
  system.
- [cl-markup](https://github.com/arielnetworks/cl-markup): Generate HTML through
  an S-expression DSL.

## JavaScript

- [Parenscript](http://common-lisp.net/project/parenscript/): Compile Common
  Lisp to JavaScript.
- [cl-javascript](http://marijnhaverbeke.nl/cl-javascript/): Compile JavaScript
  to Common Lisp.
- [parse-js](http://marijnhaverbeke.nl/parse-js/): Javascript parser.

# Web Service Clients

- [avatar-api](https://github.com/eudoxia0/avatar-api): Gravatar and others
- [chirp](https://github.com/Shinmera/chirp): Twitter
- [humbler](https://github.com/Shinmera/humbler): Tumblr

# Web Servers

- [Hunchentoot](http://weitz.de/hunchentoot/): A mature web server in pure CL.
- [Wookie](http://wookie.beeets.com/): Asynchronous server
- [Portable AllegroServe](https://github.com/franzinc/aserve): Portable version
  of AllegroServe, developed by Franz

# System Interface

- [cl-fad](http://weitz.de/cl-fad/): Portable pathname library.
- [iolib](https://github.com/sionescu/iolib): Input/Output library.

# Parallel, Concurrent and Async

- [lparallel](http://lparallel.org/)
- [Xecto](https://github.com/pkhuong/Xecto): An implementation of data parallelism.
- [cl-async](http://orthecreedence.github.io/cl-async/)
- [chanl](https://github.com/sykopomp/chanl)
- [cl-cuda](https://github.com/takagi/cl-cuda)

# Encodings

- [trivial-utf-8](http://common-lisp.net/project/trivial-utf-8/): A simple way
  to handle UTF-8 conversion.
- [babel](http://common-lisp.net/project/babel/): A heavier library to convert
  across multiple character encodings.

# Localization

- [cl-locale](https://github.com/fukamachi/cl-locale)

# IRC

- [colleen](https://github.com/Shinmera/colleen): Modular IRC bot framework.

# DSLs

- [texp](http://mr.gy/software/texp/): Generates TeX using S-expressions.
- [inferior-shell](http://quickdocs.org/inferior-shell/): DSL for writing shell
  scripts.

# Unit Testing

- [fiveam](http://common-lisp.net/project/fiveam/)
- [cl-test-more](http://8arrow.org/cl-test-more/)

# Logging

- [log4cl](https://github.com/7max/log4cl)

# Language Extensions

## Syntax

- [cl21](http://cl21.org/): A redesign of Common Lisp, implemented in Common
  Lisp.
- [cl-syntax](https://github.com/m2ym/cl-syntax): Building blocks for extending
  the syntax.
- [cl-annot](https://github.com/arielnetworks/cl-annot): Decorator syntax.
- [cl-2dsyntax](http://lisp.hyperprostor.unas.cz/cl-2dsyntax/): Python-like
  syntax implemented in reader macros.

## Type System

- [trivia-types](https://github.com/m2ym/trivial-types)
- [optima](https://github.com/m2ym/optima): Optimized pattern matching.
- [cl-algebraic-data-type](https://bitbucket.org/tarballs_are_good/cl-algebraic-data-type)
- [template](https://bitbucket.org/tarballs_are_good/template): C++-like
  template metaprogramming.
- [interface](https://bitbucket.org/tarballs_are_good/interface): Defining
  interfaces.

# Misc

- [lesque](https://github.com/fukamachi/lesque): Job queue (Port of
[Resque](https://github.com/resque/resque)).

http://www.cliki.net/ContextL
https://github.com/melisgl/named-readtables
http://common-lisp.net/project/iterate/ local-time http://www.cliki.net/cl-ppcre
http://www.cliki.net/cl-interpol https://github.com/froydnj/diff/
http://www.cliki.net/Soundex http://www.cliki.net/fast-io
http://www.cliki.net/flexi-streams CL-LDAP cl-openid

CLX 
Saluto 
usocket
https://github.com/pyb/zen
http://www.cliki.net/Chemboy
http://www.cliki.net/simple-currency
https://github.com/davazp/cl-icalendar
puri
cl-zmq -
cl-xmpp -
 cl-irc
Buclet -
cl-horde3d - 
cl-sdl2
cl-gd -
cl-graphviz
https://github.com/tkych/quicksearch
https://github.com/tkych/cl-spark
https://github.com/tkych/donuts
https://github.com/aerique/okra#readme
s-dot
http://www.cliki.net/cl-geo
http://www.cliki.net/cl-wkb
elasticity
http://common-lisp.net/project/cl-sbml/
 
# Configuration Management

- [envy](https://github.com/fukamachi/envy)
- [universal-config](https://github.com/Shinmera/Universal-Config)

# Build Systems

- [ASDF](http://common-lisp.net/project/asdf/): **The** Common Lisp build
  system.
- [asdf-linguist](https://github.com/eudoxia0/asdf-linguist): Extensions to ASDF
  for compiling other languages, Sass files, etc.
- [XCVB](http://common-lisp.net/project/xcvb/): A build system built at ITA
  Software to handle large parallel builds.

## Package Management

- [Quicklisp](http://www.quicklisp.org/)
- [Quickutil](https://github.com/tarballs-are-good/quickutil): Package manager
  for utilities.
- [qlot](https://github.com/fukamachi/qlot)
- [cl-project](https://github.com/fukamachi/cl-project): Project skeleton
  generator.

# Benchmarking

- [trivial-benchmark](https://github.com/Shinmera/trivial-benchmark)
