---
title: Recommended Libraries
---

# Databases

## Relational

- [sxql](https://github.com/fukamachi/sxql): DSL for generating SQL.
- [datafly](https://github.com/fukamachi/datafly): Lightweight ORM-like layer.

### ORMs

- [Crane](http://eudoxia.me/crane/): Supports Postgres, MySQL and
  SQLite3, provides automatic migrations.
- [Integral](https://github.com/fukamachi/integral): Another ORM.
- [Postmodern](http://marijnhaverbeke.nl/postmodern/): A Postgres-only ORM.

### Drivers

- [cl-dbi](http://8arrow.org/cl-dbi/): Backend-agnostic database interface.

## NoSQL

- [cl-redis](https://github.com/vseloved/cl-redis)
- [cl-mongo](https://github.com/fons/cl-mongo)
- [cl-rethinkdb](https://github.com/orthecreedence/cl-rethinkdb)
- [ClouchDb](http://common-lisp.net/project/clouchdb/): CouchDB interface.
- [cl-memcached](https://github.com/quasi/cl-memcached)
- [clache](https://github.com/m2ym/clache): Cache facility.

# FFI

## C

- [CFFI](http://common-lisp.net/project/cffi/): Portable, easy to use C foreign
  function interface.
- [cl-autowrap](https://github.com/rpav/cl-autowrap): Automatically parser
  header files into CFFI definitions.

## Java

- [CL+J](http://common-lisp.net/project/cl-plus-j/)
- [Foil](http://foil.sourceforge.net/)

## Scripting Languages

- [burgled-batteries](https://github.com/pinterface/burgled-batteries): C-level
  Python bridge.

# Cryptography

- [Ironclad](http://method-combination.net/lisp/ironclad/): A complete library
  with a flexible API.
- [crypto-shortcuts](https://github.com/Shinmera/crypto-shortcuts): A simpler
  interface to Ironclad.

# Graphics

- [CLinch](https://github.com/BradWBeer/CLinch): OpenGL-based Graphics Engine.
- [Varjo](https://github.com/cbaggers/varjo): Compiles Lisp to
  [GLSL](http://www.opengl.org/documentation/glsl/).
- [cl-cairo2](https://github.com/rpav/cl-cairo2): Cairo bindings.
- [Vecto](http://www.xach.com/lisp/vecto/): Generates vector graphics.
- [zpng](http://www.xach.com/lisp/zpng/): Create PNG images.
- [cl-svg](https://code.google.com/p/cl-svg/): Library for generating SVGs.
- [cl-horde3d](https://github.com/anwyn/cl-horde3d/):
  [Horde3D](http://www.horde3d.org/) bindings.
- [cl-sdl2](https://github.com/lispgames/cl-sdl2)
- [cl-gd](http://weitz.de/cl-gd/): Binding to the
  [GD](http://libgd.bitbucket.org/) graphics library.
- [Okra](https://github.com/aerique/okra): [OGRE](http://www.ogre3d.org/)
  bindings.

## GUI

- [CommonQt](http://common-lisp.net/project/commonqt/): Qt bindings.
- [cl-cffi-gtk](http://www.crategus.com/books/cl-cffi-gtk/): GTK+ binding.
- [cl-xul](https://github.com/mmontone/cl-xul): Binding to Mozilla's XUL engine.

# Game Development

- [Xelf](http://xelf.me/reference-plain.html): Extensible game engine.
- [Buclet](https://github.com/aerique/buclet): Bindings to the Bullet library.

# Parsing

- [cl-ppcre](http://weitz.de/cl-ppcre/): Regular expressions.
- [esrap](https://github.com/nikodemus/esrap): A packrat parser generator.
- [cl-parsec](https://github.com/vseloved/cl-parsec): Parser combinators.

# Data Formats

- [CXML](http://common-lisp.net/project/cxml/): XML parser.
- [plump](https://github.com/Shinmera/plump): Lenient XML parser.
- [lquery](https://github.com/Shinmera/lquery): A jQuery-like library.
- [http-parse](https://github.com/orthecreedence/http-parse): Parsing HTTP
  requests.
- [simple-currency](https://github.com/a0-prw/simple-currency): Currency conversion.
- [puri-unicode](https://github.com/archimag/puri-unicode): URI parser.

## JSON

- [cl-json](http://common-lisp.net/project/cl-json/): JSON parser and
  serializer.
- [jsown](https://github.com/madnificent/jsown): Parse JSON as fast as possible.

(Sabra Crolleton
[compared](https://sites.google.com/site/sabraonthehill/home/json-libraries) the
different libraries here)

## Data Validation

- [ratify](https://github.com/Shinmera/ratify): Validate data (Email, dates,
  URLs).

# Network

- [Drakma](http://weitz.de/drakma/): HTTP client.
- [Dexador](http://quickdocs.org/dexador/): HTTp client (that [aims at replacing Drakma](http://www.slideshare.net/fukamachi/dexador-rises)).
- [usocket](http://common-lisp.net/project/usocket/): Portable socket
  abstraction.

## Email

- [Postmaster](https://github.com/eudoxia0/postmaster): Provides a simple
  interface to SMTP and IMAP.

## SSH

- [trivial-ssh](https://github.com/eudoxia0/trivial-ssh): A simple SSH/SCP
  library built on top of libssh2.

## IRC

- [colleen](https://github.com/Shinmera/colleen): Modular IRC bot framework.
- [cl-irc](http://www.common-lisp.net/project/cl-irc/): IRC framework.

# Science

- [GSLL](http://common-lisp.net/project/gsll/): The GNU Scientific Library for
  Lisp.

# Mathematics

- [Napa-FFT3](https://github.com/pkhuong/Napa-FFT3): High-performance FFT.
- [common-lisp-stat](https://github.com/blindglobe/common-lisp-stat/)
- [lisp-matrix](https://github.com/blindglobe/lisp-matrix): Linear algebra.

## Plotting

- [cl-spark](https://github.com/tkych/cl-spark): Sparkline charts.

## Machine Learning

- [NLP](https://github.com/vseloved/cl-nlp): Natural Language Processing
  toolkit.

# Web Development

- [Clack](http://clacklisp.org/): The equivalent of Ruby's Rack or Python's
  WSGI.

## Web Frameworks

- [Lucerne](): A [Flask](http://flask.pocoo.org/)-inspired, Clack-based
  microframework.
- [radiance](https://github.com/Shinmera/radiance): An extensible CMS.
- [Caveman2](http://8arrow.org/caveman/): A larger framework built on Clack.
- [Ningle](http://8arrow.org/ningle/): Microframework built on Clack.

### Modules

- [clack-errors](https://github.com/eudoxia0/clack-errors): Better error pages
- [hermetic](https://github.com/eudoxia0/hermetic): Authentication for
  Clack-based applications.
- [OpenID](http://common-lisp.net/project/cl-openid/darcs/cl-openid/README.html): OpenID interface.
- [Saluto](https://github.com/dmitrys99/saluto): RESTAS authentication.

## Template Engines

- [eco](https://github.com/eudoxia0/eco): Fast, designer-friendly templates.
- [cl-closure-template](https://github.com/archimag/cl-closure-template):
  Google's Closure template engine on Common Lisp.
- [djula](http://mmontone.github.io/djula/): Port of the Django template system.
- [cl-markup](https://github.com/arielnetworks/cl-markup): Generate HTML through
  an S-expression DSL.

## JavaScript

- [Parenscript](http://common-lisp.net/project/parenscript/): Compile Common
  Lisp to JavaScript.
- [cl-javascript](http://marijnhaverbeke.nl/cl-javascript/): Compile JavaScript
  to Common Lisp.
- [parse-js](http://marijnhaverbeke.nl/parse-js/): Javascript parser.

## Web Service Clients

- [avatar-api](https://github.com/eudoxia0/avatar-api): Gravatar and others.
- [chirp](https://github.com/Shinmera/chirp): Twitter.
- [humbler](https://github.com/Shinmera/humbler): Tumblr.

## Web Servers

- [Hunchentoot](http://weitz.de/hunchentoot/): A mature web server in pure CL.
- [Wookie](http://wookie.beeets.com/): Asynchronous server
- [Portable AllegroServe](https://github.com/franzinc/aserve): Portable version
  of AllegroServe, developed by Franz

# System Interface

- [cl-fad](http://weitz.de/cl-fad/): Portable pathname library.
- [iolib](https://github.com/sionescu/iolib): Input/Output library.
- [fast-io](https://github.com/rpav/fast-io): Fast octet streams and vectors.

## Parallel, Concurrent and Async

- [lparallel](http://lparallel.org/): Parallel versions of most CL operations.
- [Xecto](https://github.com/pkhuong/Xecto): An implementation of data
  parallelism.
- [cl-async](http://orthecreedence.github.io/cl-async/): Async operations.
- [chanl](https://github.com/sykopomp/chanl): Channel-based concurrency.
- [cl-cuda](https://github.com/takagi/cl-cuda): Access NVIDIA's CUDA.

## Encodings

- [trivial-utf-8](http://common-lisp.net/project/trivial-utf-8/): A simple way
  to handle UTF-8 conversion.
- [babel](http://common-lisp.net/project/babel/): A heavier library to convert
  across multiple character encodings.

# Localization

- [cl-locale](https://github.com/fukamachi/cl-locale)

# Date and Time

- [local-time](http://common-lisp.net/project/local-time/manual.html)

# Unit Testing

- [fiveam](http://common-lisp.net/project/fiveam/)
- [prove](http://quickdocs.org/prove/)

# Logging

- [log4cl](https://github.com/7max/log4cl)

# Language Extensions

## Syntax

- [cl21](http://cl21.org/): A redesign of Common Lisp, implemented in Common
  Lisp.
- [cl-syntax](https://github.com/m2ym/cl-syntax): Building blocks for extending
  the syntax.
- [cl-annot](https://github.com/arielnetworks/cl-annot): Decorator syntax.
- [cl-2dsyntax](http://www.cliki.net/cl-2dsyntax): Python-like
  syntax implemented in reader macros.
- [named-readtables](https://github.com/melisgl/named-readtables): Separate
  reader macros into packages.
- [cl-interpol](http://www.cliki.net/cl-interpol): String iterpolation.

## Type System

- [trivial-types](https://github.com/m2ym/trivial-types): Simple type
  declarations.
- [optima](https://github.com/m2ym/optima): Optimized pattern matching.
- [cl-algebraic-data-type](https://bitbucket.org/tarballs_are_good/cl-algebraic-data-type): ADT library.
- [template](https://bitbucket.org/tarballs_are_good/template): C++-like
  template metaprogramming.
- [interface](https://bitbucket.org/tarballs_are_good/interface): Defining
  interfaces.

# Build Systems

- [ASDF](http://common-lisp.net/project/asdf/): **The** Common Lisp build
  system.
- [asdf-linguist](https://github.com/eudoxia0/asdf-linguist): Extensions to ASDF
  for compiling other languages, Sass files, etc.
- [XCVB](http://common-lisp.net/project/xcvb/): A build system built at ITA
  Software to handle large parallel builds.

## Package Management

- [Quicklisp](http://www.quicklisp.org/): Package manager.
- [Quickutil](https://github.com/tarballs-are-good/quickutil): Package manager
  for utilities.
- [qlot](https://github.com/fukamachi/qlot): Like Ruby's
  [Bundler](http://bundler.io/).
- [cl-project](https://github.com/fukamachi/cl-project): Project skeleton
  generator.

# DSLs

- [texp](http://mr.gy/software/texp/): Generates TeX using S-expressions.
- [inferior-shell](http://quickdocs.org/inferior-shell/): DSL for writing shell
  scripts.
- [donuts](https://github.com/tkych/donuts): Generates `dot` graphs.

# Miscellaneous

- [iterate](http://common-lisp.net/project/iterate/): A lispier version of
  `loop`.
- [quicksearch](https://github.com/tkych/quicksearch): Search for libraries.
- [lesque](https://github.com/fukamachi/lesque): Job queue (Port of
[Resque](https://github.com/resque/resque)).

## Configuration Management

- [envy](https://github.com/fukamachi/envy): Manage configuration options
  through environment variables.
- [universal-config](http://shinmera.github.io/Universal-Config/): Configuration
  persistence.

## Benchmarking

- [trivial-benchmark](https://github.com/Shinmera/trivial-benchmark):
  Statistical benchmarking.
