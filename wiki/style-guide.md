---
title: Style Guide
layout: wiki
---

This is an opinionated guide to writing good, maintainable Common Lisp code.

This page is largely based on Google's [Common Lisp Style Guide][google] and
Ariel Networks' own [guide][an].

# Naming

## Variables

**Special variables** (Mutable globals) should be surrounded by asterisks. These
  are called earmuffs.

For example:

~~~lisp
(defparameter *positions* (make-array ...))

(defparameter *db* (make-hash-table))
~~~

**Constants** should be surrounded with plus signs. For example:

~~~lisp
(defparameter +golden-ratio+ 1.6180339)

(defparameter +allowed-operators '(+ - * / expt))
~~~

## Predicates

A predicate is a function that, given some input, returns `t` or `nil`.

Predicates should be suffixed with:

`p`
: If the rest of the function name is a single word, e.g: `abstractp`, `bluep`,
`evenp`.

`-p`
: If the rest of the function name is more than one word, e.g
`largest-planet-p`, `request-throttled-p`.

# Comments

## Comment Hierarchy

Comments that start with four semicolons, `;;;;`, should appear at the top of a
file, explaining its purpose.

You should **not** include copyright or authorship information in file-level
comments. The license should not be mentioned outside the system definition and
the README.

Comments starting with three semicolons, `;;;`, should be used to separate
regions of the code.

Comments with two semicolons, `;;`, should describe regions of code within a
function or some other top-level form, while single-semicolon comments, `;`,
should just be short notes on a single line.

## Do not use comments to erase code

When testing, it's fine to use comments, especially multi-line comments, so
experimentally remove a large segment of code. However, all projects should use
version control, so code should be liberally deleted rather than hidden inside a
comment.

# CLOS

The ideal class definition looks like this:

~~~lisp
(defclass request ()
  ((url :reader request-url
        :initarg :url
        :type string
        :documentation "Request URL.")
   (method :reader request-method
           :initarg :method
           :initform :get
           :type keyword
           :documentation "Request method, e.g :get, :post.")
   (parameters :reader request-parameters
               :initarg :parameters
               :initform nil
               :type association-list
               :documentation "The request parameters, as an association list."))
  (:documentation "A general HTTP request."))
~~~

## Slot options

The following slot options should be used in this order:

`:accessor`, `:reader` or `:writer`
: The name of the accessor method.

`:initarg`
: The keyword argument used to initialize the value.

`:initform` (If any)
: The initial value of the slot, if it's not explicitly given.

`:type` (If possible)
: The type of the slot.

`:documentation`
: The slot's documentation string.

MOP-defined slot options should be added after all other slot options and before
the `:documentation` option.

## Document everything

Common Lisp allows you to add docstrings both to classes and individual slots,
and you should use this.

## Use the `:type` slot option

Types are documentation, and Common Lisp allows you to declare the type of class
slots.

~~~lisp
(defclass person ()
  ((name :accessor person-name
         :initarg :name
         :type string
         :documentation "The person's name.")
   (age :accessor person-age
        :initarg :age
        :initform 0
        :type integer
        :documentation "The person's age."))
  (:documentation "A person."))
~~~

Here, the [trivial-types][tt] library will come in handy.

# Flow Control

In short:

* Use `if` when you have a true branch and a false branch.
* Use `when` or `unless` when you're only interested in one condition branch.
* Use `cond` when you have several conditional branches.

## Use `when`, `unless`

If you have an `if` expression with no else part, you should use `when` instead,
and when you have an expression like `(if (not <condition>) ...)` with no else
part, you should use `unless`.

For example:

~~~lisp
(if (engine-running-p car)
    (drive car))

(if (not (seatbelts-fastened-p car))
    (warn-passengers car))
~~~

Should be:

~~~lisp
(when (engine-running-p car)
  (drive car))

(unless (seatbelts-fastened-p car)
  (warn-passengers car))
~~~

Not the difference in indentation.

## Keep conditions short

Large conditional expressions are harder to read, so should be factored out into
functions.

For example, this:

~~~lisp
(if (and (fuelledp rocket)
         (every #'strapped-in-p
               (crew rocket))
         (sensors-working-p rocket))
    (launch rocket)
    (error "Aborting launch."))
~~~

Should be written as:

~~~lisp
(defun rocket-ready-p (rocket)
  (and (fuelledp rocket)
       (every #'strapped-in-p
              (crew rocket))
       (sensors-working-p rocket)))

(if (rocket-ready-p rocket)
    (launch rocket)
    (error "Aborting launch."))
~~~

# Packages

## One Package Per File

Unless it makes sense to have one package cover multiple files.

# Project Structure

## Directory Structure

The average, small project will look like this:

~~~
cl-sqlite3/
  src/
    cl-sqlite3.lisp
  t/
    cl-sqlite3.lisp
  .gitignore
  README.md
  cl-sqlite3.asd
  cl-sqlite3-test.asd
~~~

As an example of a larger project using
[continuous integration](continuous-integration.html), multiple packages, and
optional contrib systems, here's what a hypothetical web scraping framework
would look like:

~~~
spider/
  src/
    http/              # The code here would be under the package `spider.http`
      request.lisp
      response.lisp
    downloader/        # The code here would be under the package `spider.downloader`
      downloader.lisp
      middleware.lisp
    condition.lisp     # Web scraping-related conditions
    util.lisp          # Utility functions
    settings.lisp      # Settings for the web scraper
  t/
    http.lisp          # Request/response test suite
    downloader.lisp    # Downloader test suite
    final.lisp         # Code to run the test suites, and set up/tear down any test fixtures
  contrib/
    run-js/            # A contrib module to run JavaScript in the scraper
      README.md        # A description of the module
      run-js.lisp      # The source code
  .gitignore
  .travis.yml          # The .travis.yml file to enable continuous integration
  README.md
  spider.asd
  spider-test.asd
  spider-run-js.asd
~~~

## The README

You should use Markdown for the README file, for two reasons:

* Most source code hosting services detect Markdown README files and display
  them appropriately, which makes them easier to read than a plain text file.

* Quickdocs also extracts Markdown README files, making project descriptions
  easier to read.

The average `README.md` file should look like this:

~~~markdown
# [Project title]

[A short, one-line description of the project]

# Overview

[A longer description of the project, optionally with sub-sections like
'Features', 'History', 'Motivation', etc.]

# Usage

[Examples of usage]

# License

Copyright (c) [Year] [Authors]

Licensed under the [Your license here] License.
~~~

If the project is small enough, you should include a 'Documentation' section
after 'Usage' describing its API. Larger projects should have separate
documentation, however.

## Use Continuous Integration services

[Continuous integration](continuous-integration.html) allows you to move testing
of your project to an external service, and allows potential users to see that
its tests are passing without having to download and test it themselves

CI services are especially useful when the project depends on external
dependencies, such as databases, to be installed for testing. An ORM, or
database interface library, or a binding to a library written in C, would
require specific configuration and setup of external things in order to be
tested. CI services allow you to install all the necessary packages and
configure the virtual machine so testing doesn't require the patience of your
users.

[google]: https://google-styleguide.googlecode.com/svn/trunk/lispguide.xml
[an]: http://labs.ariel-networks.com/cl-style-guide.html
[tt]: https://github.com/m2ym/trivial-types
