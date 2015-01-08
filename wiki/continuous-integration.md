---
title: Continuous Integration
layout: wiki
---

# Travis CI

[Travis CI][travis] is a free continuous integration service that runs from
GitHub repositories.

To use Travis, simply add a `.travis.yml` file to your repository, sign up with
your GitHub account, and enable the repo in your settings.

## The `.travis.yml` file

~~~yaml
language: common-lisp

env:
  matrix:
    - LISP=sbcl

install:
  # Install cl-travis
  - curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | bash

# If the tests use FiveAM
script:
  - cl -e '(ql:quickload :fiveam)'
       -e '(setf fiveam:*debug-on-error* t)'
       -e '(setf *debugger-hook*
                 (lambda (c h)
                   (declare (ignore c h))
                   (uiop:quit -1)))'
       -e '(ql:quickload :crane-test)'

# If the tests use prove
script:
  - cl -l prove
       -e '(or (prove:run :woo-test)
               (uiop:quit -1))'
~~~

## Example Projects

These are the `.travis.yml` files used by various Common Lisp projects.

### [Crane][crane]

~~~yaml
language: common-lisp

addons:
  postgresql: "9.1"

env:
  matrix:
    - LISP=sbcl

install:
  # Install cl-travis
  - curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | bash
  # Update repo
  - sudo apt-get update
  # Install SQLite
  - sudo apt-get install -y sqlite3
  # Set up Postgres
  - sudo -u postgres createdb crane_test_db
  - sudo -u postgres psql -c "CREATE USER crane_test_user WITH PASSWORD 'crane_test_user'"
  - sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE crane_test_db TO crane_test_user"

script:
  - cl -e '(ql:quickload :fiveam)'
       -e '(setf fiveam:*debug-on-error* t)'
       -e '(setf *debugger-hook*
                 (lambda (c h)
                   (declare (ignore c h))
                   (uiop:quit -1)))'
       -e '(ql:quickload :crane-test)'
~~~

### [Woo][woo]

~~~yaml
language: common-lisp

env:
  matrix:
    - LISP=sbcl

before_install:
  - sudo apt-get install libev-dev

install:
  # Install cl-travis
  - curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | bash

before_script:
  - git clone https://github.com/fukamachi/lev ~/lisp/lev
  - git clone https://github.com/fukamachi/fast-http ~/lisp/fast-http

script:
  - cl -l prove
       -e '(or (prove:run :woo-test)
               (uiop:quit -1))'
~~~

[travis]: https://travis-ci.org/
[crane]: http://eudoxia.me/crane
[woo]: https://github.com/fukamachi/woo
