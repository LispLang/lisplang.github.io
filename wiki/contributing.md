---
title: Contributing
layout: wiki
---

# Version Control

The source for this website is hosted on GitHub [here]({{ site.repo_url}}).

## How to Contribute

# Kramdown Quick Reference

We use [Kramdown](http://kramdown.gettalong.org) to parse the Markdown pages to
HTML. Kramdown has a few extensions to make life easier.

## Code

Inline code uses backticks. For example, `` `(foo bar)` `` will produce `(foo bar)`.

Code blocks are created using three tildes (`~`), optionally followed by the
name of the language (`lisp`, usually).

~~~~
~~~lisp
(defun id (x) x)
~~~
~~~~

## Tables

# Custom Links

Often-used links are abbreviated into variables, kept in the `_config.yml`
file. For example, the link to the {{ site.l.sbcl }} implementation is
`{% raw %}{{ site.l.sbcl }}{% endraw %}`. For a complete list of custom links, see the
`_config.yml` file in the repository.

# Templates

We use (Abuse?) the Jekyll `include` system to create custom MediaWiki like
templates.

### `fig`

The `fig` template inserts figures with descriptions. It takes two parameters:

`uri`
: The path to the image, relative to `/wiki/img`. That is, it's not possible to
link to outside images: If you want an image, it goes in the wiki.

`desc`
: A description of the image, which will appear under the image.

Examples:

{% raw %}
~~~
{% include fig uri="opusmodus.jpg" desc="Screenshot of the composer" %}
~~~
{% endraw %}

Creates:

{% include fig uri="opusmodus.jpg" desc="Screenshot of the composer" %}
