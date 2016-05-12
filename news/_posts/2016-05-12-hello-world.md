---
title: Hello, World!
author: Fernando Borretti
tags: [administrativia]
---

I'm quite happpy

# Previously

If someone wanted to learn Common Lisp, the language, their best bet was Peter
Seibel's [_Practical Common Lisp_][pcl]. But the book, written in 2005, doesn't
cover setting up a modern Common Lisp environment: [Quicklisp][ql] is a more
recent invention.

Learning how to write [ASDF][asdf] systems (think `package.json`), how a library
should be structured, where to find documentation (the answer is
[Quickdocs][qd]), all of that information is spread across dozens of tutorials
in different websites.

Languages created by a single person or a small group, around which a community
accretes, tend to do better here: [Python][python], [Ruby][ruby] and
[Scala][scala] all have websites that provide all of these resources. Languages
created by commitees, like JavaScript or Common Lisp or C, rarely have official
websites.

# Goals

The goal of `lisp-lang.org` is to **lower the barrier to entry to Common Lisp**:
provide a central location to both advertise Common Lisp, and provide all the
information prospective users need to become productive with it, without having
to collect it from different sites and blogs, and without falling into choice
paralysis. 'Productive' means going beyond teaching users about lists and
macros, and including information on how to write libraries, unit-test them, use
CI and code coverage, and publish them to Quicklisp.

Currently we have [tutorials][tut], a [style guide][style], a showcase of
[success stories][story], and a collection of [Lisp books][book].

In the future, there'll be a wiki, and a better-looking version of the
[CLHS][clhs] (generated from the [TeX sources][tex]). Maybe a forum (all written
in Lisp, of course) if the community demands it, but currently people seem to be
fine with Reddit and IRC.

# The Logo

Languages have to have logos. No just to associate them with easily remembered
images, but because the logo is often use to identify the language in lieu of
words.

For instance, [Stripe][stripe] uses logos to identify languages when
advertising integrations:

![Stripe integration](/assets/img/news/stripe-integration.png)

Because Common Lisp was created by a [group of organizations][credits], it never
got an 'official' logo. As a result, there are basically three logos in use in
the wild: endless variants of the letter lambda, the
[Lisp salamander][salamander], and the [Lisp alien][alien].

But logos have to be 1) beautiful and 2) simple, so they are easily
remembered. The salamander isn't the former and the alien isn't the latter.

But becacuse I'm not a graphic designer and good designers are expensive, I just
slapped together a lambda and a rounded square, and called it a day:

<img src="/assets/img/logo/old/blue.png" width="300" alt="old logo" title="old logo">

Then a friend, who is a self-taught graphic designer, saw it and decided he
couldn't let it stand. So he made this slightly different but curvier and thus
more artistic logo:

<img src="/assets/img/logo/blue.png" width="300" alt="new logo" title="new logo">

# The Competition

The two websites that come close to this project are [common-lisp.net][cl.net]
and [Cliki][cliki]. The former is primarily a [GitLab][gl]-based hosting service
for Common Lisp projects, but the landing page contains a brief description of
the language, plus links to tutorials and the like.

Cliki is a wiki, but it looks horrible (which itself is a sufficient reason to
replace it), and despite the number of articles, doesn't contain much useful
information. For instance, the article on [CLOS][clos] is mostly (dead) links to
other websites, and doesn't have code examples. So it will have to go.

And the [CLHS][clhs] needs to be replaced as well, simply because it shows its
terrible age, and it's under Lispworks' copyright.

[pcl]: /books/#practical-common-lisp
[ql]: https://www.quicklisp.org/beta/
[python]: https://www.python.org/
[ruby]: https://www.ruby-lang.org/en/
[scala]: http://www.scala-lang.org/
[asdf]: https://common-lisp.net/project/asdf/
[qd]: http://quickdocs.org/
[tut]: /learn/
[style]: /style-guide/
[story]: /success/
[book]: /books/
[clhs]: http://www.lispworks.com/documentation/HyperSpec/Front/
[tex]: https://github.com/LispLang/ansi-spec
[stripe]: https://stripe.com/us/features
[credits]: http://www.lispworks.com/documentation/lw50/CLHS/Body/00_.htm
[salamander]: https://web.archive.org/web/20051126033302/http://www.normal-null.de/lisp_logo.html
[alien]: http://lisperati.com/logo.html
[cl.net]: https://common-lisp.net/
[cliki]: http://cliki.net/
[gl]: https://about.gitlab.com/
[clos]: http://cliki.net/CLOS
