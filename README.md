Nautilus [![Build Status](https://travis-ci.org/DarrenN/nautilus.svg?branch=master)](https://travis-ci.org/DarrenN/nautilus) [![Coverage Status](https://coveralls.io/repos/github/DarrenN/nautilus/badge.svg?branch=master)](https://coveralls.io/github/DarrenN/nautilus?branch=master)
========

![Golden Nautilus - Alberto J. Almarza](https://c1.staticflickr.com/8/7317/27363231515_421d83ffe5_b.jpg)

Walks the [Papers We Love](http://paperswelove.org) [Repo](https://github.com/papers-we-love/papers-we-love) for links to papers and PDFs then gathers metadata about them into a SQLite3 database file.

<img src="http://paperswelove.org/images/logo-top.svg" />

### To install

```
$ brew cask install racket
$ cd nautlius
$ raco pkg install
$ raco exe -o nautilus main.rkt
```

### Generate documentation

The docs go more in depth into how to setup Nautilus and how the DB is structured. You probably want to read them:

```
$ cd scribblings
$ raco scribble +m nautilus.scrbl
$ open nautilus.html
```

### Config file

1. Copy `nautliusrc.template` to `.nautliusrc`
1. Update the values inside to point to correct directories, etc.

---

_["Golden Nautilus"](https://www.flickr.com/photos/albertotem/27363231515/) Alberto J. Almarza - Pen and ink May 2016_

<a href="http://racket-lang.org/"><img src="http://racket-lang.org/img/racket-logo.svg" width="80" height="80" /></a>
