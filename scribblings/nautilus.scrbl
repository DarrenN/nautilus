#lang scribble/manual
@require[@for-label[nautilus
                    racket/base]]

@title{nautilus}
@author[(author+email "Darren_N" "info@v25media.com")]

Nautilus checks out the @link["https://github.com/papers-we-love/papers-we-love"]{Papers We Love} repo locally and walks the directories, looking for README files and PDFs. It uses those to query @link["https://www.semanticscholar.org/"]{Semantic Scholar} and build up a small @link["https://sqlite.org/"]{SQLite} database of metadata about the contents of the repo. The db file is checked back into the Papers We Love repo.

@section[#:tag "installation"]{Installation}

Nautilus requires @link["httpa://download.racket-lang.org/"]{Racket} to run. On macOS you can install it via @link["https://brew.sh/"]{Homebrew} as well. After Racket is setup, we setup our dependencies and compile Nautilus to a binary:

@codeblock{
$ brew cask install racket
$ cd nautlius
$ raco pkg install
$ raco exe -o nautilus main.rkt
}

@section[#:tag "configuration"]{Configuration}

Nautlius uses @code{.nautilusrc} files for local config:

@codeblock{
{
  "logfile-path": "/tmp/logs",
  "sqlite-path": "/home/sakura/papers-we-love/nautilus.db",
  "pwlrepo-path": "/home/sakura/papers-we-love",
  "pwlrepo-hostname": "github.com",
  "pwlrepo-repository": "papers-we-love/papers-we-love.git"
}
}

This file should be in the top level directory next to @code{main.rkt}. This file mostly tells nautilus where things are locally and where to find the Papers We Love repo.

@section[#:tag "running"]{Running}

Run the program by executing the binary:

@codeblock{
$ ./nautilus
}

Nautilus will look for the @code{.nautilusrc} file. If it can't be found it will fall back on default values. A new log file will be made every 24 hours. The first time nautlius runs it will check out the Papers We Love repo, which can take some time (lots of PDFs). It will then create the db file and start loading it with data. Again, on the first run this will take a while (5 - 10min).

Nautilus will also try to check the db file into the repo, but will fail unless you're a contributor to the project.

@section[#:tag "tables"]{Tables}

The db contains the following tables:

@codeblock{
authors        files          papers         tagspapers
authorspapers  links          tags
}

With the following schemas:

@bold{authors}
@codeblock{
CREATE TABLE authors
  (id integer primary key,
   name varchar unique not null,
   first_name varchar,
   middle_name varchar,
   last_name varchar);
}

@bold{authorspapers}
@codeblock{
CREATE TABLE authorspapers
  (id integer primary key,
   author_id integer not null,
   paper_id integer not null);
}

@bold{files}
@codeblock{
CREATE TABLE files
  (id integer primary key,
   paper_id int,
   sha1 varchar unique not null,
   filename varchar not null,
   directory varchar not null,
   normalized varchar not null,
   created varchar,
   modified varchar);
}

@bold{links}
@codeblock{
CREATE TABLE links
  (id integer primary key,
   paper_id int,
   url varchar unique not null,
   title varchar not null,
   directory varchar not null,
   status int not null,
   created varchar,
   modified varchar);
}

@bold{papers}
@codeblock{
CREATE TABLE papers
  (id integer primary key,
   title varchar unique not null,
   year int,
   abstract varchar,
   venue varchar,
   directory varchar,
   created varchar,
   modified varchar);
}

@bold{tags}
@codeblock{
CREATE TABLE tags
  (id integer primary key,
   tag varchar unique not null);
}

@bold{tagspapers}
@codeblock{
CREATE TABLE tagspapers
  (id integer primary key,
   tag_id integer not null,
   paper_id integer not null);
}

@section[#:tag "queries"]{Fun queries}

You can use SQL queries in SQLite to pull information out of the tables.

Get all papers with link/file information and authors/tags:

@codeblock{
SELECT
    papers.title,
    links.url,
    '/' || files.directory || files.filename,
    group_concat(DISTINCT authors.name),
    group_concat(DISTINCT tags.tag)
FROM papers
JOIN links ON links.paper_id = papers.id
JOIN files ON files.paper_id = papers.id
JOIN authorspapers AS ap ON ap.paper_id = papers.id
JOIN authors ON authors.id = ap.author_id
JOIN tagspapers AS tp ON tp.paper_id = papers.id
JOIN tags ON tags.id = tp.tag_id
GROUP BY papers.title;
}

Tags with counts of papers:

@codeblock{
SELECT
  tags.tag,
  count(papers.id)
FROM papers
JOIN tagspapers AS tp ON tp.paper_id = papers.id
JOIN tags ON tags.id = tp.tag_id
GROUP BY tags.tag;
}

Authors with counts of papers:

@codeblock{
SELECT
  authors.name,
  count(papers.id)
FROM papers
JOIN authorspapers AS ap ON ap.paper_id = papers.id
JOIN authors ON authors.id = ap.author_id
GROUP BY authors.name;
}
