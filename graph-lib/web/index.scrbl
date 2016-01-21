#lang scribble/manual
@(require "../lib/doc.rkt")
@(require "../lib/doc/fork.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Ph.C}

@forkongithub["https://github.com/jsmaniac/phc/"]{Fork me on GitHub!}

@hyperlink["https://travis-ci.org/jsmaniac/phc"]{
 @remote-image["https://img.shields.io/travis/jsmaniac/phc.svg"]{
  Build Status,}}
@hyperlink["https://coveralls.io/github/jsmaniac/phc?branch=master"]{
 @remote-image["https://img.shields.io/coveralls/jsmaniac/phc.svg"]{
  Coverage Status.}}


@;@(table-of-contents)

@section{Introduction}

@section{Documentation}

@itemlist[
 @item{@hyperlink["docs/"]{Documentation}}
 @item{@hyperlink["coverage/"]{Coverage info}}]

@section{Dependency diagram}

A @hyperlink["deps.png"]{PNG version} and a @hyperlink["deps.pdf"]{PDF version}
are available.

@hyperlink["deps.svg"]{
 @image["docs/deps" #:suffixes '(".pdf" ".svg")]{Dependency diagram}}
