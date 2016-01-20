#lang scribble/manual
@(require "lib/doc.rkt")
@doc-lib-setup

@;(require scribble/core
@;          scribble/html-properties)

@title[#:style manual-doc-style]{Ph.C}

@hyperlink["https://travis-ci.org/jsmaniac/phc"]{
 @remote-image["https://travis-ci.org/jsmaniac/phc.png?branch=master"]{
  Build Status}}
@hyperlink["https://coveralls.io/github/jsmaniac/cover?branch=master"]{
 @remote-image[(string-append "https://coveralls.io/repos/jsmaniac/cover/"
                              "badge.svg?branch=master")]{
  Coverage Status}}

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
