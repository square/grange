grange
======

A golang port of librange.

Incomplete and not ready to be used for anything.

Goals
-----

* Easily run cross-platform.
* Good error messages for query language.
* Fast. (Looking at you, `clusters`.)

Supported Syntax
----------------

* `a.example.com`
* `a.example.com,b.example.com`
* `{a,b}.example.com`
* `example.{com,org}`
* `%cluster1`
* `%cluster1:KEY`
* `%cluster1,%cluster2`
* `%cluster1 & %cluster2`
* `%cluster1 - %cluster2`
* `has(TYPE;mysql)`
* `has(TYPE;mysql)&has(ENV;prod)`
* `$KEY`
* `@group`
* Expressions as values.
* Extraneous whitespace.

### Unsupported (yet)

* `a1..9.example.com`
* `?example.com`
* `clusters(example.com)`
* `%{expr}`
* `%{expr}:KEY`
* `%cluster:{KEY1,KEY2}`

Development
-----------

This is library, so does not export a main function. Run it via tests.

    go test

Notes
-----

* Parser and lexer are still in flux, I'm just toying with ideas.
