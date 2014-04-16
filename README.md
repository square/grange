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
* `%cluster1:SOMEKEY`
* `%cluster1:KEYS`
* `%cluster1,%cluster2`
* `%cluster1 & %cluster2`
* `%cluster1 - %cluster2`
* `has(TYPE;mysql)`
* `has(TYPE;mysql)&has(ENV;prod)`
* `$KEY`
* `@group`
* `%{expr}`
* `%{expr}:KEY`
* Expressions as values.
* Extraneous whitespace.

### Unsupported (yet)

* `a1..9.example.com`
* `?example.com`
* `clusters(example.com)`
* `%cluster:{KEY1,KEY2}`

Development
-----------

This is library, so does not export a main function. Run it via tests.

    go test

Notes
-----

* Parser and lexer are still in flux, I'm just toying with ideas.
