# schism

A batteries included library of CRDT implementations of Clojure's core
data types: Sets, Maps, Vectors, and Lists with support for distributed
modification and eventual consistency.

## Motivation

Clojure is one of a handful of languages which can be authored and
executed in both a high performance server environment and in a web
browser. This strength affords many benefits, one of which the
language covers is not allowing for concurrent modification of data
with rich synchronization semantics.

## Usage

Make some data, serialize it across nodes, bring it back and
synchronize it. Everything magically auto syncs!

## License

Copyright © 2018 Alex Redington

Distributed under the MIT License
