# schism

A batteries included library of CRDT implementations of Clojure's core
data types: Sets, Maps, Vectors, and Lists with support for distributed
modification and eventual consistency.

## Dependency Information

Latest release: 0.1.2

[Leiningen](http://github.com/technomancy/leiningen/) and [Boot](http://boot-clj.com)
dependency information:

```
[com.holychao/schism "0.1.2"]
```

## Motivation

Clojure is one of a handful of languages which can be authored and
executed in both a high performance server environment and in a web
browser. This strength affords many benefits, one of which the
language does not cover is allowing for concurrent modification of data
with rich synchronization semantics.

There are some other efforts similar to this. Schism's aims are:

- To minimize the locus of concerns outside of collection data
  structures.
- To provide collection data structures with inferior performance to
  Clojure's own persistent data structures, but which only incur
  sub-linear cost increases, available through the same interfaces.
- To provide collection data structures with greater storage, and
  serialization costs than Clojure's own persistent data structures,
  which grow with an upper bound of the number of elements in the
  collection, independent of the number of operations against that
  collection.
- Allow for a Clojure and ClojureScript processes executing on
  separate nodes to maintain convergent data.


## Limitations

Because schism refuses to use tombstones, some convergence operations
will have different results than structures which will embrace the
costs of tombstones.

For example, Schism's set may drop a recently added element during
convergence with certain vector clock states. While this quality is
undesirable, I accept it more readily than monotonically increasing
storage requirements for data explicitly intended for communication
between nodes. Future work may pursue allowing for the convergence
operation to have some configurability so that vector clocks will
retain information more eagerly to reduce the incidence of this phenomena.

## Usage

`schism.core` contains functions for generating new collections,
e.g. `schism.core/convergent-set`. These functions accept arguments
much like their Clojure core equivalents, so `(convergent-set :a :b
:c)` creates a set equivalent to `#{:a :b :c}`

These collections support Clojure's collection operations with the
same semantic consequences (save for above mentioned performance and
storage costs). Any divergence between a schism collection's behavior
and a Clojure collection's behavior is a bug. Please file a report, as
these should be relatively fast and easy to fix.

String coercion of a schism collection is identical to that of a
Clojure collection, e.g. a convergent set will coerce to a string as
`#{:a :b :c}`. However, `pr-str` will generate a longer
tagged literal containing all synchronization data for the structure
to operate correctly. Schism eagerly enables edn readers for its
structures, so synchronization can be as simple as sending the results
of `pr-str` over the wire, reading on the receiving side and
converging with the receiver's local copy.

Convergence is done with `schism.core/converge`. It accepts two
collections, which it assumes are both copies of the same replicated
collection, and returns a single collection: the result of
convergence. Converge replicates the metadata of its first argument
into its return value, if present.

Each CLJ and CLJS process working with any shared schism collection is
a node in a distributed computing cluster. Each node should have an
identity in order for synchronization to operate correctly. You may
invoke `schism.core/initialize-node!` with no arguments to initialize
the current process to have a randomly generated UUID as its node
identifier. You may use any serializable value as the node identifier
by passing that value to initialize-node. If you can create stable
node identifiers, that can lead to some minor reduction in the storage
requirements for schism's data structures. If two nodes operate on a
replicated collection independently and do NOT have distinct node
identifiers, schism's convergence behavior is undefined. (Don't do
this). If you do not explicitly invoke `initialize-node!`, it is left
at the value `nil`.

## Nested collections

It is common to build up a tree of maps and vectors to create several
addressable values that cohere to a common whole. If one built this
collection up out of individual schism collections, a number of
problems with convergence and serialization would present
themselves. I have provided `schism.core/nested-map` and
`schism.core/nested-vector` to address both these problems. While
these provide best-least-surprise convergence, it's important to
understand the limitations and leverages of best:

- These collections incur substantially more CPU time to conduct
  simple operations as a isomorphic mirror of all modifications must
  be computed on each update.
- While adding empty collections should work, please avoid doing so.
- `clojure.core/assoc-in`, `clojure.core/update-in`, and friends
  should work with the convergent map without any special handling.
- Vectors containing leaf nodes will compose tail insertions, so two
  nodes adding to the tail with `conj` will have both of their
  additions retained in chronological order.
- Vectors at intermediate nodes will treat the child at the index as
  identity.
- Collections must be isomorphic to converge: Do not allow one node to
  place a vector and another node to place a map at the same path in
  the tree.

Given the additional complications I strongly encourage clients to
pursue a strategy of retaining a shallow convergent-vector of entity
ids and one convergent-map for each entity. For those cases where this
combo is not sufficient, please wield `nested-map` and `nested-vector`
with care.

## Further work

- Configurable convergence
- Other good ideas as the community provides them

## Contributing

I don't use CA's or other such things. Bugfixes are welcome and
appreciated.

I reserve the right to dismiss feature requests in the guise of PRs.

All work will be evaluated in light of conformance with motivations as
stated in this document.

## License

Copyright © 2020 Alex Redington

Distributed under the MIT License
