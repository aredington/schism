(ns schism.core
  (:require [schism.impl.types.set :as sset]
            [schism.impl.types.map :as smap]
            [schism.impl.types.list :as slist]
            [schism.impl.types.vector :as svector]
            [schism.impl.protocols :as sp]
            [schism.node :as sn]))

(defn convergent-set
  "Create a new ORSWOT containing args. Each arg will be recorded as
  being added to the set by the current node at invocation time."
  [& args]
  (apply sset/new-set args))

(defn convergent-map
  "Create a new ORMWOT, establishing associations between each pair of
  key-value arguments. Each entry will be recorded as being added to
  the map by the current node at invocation time."
  [& args]
  (apply smap/new-map args))

(defn convergent-list
  "Create a new convergent list containing args. Args will be placed
  into the list in the order they appear during invocation, just as
  with `clojure.core/list`. Each entry will be recorded as being added
  to the list by the current node at invocation time."
  [& args]
  (apply slist/new-list args))

(defn convergent-vector
  "Create a new convergent vector containing args. Each entry will be
  recorded as being added to the list by the current node at
  invocation time, in the ordinal position it occupied during
  invocation."
  [& args]
  (apply svector/new-vector args))

(defn converge
  "Return a converged copy of `c1` containing the modifications of
  `c2`. Convergence is defined on a per-type basis. If `c1` has
  metadata, retain that metadata on the returned result. Convergence
  ticks the vector clock for the node on which convergence is
  occurring. `c1` and `c2` must be collections of the same type.

  The behavior of `converge` is not defined when either:

  - The current value of `schism.node/*current-node*` is nil

  - The current value of `schism.node/*current-node*` is shared with
  another node making modifications to the same logical collection."
  [c1 c2]
  (sp/synchronize c1 c2))

(def initialize-node!
  "Initialize the current node to an edn serializable value if
  provided. If invoked with no argument, initializes the current node
  to a random UUID."
  sn/initialize-node!)

(defmacro with-node
  "Run `body` with the current node set to `node-id`"
  [id & body]
  `(sn/with-node ~id ~@body))
