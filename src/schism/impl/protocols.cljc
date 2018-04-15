(ns schism.impl.protocols)

(defprotocol Convergent
  (synchronize [convergent other]
    "Synchronizes `convergent` with `other` such that all changes
    incorporated into `other` will be represented in a new persistent
    structure derived from `convergent`."))

(defprotocol Vclocked
  "A protocol for obtaining the current vector clock of a value, and
  for deriving a new vector clock for a value."
  (get-clock [clocked] "Returns the current vector clock of `clocked`,
  a map of node IDs to timestamps.")
  (with-clock [clocked clock] "Returns a new structure derived from
  `clocked`, associating `clock` with the returned value."))
