(ns schism.core
  (:require [schism.types.set :as sset]
            [schism.types.map :as smap]))

(defn convergent-set
  ([] (sset/new-set)))

(defn convergent-map
  ([] (smap/new-map)))
