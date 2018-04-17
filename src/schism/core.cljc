(ns schism.core
  (:require [schism.impl.types.set :as sset]
            [schism.impl.types.map :as smap]
            [schism.impl.types.list :as slist]
            [schism.impl.types.vector :as svector]
            [schism.impl.protocols :as sp]
            [schism.node :as sn]))

(defn convergent-set
  [& args]
  (apply sset/new-set args))

(defn convergent-map
  [& args]
  (apply smap/new-map args))

(defn convergent-list
  [& args]
  (apply slist/new-list args))

(defn convergent-vector
  [& args]
  (apply svector/new-vector args))

(defn converge
  [c1 c2]
  (sp/synchronize c1 c2))

(def initialize-node!
  sn/initialize-node!)
