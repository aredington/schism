(ns schism.core
  (:require [schism.types.set :as sset]
            [schism.types.map :as smap]
            [schism.types.list :as slist]
            [schism.protocols :as sp]
            [schism.node :as sn]))

(defn convergent-set
  ([] (sset/new-set)))

(defn convergent-map
  ([] (smap/new-map)))

(defn convergent-list
  ([] (slist/new-list)))

(defn converge
  [c1 c2]
  (sp/synchronize c1 c2))

(def initialize-node!
  sn/initialize-node!)
