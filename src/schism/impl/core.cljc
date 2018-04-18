(ns schism.impl.core
  (:require [schism.node :as node]
            [clojure.set :as set])
  #?(:clj (:import (java.util Date))))

(def timefn (memfn ^Date getTime))

(defn node-and-threshold
  [data]
  (->> data
       :vector-clock
       (reduce-kv (fn [[node time] candidate-node candidate-time]
                    (if (< (timefn time) (timefn candidate-time))
                      [candidate-node candidate-time]
                      [node time]))
                   (-> data :vector-clock first))
       (#(update % 1 timefn))))

(defn retain-elements
  "Accepts two maps of the form

  {:vector-clock <map of nodes to update times>
   :elements <vector of {:data <opaque value>
                         :author-node <node-id>
                         :record-time <Date object>}>}

  Returns a seq of elements to be retained using ORSWOT merge semantics."
  [own-data other-data]
  (let [other-threshold (-> own-data :vector-clock (get node/*current-node*) timefn)
        [other-node own-threshold] (node-and-threshold other-data)
        other-additions (remove #(> other-threshold (timefn (:record-time %))) (:elements other-data))
        own-additions (remove #(> own-threshold (timefn (:record-time %))) (:elements own-data))]
    (concat other-additions own-additions)))

(defn common-elements
  "Accepts two maps of the form

   {:vector-clock <map of nodes to update times>
    :elements <vector of {:data <opaque value>
                          :author-node <node-id>
                          :record-time <Date object>}>}

   Returns a vector of the common elements."
  [a-data b-data]
  (set/intersection (set (:elements a-data)) (set (:elements b-data))))

(defn distinct-data
  "Accepts two maps of the form

   {:vector-clock <map of nodes to update times>
    :elements <vector of {:data <opaque value>
                          :author-node <node-id>
                          :record-time <Date object>}>}

   Returns a vector of the two maps with the common elements entries removed."
  [a-data b-data]
  (let [common-elements (set/intersection (set (:elements a-data)) (set (:elements b-data)))]
    [(update a-data :elements #(remove common-elements %))
     (update b-data :elements #(remove common-elements %))]))
