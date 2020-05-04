(ns schism.impl.core
  (:require [schism.node :as node]
            [clojure.set :as set])
  #?(:clj (:import (java.util Date))))

(def to-millis (memfn ^Date getTime))

(defn to-date
  [millis]
  #?(:clj (Date. millis)
     :cljs (js/Date. millis)))

(defn now
  []
  #?(:clj (Date.)
     :cljs (js/Date.)))

(defn node-and-threshold
  [data]
  (->> data
       :vector-clock
       (reduce-kv (fn [[node time] candidate-node candidate-time]
                    (if (< (to-millis time) (to-millis candidate-time))
                      [candidate-node candidate-time]
                      [node time]))
                   (-> data :vector-clock first))
       (#(update % 1 to-millis))))

(defn retain-elements
  "Accepts two maps of the form

  {:vector-clock <map of nodes to update times>
   :elements <vector of {:data <opaque value>
                         :author-node <node-id>
                         :record-time <Date object>}>}

  Returns a seq of elements to be retained using ORSWOT merge semantics."
  [own-data other-data]
  (let [other-threshold (-> own-data :vector-clock (get node/*current-node*) to-millis)
        [other-node own-threshold] (node-and-threshold other-data)
        own-vclock-for-other (-> own-data :vector-clock (get other-node))
        other-vclock-limiter (if own-vclock-for-other
                               (fn [{:keys [record-time] :as element}]
                                 (>= (to-millis own-vclock-for-other) (to-millis record-time)))
                               (constantly true))
        other-vclock-for-own (-> other-data :vector-clock (get node/*current-node*))
        own-vclock-limiter (if other-vclock-for-own
                             (fn [{:keys [record-time] :as element}]
                               (>= (to-millis other-vclock-for-own) (to-millis record-time)))
                             (constantly true))
        other-additions (remove #(and (> other-threshold (to-millis (:record-time %)))
                                      (other-vclock-limiter %)) (:elements other-data))
        own-additions (remove #(and (> own-threshold (to-millis (:record-time %)))
                                    (own-vclock-limiter %)) (:elements own-data))]
    (concat other-additions own-additions)))

(defn common-elements
  "Accepts maps of the form

   {:vector-clock <map of nodes to update times>
    :elements <vector of {:data <opaque value>
                          :author-node <node-id>
                          :record-time <Date object>}>}

   Returns a vector of the common elements."
  [& datasets]
  (apply set/intersection (map (comp set :elements) datasets)))

(defn distinct-data
  "Accepts maps of the form

   {:vector-clock <map of nodes to update times>
    :elements <vector of {:data <opaque value>
                          :author-node <node-id>
                          :record-time <Date object>}>}

   Returns a vector of the maps with the common elements entries removed."
  [& datasets]
  (let [common-elements (apply common-elements datasets)]
    (->> datasets
         (map #(update % :elements (fn [elements] (remove common-elements elements))))
         (into []))))

(defn merged-clock
  "Accepts a collection of elements, and two or more datasets of the form

   {:vector-clock <map of nodes to update times>
    :elements <vector of {:data <opaque value>
                          :author-node <node-id>
                          :record-time <Date object>}>}

   Returns a vector clock of the relevant nodes, that being the nodes
  referenced as :author-node in elements."
  [elements & datasets]
  (let [relevant-nodes (set (map :author-node elements))]
    (-> (apply merge-with (partial max-key to-millis) (map :vector-clock datasets))
        (select-keys relevant-nodes))))

(def tail-insertion-sort-value
  "The value to use when sorting insertions by index, and the recorded
  index was -1, indicating the element was inserted at the tail."
  #?(:clj Long/MAX_VALUE
     :cljs (.-MAX_SAFE_INTEGER js/Number)))

(defn assoc-n-with-tail-support
  "Assoc with support to place `v` at the tail of `a` when `n` is -1."
  [a n v]
  (if (= n -1)
    (conj a v)
    (assoc a n v)))
