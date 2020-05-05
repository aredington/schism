(ns schism.impl.types.nesting-util
  "Utility functions for supporting nested collections."
  (:require [clojure.data :refer [diff]]
            [schism.impl.core :as ic]))


(defn compare-paths
  [[[a-type a-val :as first-a] & rest-a :as a] [[b-type b-val :as first-b] & rest-b :as b]]
  (cond
    (and (nil? a) (nil? b)) 0
    (nil? a) -1
    (nil? b) 1
    (= first-a first-b) (recur rest-a rest-b)
    (and (= a-type 's) (= b-type 'a)) -1
    (and (= b-type 's) (= a-type 'a)) 1
    (and (= a-type 's) (= b-type 's)) (compare a-val b-val)
    (or rest-a rest-b) (recur rest-a rest-b)
    :else (compare (hash a-val) (hash b-val))))

(defn flat
  "Flattens a structure of associatives and sequentials to an
  associative of paths to leaf values. Each step in a path will retain
  both the type and the edge value, maps will be flattened to
  associatives. Vectors, lists, and other seqs will be flattened to
  sequentials."
  [c]
  (if (or (not (coll? c))
          (empty? c))
    c
    (let [marker (if (map? c) 'a 's)
          m (if (map? c)
              c
              (map-indexed (fn [i e] [i e]) c))]
      (into {}
            (mapcat (fn [[k v]]
                      (let [child-flat (flat v)]
                        (if (or (not (coll? child-flat))
                                (empty? child-flat))
                          [[[[marker k]] child-flat]]
                          (for [[path element] child-flat]
                            [(apply vector [marker k] path) element])))))
            m))))

(defn access-path
  "`flat` returns a reconstitution path of both type and key. This is
  useful for reprojecting the flat version up into a nested structure,
  but is not amenable to `get-in`, `assoc-in`, et al. `access-path` will
  convert a reconstitution path into a more conventional access path."
  [reconstitution-path]
  (map second reconstitution-path))

(defn- assoc*
  [m [type key] v]
  (let [m (if m
            m
            (cond
              (= type 'a) {}
              (= type 's) []))
        assoc-fn (if (vector? m)
                   ic/assoc-n-with-tail-support
                   assoc)]
    (assoc-fn m key v)))

(defn- assoc-in*
  [m [[type key :as kspec] & kspecs] v]
  (if kspecs
    (assoc* m kspec (assoc-in* (get m key) kspecs v))
    (assoc* m kspec v)))

(defn project
  "Constitutes a structure as produced by `flat` up into a nested
  collection of maps and vectors. As all sequential items are coerced
  to vectors, this is not reflexive of `flat`."
  ([vals] (project vals nil))
  ([vals basis]
   (reduce (fn [m [k v]]
             (assoc-in* m k v))
           basis
           vals)))

(defn clean*
  "Remove the key at k and any empty parents above it."
  [m [k & ks]]
  (if ks
    (let [cleaned (clean* (get m k) ks)]
      (if (empty? cleaned)
        (cond (vector? m) (pop m)
              (map? m) (dissoc m k))
        (assoc m k cleaned)))
    (cond (vector? m) (pop m)
          (map? m) (dissoc m k))))

(defn nested-update
  "Does all of the book-keeping for nested map/vector combo data types.
  `original` is the original data structure, `provenance` is the
  original structure's provenance data, `update` is a update function
  to progress original.
  Returns positionally: the updated `original`, and, the updated `provenance`."
  [original provenance update author timestamp]
  (let [updated (update original)
        original-vals-flat (flat original)
        update-vals-flat (flat updated)
        [deletions additions common] (diff original-vals-flat update-vals-flat)
        provenance (reduce (fn [m [k v]]
                             (if (contains? additions k)
                               m
                               (clean* m (access-path k))))
                           provenance
                           deletions)
        addition-dots (for [[k v] (sort-by first compare-paths additions)]
                        (let [to-vector? (= 's (first (last k)))
                              distinct? (not (contains? deletions k))
                              basis {:a author
                                     :t timestamp}]
                          [k (if to-vector?
                               (merge basis {:i (if distinct? -1 (last (last k)))})
                               basis)]))]
    [updated (project addition-dots provenance)]))

(defn finalize-projection-key
  [m]
  (let [{:keys [entry insert-index]} (:data m)]
    (if insert-index
      (assoc-in m [:data :entry]
                [(conj (pop (key entry)) ['s insert-index]) (val entry)])
      m)))
