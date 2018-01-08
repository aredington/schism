(ns schism.types.set
  "Definition and support for Schism's Convergent Set type, an ORSWOT
  implemented on top of Clojure's Persistent Set, Persistent Map and
  Schism's Vector Clock."
  (:require [schism.protocols :as proto]
            [schism.vector-clock :as vc]
            [schism.node :as node]
            [clojure.set :as set])
  (:import (clojure.lang IPersistentCollection IPersistentSet)
           (java.io Writer)
           (java.util Date)))

(deftype Set [data vclock birth-dots]

  IPersistentCollection
  (count [_] (count data))
  (cons [_ o] (vc/update-clock now
                               (Set. (conj data o)
                                     vclock
                                     (assoc birth-dots o [node/*current-node* now]))))
  (empty [_] (vc/update-clock _
                              (Set. (hash-set)
                                    (hash-map)
                                    (hash-map))))
  (equiv [_ other]
    (= data other))

  IPersistentSet
  (disjoin [_ o] (vc/update-clock _
                                  (Set. (disj data o)
                                        vclock
                                        (dissoc birth-dots o))))
  (contains [_ o] (contains? data o))
  (get [_ o] (get data o))

  proto/Vclocked
  (get-clock [_] vclock)
  (with-clock [_ new-clock] (Set. data
                                  new-clock
                                  birth-dots))

  proto/Convergent
  (synchronize [this other]
    (let [own-clock (.vclock this)
          own-data (.data this)
          own-dots (.birth-dots this)
          other-clock (.vclock other)
          other-data (.data other)
          other-dots (.birth-dots other)
          retain (set/intersection own-data other-data)
          timefn (memfn getTime)
          other-addition-threshold (timefn (own-clock node/*current-node*))
          other-additions (->> own-data
                               (set/difference other-data)
                               (remove #(> other-addition-threshold (timefn (last (other-dots %))))))
          own-addition-threshold (->> other-clock
                                      vals
                                      (map timefn)
                                      (apply max))
          own-additions  (->> other-data
                              (set/difference own-data)
                              (remove #(>  own-addition-threshold (timefn (last (own-dots %))))))
          completed-data (into retain (concat other-additions own-additions))
          completed-birth-dots (->>  completed-data
                                     (map (fn [e] (let [own-val (own-dots e)
                                                        other-val (other-dots e)
                                                        candidates (remove nil? [own-val other-val])]
                                                    [e (if (> (count candidates) 1)
                                                         (apply max-key (comp timefn last) candidates)
                                                         (first candidates))])))
                                     (into {}))
          relevant-nodes (->> completed-birth-dots
                              vals
                              (map first)
                              set)
          completed-vclock (-> (merge-with (partial max-key timefn) own-clock other-clock)
                               (select-keys relevant-nodes))]
      (vc/update-clock _
                       (Set. completed-data
                             completed-vclock
                             completed-birth-dots)))))

(defmethod print-method Set
  [^Set s ^Writer writer]
  (.write writer "#schism/set [")
  (.write writer (pr-str (.data s)))
  (.write writer ", ")
  (.write writer (pr-str (.vclock s)))
  (.write writer ", ")
  (.write writer (pr-str (.birth-dots s)))
  (.write writer "]"))

(defn read-edn-set
  [read-object]
  (let [[data vclock birth-dots] read-object]
    (Set. data vclock birth-dots)))

(defn new-set
  ([] (Set. (hash-set)
            (hash-map)
            (hash-map)))
  ([& args] (vc/update-clock now
                             (Set. (apply hash-set args)
                                   (hash-map)
                                   (apply hash-map
                                          (mapcat (fn [o]
                                                    [o [node/*current-node* now]])
                                                  args))))))
