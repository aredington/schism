(ns schism.types.set
  "Definition and support for Schism's Convergent Set type, an ORSWOT
  implemented on top of Clojure's Persistent Set, Persistent Map and
  Schism's Vector Clock."
  (:require [schism.protocols :as proto]
            [schism.vector-clock :as vc]
            [schism.node :as node]
            [clojure.set :as set]
            #?(:cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [schism.vector-clock :as vc]))
  #?(:clj (:import (clojure.lang IPersistentCollection IPersistentSet Murmur3 IHashEq Counted Seqable RT IFn)
                   (java.io Writer)
                   (java.util Date Collection)
                   (java.lang Object))))

;; A CLJS and CLJ implementation of ORSWOT (Observed-Removed Set without Tombstones)

;; Each set maintains its own vector clock, and birth dots for each
;; member. A birth dot consists of the node adding the member, and the
;; date at which it was added. The vector clock determines if an
;; element has been removed: a compared set absent an element but with
;; a newer vector clock than the own birthdot on element indicates
;; that it was removed; a compared set absent an element with an older
;; vector clock indicates that the birthdot was never seen and should
;; be in the merged set.

;; Dot membership is always exactly the cardinality of the Set. Clock
;; membership is at most the cardinality of the set, but can correctly
;; synchronize with one more member than the total number of nodes in
;; the birth-dots; if a Vclock indicates an element was removed, the
;; node converging its changes can claim responsibility for removing
;; the element in the merged set.

(declare orswot-conj orswot-empty orswot-disj)

#?(:clj (deftype Set [data vclock birth-dots]
          Counted
          (count [this] (count (.data this)))

          IPersistentCollection
          (cons [this o] (orswot-conj this o))
          (empty [this] (orswot-empty this))
          (equiv [this other]
            (= (.data this) other))

          IPersistentSet
          (disjoin [this o] (orswot-disj this o))
          (contains [this o] (contains? (.data this) o))
          (get [this o] (get (.data this) o))

          Object
          (equals [this o]
            (or (identical? this o)
                (and (instance? java.util.Set o)
                     (= (.size o) (count this))
                     (every? (partial contains? this) o))))
          (hashCode [this]
            (reduce (fn [m o] (+ m (.hashCode o))) 0 (seq this)))
          (toString [this]
            (.toString data))

          IHashEq
          (hasheq [this]
            (Murmur3/hashUnordered (.data this)))

          Seqable
          (seq [this] (seq (.data this)))

          java.util.Set
          (toArray [this o] (RT/seqToArray (seq this)))
          (add [this o] (throw (UnsupportedOperationException.)))
          (remove [this o] (throw (UnsupportedOperationException.)))
          (addAll [this c] (throw (UnsupportedOperationException.)))
          (clear [this] (throw (UnsupportedOperationException.)))
          (retainAll [this c] (throw (UnsupportedOperationException.)))
          (removeAll [this c] (throw (UnsupportedOperationException.)))
          (containsAll [this c] (.containsAll (.data this) c))
          (size [this] (count (.data this)))
          (isEmpty [this] (zero? (count (.data this))))
          (iterator [this] (.iterator (seq this)))

          IFn
          (invoke [this arg1]
            (get this arg1)))
   :cljs (deftype Set [data vclock birth-dots]
           ICounted
           (-count [this] (count (.-data this)))

           IEmptyableCollection
           (-empty [this] (orswot-empty this))

           ICollection
           (-conj [this o] (orswot-conj this o))

           ISet
           (-disjoin [this o] (orswot-disj this o))

           IEquiv
           (-equiv [this other]
             (= (.-data this) other))

           ILookup
           (-lookup [this o]
             (-lookup (.-data this) o))
           (-lookup [this o not-found]
             (-lookup (.-data this) o not-found))

           IPrintWithWriter
           (-pr-writer [o writer opts]
             (-write writer "#schism/set [")
             (-write writer (pr-str (.-data o)))
             (-write writer ", ")
             (-write writer (pr-str (.-vclock o)))
             (-write writer ", ")
             (-write writer (pr-str (.-birth-dots o)))
             (-write writer "]"))

           IHash
           (-hash [this] (hash-unordered-coll this))

           IFn
           (-invoke [this o] (-lookup this o))

           ISeqable
           (-seq [this] (-seq (.-data this)))

           Object
           (toString [this] (.toString (.-data this)))))

(defn orswot-conj
  [^Set orswot o]
  (vc/update-clock now
                   (Set. (conj (.-data orswot) o)
                         (.-vclock orswot)
                         (assoc (.-birth-dots orswot) o [node/*current-node* now]))))

(defn orswot-empty
  [_]
  (vc/update-clock _
                   (Set. (hash-set)
                         (hash-map)
                         (hash-map))))

(defn orswot-disj
  [^Set orswot o]
  (vc/update-clock _
                   (Set. (disj (.-data orswot) o)
                         (.-vclock orswot)
                         (dissoc (.-birth-dots orswot) o))))

(extend-type Set
  proto/Vclocked
  (get-clock [this] (.-vclock this))
  (with-clock [this new-clock] (Set. (.-data this)
                                     new-clock
                                     (.-birth-dots this)))

  proto/Convergent
  (synchronize [this other]
    (let [own-clock (.-vclock this)
          own-data (.-data this)
          own-dots (.-birth-dots this)
          other-clock (.-vclock other)
          other-data (.-data other)
          other-dots (.-birth-dots other)
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

#?(:clj (defmethod print-method Set
          [^Set s ^Writer writer]
          (.write writer "#schism/set [")
          (.write writer (pr-str (.data s)))
          (.write writer ", ")
          (.write writer (pr-str (.vclock s)))
          (.write writer ", ")
          (.write writer (pr-str (.birth-dots s)))
          (.write writer "]")))

(defn read-edn-set
  [read-object]
  (let [[data vclock birth-dots] read-object]
    (Set. data vclock birth-dots)))

#?(:cljs (cljs.reader/register-tag-parser! 'schism/set read-edn-set))

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
