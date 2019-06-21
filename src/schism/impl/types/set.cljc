(ns schism.impl.types.set
  "Definition and support for Schism's Convergent Set type, an ORSWOT
  implemented on top of Clojure's Persistent Set, Persistent Map and
  Schism's Vector Clock."
  (:require [schism.impl.protocols :as proto]
            [schism.impl.vector-clock :as vc]
            [schism.impl.core :as ic]
            [schism.node :as node]
            [clojure.set :as set]
            #?(:cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [schism.impl.vector-clock :as vc]))
  #?(:clj (:import (clojure.lang IPersistentCollection IPersistentSet IHashEq Counted Seqable RT IFn IObj IMeta)
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

;; TODO:

;; With vector clocks AND birthdots it is possible to disambiguate the
;; removal of an element from the post-replication addition of that
;; element. When merging other, examine it's vector clock for each
;; entry that is uniquely in own. If that entry's authoring node is
;; present in the vector clock, and the timestamp in other's vector
;; clock is less than the birth dot of own's entry, then retain the
;; entry, as other never saw it and could not have removed it.

(declare orswot-conj orswot-empty orswot-disj)

#?(:clj (deftype Set [data vclock birth-dots]
          Counted
          (count [this] (.count ^Counted (.-data this)))

          IPersistentCollection
          (cons [this o] (orswot-conj this o))
          (empty [this] (orswot-empty this))
          (equiv [this other]
            (.equiv ^IPersistentCollection (.-data this) other))

          IPersistentSet
          (disjoin [this o] (orswot-disj this o))
          (contains [this o] (.contains ^IPersistentSet (.-data this) o))
          (get [this o] (.get ^IPersistentSet (.-data this) o))

          Object
          (equals [this o]
            (.equals (.-data this) o))
          (hashCode [this]
            (.hashCode (.-data this)))
          (toString [this]
            (.toString data))

          IHashEq
          (hasheq [this]
            (.hasheq ^IHashEq (.-data this)))

          Seqable
          (seq [this] (.seq ^Seqable (.-data this)))

          java.util.Set
          (toArray [this] (.toArray (.-data this)))
          (^"[Ljava.lang.Object;" toArray [this ^"[Ljava.lang.Object;" a]
           (.toArray ^java.util.Set (.-data this) a))
          (add [this o] (throw (UnsupportedOperationException.)))
          (remove [this o] (throw (UnsupportedOperationException.)))
          (addAll [this c] (throw (UnsupportedOperationException.)))
          (clear [this] (throw (UnsupportedOperationException.)))
          (retainAll [this c] (throw (UnsupportedOperationException.)))
          (removeAll [this c] (throw (UnsupportedOperationException.)))
          (containsAll [this c] (.containsAll ^java.util.Set (.-data this) c))
          (size [this] (.size ^java.util.Set (.-data this)))
          (isEmpty [this] (.isEmpty ^java.util.Set (.-data this)))
          (iterator [this] (.iterator ^java.util.Set (.-data this)))

          IFn
          (invoke [this arg1]
            (.invoke ^IFn (.-data this) arg1))

          IObj
          (withMeta [this meta]
            (Set. (.withMeta ^IObj (.-data this) meta) (.-vclock this) (.-birth-dots this)))

          IMeta
          (meta [this]
            (.meta ^IMeta (.-data this))))
   :cljs (deftype Set [data vclock birth-dots]
           ICounted
           (-count [this] (-count (.-data this)))

           IEmptyableCollection
           (-empty [this] (orswot-empty this))

           ICollection
           (-conj [this o] (orswot-conj this o))

           ISet
           (-disjoin [this o] (orswot-disj this o))

           IEquiv
           (-equiv [this other]
             (-equiv (.-data this) other))

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
           (-hash [this] (-hash (.-data this)))

           IFn
           (-invoke [this o] (-invoke (.-data this) o))

           ISeqable
           (-seq [this] (-seq (.-data this)))

           Object
           (toString [this] (.toString (.-data this)))

           IMeta
           (-meta [this]
             (-meta (.-data this)))

           IWithMeta
           (-with-meta [this meta]
             (Set. (-with-meta (.-data this)
                               meta)
                   (.-vclock this) (.-birth-dots this)))))

(defn orswot-conj
  [^Set orswot o]
  (vc/update-clock now orswot
                   (Set. (conj (.-data orswot) o)
                         (.-vclock orswot)
                         (assoc (.-birth-dots orswot) o [node/*current-node* now]))))

(defn orswot-empty
  [^Set orswot]
  (vc/update-clock _ orswot
                   (Set. (hash-set)
                         (hash-map)
                         (hash-map))))

(defn orswot-disj
  [^Set orswot o]
  (vc/update-clock _ orswot
                   (Set. (disj (.-data orswot) o)
                         (.-vclock orswot)
                         (dissoc (.-birth-dots orswot) o))))

(defn- elemental-data
  [^Set s]
  {:vector-clock (.-vclock s)
   :elements (into []
                   (for [datum (.-data s)]
                     (let [dot (get (.-birth-dots s) datum)]
                       {:data datum
                        :author-node (first dot)
                        :record-time (last dot)})))})

(extend-type Set
  proto/Vclocked
  (get-clock [this] (.-vclock this))
  (with-clock [this new-clock] (Set. (.-data this)
                                     new-clock
                                     (.-birth-dots this)))

  proto/Convergent
  (synchronize [this ^Set other]
    (let [own-meta (-> this .-data meta)
          own-data (elemental-data this)
          other-data (elemental-data other)
          retain (filter (ic/common-elements own-data other-data)
                         (:elements own-data))
          completed-elements (concat (apply ic/retain-elements
                                       (ic/distinct-data own-data other-data))
                                retain)
          completed-data (into #{} (map :data completed-elements))
          completed-birth-dots (->>  completed-elements
                                     (map (fn [{:keys [data author-node record-time]}]
                                            [data [author-node record-time]]))
                                     (into {}))
          completed-vclock (ic/merged-clock completed-elements own-data other-data)]
      (vc/update-clock _ this
                       (Set. (with-meta completed-data
                               own-meta)
                             completed-vclock
                             completed-birth-dots)))))

#?(:clj (defmethod print-method Set
          [^Set s ^Writer writer]
          (.write writer "#schism/set [")
          (.write writer (pr-str (.-data s)))
          (.write writer ", ")
          (.write writer (pr-str (.-vclock s)))
          (.write writer ", ")
          (.write writer (pr-str (.-birth-dots s)))
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
  ([& args] (vc/update-clock now nil
                             (Set. (apply hash-set args)
                                   (hash-map)
                                   (apply hash-map
                                          (mapcat (fn [o]
                                                    [o [node/*current-node* now]])
                                                  args))))))
