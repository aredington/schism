(ns schism.types.list
  "Definition and support for Schism's Convergent List type. The
  convergent list is a simple timestamped log of entries with a vector
  clock. Convergence places entries into the resultant list in
  insertion order. The vector clock conveys that an item has been
  removed from the list on another node."
  (:require [schism.protocols :as proto]
            [schism.vector-clock :as vc]
            [schism.node :as node]
            [clojure.set :as set]
            #?(:cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [schism.vector-clock :as vc]))
  #?(:clj (:import (clojure.lang IPersistentCollection IPersistentStack IReduce Counted IHashEq Seqable IObj IMeta ISeq)
                   (java.io Writer)
                   (java.util Date Collection)
                   (java.lang Object))))

;; A CLJ & CLJS implementation of a convergent list

;; Each list maintains its own vector clock, and insertion times for
;; each element of the list. List entries and insertions are
;; correlated positionally (as the list may contain the same item
;; multiple times.) Insertion times dictate ordering. The vector clock
;; determines if an entry has been removed.

(declare clist-conj clist-rest clist-empty)

#?(:clj (deftype List [data vclock insertions]
          Counted
          (count [this] (.count ^Counted (.data this)))

          IPersistentCollection
          (cons [this o] (clist-conj this o))
          (empty [this] (clist-empty this))
          (equiv [this other] (.equiv ^IPersistentCollection (.data this) other))

          Object
          (equals [this o]
            (.equals (.data this) o))
          (hashCode [this]
            (.hashCode (.data this)))
          (toString [this]
            (.toString (.data this)))

          IHashEq
          (hasheq [this]
            (.hasheq ^IHashEq (.data this)))

          Seqable
          (seq [this]
            this)

          java.util.List
          (add [this o] (.add ^java.util.List (.data this) o))
          (add [this index o] (.add ^java.util.List (.data this) index o))
          (addAll [this c] (.addAll ^java.util.List (.data this) c))
          (clear [this] (.clear ^java.util.List (.data this)))
          (contains [this o] (.contains ^java.util.List (.data this) o))
          (containsAll [this c] (.containsAll ^java.util.List (.data this) c))
          (get [this i] (.get ^java.util.List (.data this) i))
          (indexOf [this o] (.indexOf ^java.util.List (.data this) o))
          (isEmpty [this] (.isEmpty ^java.util.List (.data this)))
          (iterator [this] (.iterator ^java.util.List (.data this)))
          (lastIndexOf [this o] (.lastIndexOf ^java.util.List (.data this) o))
          (listIterator [this] (.listIterator ^java.util.List (.data this)))
          (^Object remove [this ^int i] (.remove ^java.util.List (.data this) i))
          (^boolean remove [this ^Object o] (.remove ^java.util.List (.data this) o))
          (removeAll [this c] (.removeAll ^java.util.List (.data this) c))
          (replaceAll [this op] (.replaceAll ^java.util.List (.data this) op))
          (retainAll [this c] (.retainAll ^java.util.List (.data this) c))
          (set [this i e] (.set ^java.util.List (.data this) i e))
          (size [this] (.size ^java.util.List (.data this)))
          (sort [this c] (.sort ^java.util.List (.data this) c))
          (spliterator [this] (.spliterator ^java.util.List (.data this)))
          (subList [this i j] (.subList ^java.util.List (.data this) i j))
          (toArray [this] (.toArray ^java.util.List (.data this)))
          (toArray [this a] (.toArray ^java.util.List (.data this) a))

          IObj
          (withMeta [this meta]
            (List. (with-meta ^IObj (.data this)
                     meta)
                   (.vclock this)
                   (.insertions this)))

          IMeta
          (meta [this]
            (.meta ^IMeta (.data this)))

          IReduce
          (reduce [this f]
            (.reduce ^IReduce (.data this) f))

          IPersistentStack
          (peek [this] (.peek ^IPersistentStack (.data this)))
          (pop [this]
            (clist-rest this))

          ISeq
          (first [this] (.first ^ISeq (.data this)))
          (next [this] (clist-rest this))
          (more [this] (clist-rest this)))
   :cljs (deftype List [data vclock insertions]
           ICounted
           (-count [this] (-count (.-data this)))

           IEmptyableCollection
           (-empty [this] (clist-empty this))

           ICollection
           (-conj [this o] (clist-conj this o))

           IEquiv
           (-equiv [this other]
             (-equiv (.-data this) other))

           IPrintWithWriter
           (-pr-writer [o writer opts]
             (-write writer "#schism/list [")
             (-write writer (pr-str (.-data o)))
             (-write writer ", ")
             (-write writer (pr-str (.-vclock o)))
             (-write writer ", ")
             (-write writer (pr-str (.-insertions o)))
             (-write writer "]"))

           IHash
           (-hash [this] (-hash (.-data this)))

           ISeqable
           (-seq [this] this)

           Object
           (toString [this] (.toString (.-data this)))

           IMeta
           (-meta [this]
             (-meta (.-data this)))

           IWithMeta
           (-with-meta [this meta]
             (List. (-with-meta (.-data this)
                                meta)
                    (.-vclock this)
                    (.-insertions this)))

           ISeq
           (-first [this]
             (-first (.-data this)))
           (-rest [this]
             (clist-rest this))))

(defn clist-conj [^List clist o]
  (vc/update-clock now
                   (List. (conj (.-data clist) o)
                          (.-vclock clist)
                          (conj (.-insertions clist) [node/*current-node* now]))))

(defn clist-empty [^List clist]
  (vc/update-clock _
                   (List. (list)
                          (hash-map)
                          (list))))

(defn clist-rest [^List clist]
  (vc/update-clock _
                   (List. (rest (.-data clist))
                          (.-vclock clist)
                          (rest (.-insertions clist)))))

(extend-type List
  proto/Vclocked
  (get-clock [this] (.-vclock this))
  (with-clock [this new-clock] (List. (.-data this)
                                      new-clock
                                      (.-insertions this)))

  proto/Convergent
  (synchronize [this ^List other]
    (let [own-clock (.-vclock this)
          own-data (.-data this)
          own-insertions (.-insertions this)
          own-tuples (map (partial apply vector) own-data own-insertions)
          own-meta (meta own-data)
          other-clock (.-vclock this)
          other-data (.-data other)
          other-insertions (.-insertions other)
          other-tuples (map (partial apply vector) other-data other-insertions)
          ;; Can't use set logic because could insert the same value
          ;; multiple times in the same millisecond: instead find the
          ;; common tail between own and other data.
          common-tail (->> (map vector (reverse own-tuples) (reverse other-tuples))
                           (take-while (partial apply =))
                           reverse
                           (map first))
          timefn (memfn ^Date getTime)
          other-addition-threshold (timefn (own-clock node/*current-node*))
          other-addition-tuples (take-while #(< other-addition-threshold (timefn (last %))) other-tuples)
          own-addition-threshold (->> other-clock
                                      vals
                                      (map timefn)
                                      (apply max))
          own-addition-tuples (take-while #(< own-addition-threshold (timefn (last %))) own-tuples)
          common-addition-tuples (->> (into '() (concat own-addition-tuples other-addition-tuples))
                                      (sort-by last))
          all-pairs (concat common-addition-tuples common-tail)
          merged-vclock (merge-with (partial max-key timefn) own-clock other-clock)]
      (vc/update-clock _
                       (List. (with-meta (map first all-pairs)
                                own-meta)
                              merged-vclock
                              (map last all-pairs))))))

#?(:clj (defmethod print-method List
          [^List l ^Writer writer]
          (.write writer "#schism/list [")
          (.write writer (pr-str (.data l)))
          (.write writer ", ")
          (.write writer (pr-str (.vclock l)))
          (.write writer ", ")
          (.write writer (pr-str (.insertions l)))
          (.write writer "]")))

(defn read-edn-list
  [read-object]
  (let [[data vclock insertions] read-object]
    (List. data vclock insertions)))

#?(:cljs (cljs.reader/register-tag-parser! #'schism/list read-edn-list))

(defn new-list
  ([] (List. (list)
             (hash-map)
             (list)))
  ([& args] (vc/update-clock now
                             (List. (apply list args)
                                    (hash-map)
                                    (apply list (repeat (count args) [node/*current-node* now]))))))
