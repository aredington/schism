(ns schism.impl.types.vector
  "Definition and support for Schism's Convergent vector type. The
  convergent vector is a timestamped log of entries with a vector
  clock & insertion index. Convergence places entries into the
  resultant vector in insertion order, with insertions occurring by
  replaying insertions operations in order. The vector clock conveys
  that an item has been removed from the vector on another node."
  (:require [schism.impl.protocols :as proto]
            [schism.impl.vector-clock :as vc]
            [schism.impl.core :as ic]
            [schism.node :as node]
            [clojure.set :as set]
            #?(:cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [schism.impl.vector-clock :as vc]))
  #?(:clj (:import (clojure.lang IPersistentCollection IPersistentStack IPersistentVector Reversible IReduce IKVReduce Indexed Associative Counted IHashEq Seqable IObj IMeta IFn)
                   (java.io Writer)
                   (java.util Date Collection)
                   (java.lang Object Long))))

;; A CLJ & CLJS implementation of a convergent vector

;; Each vector maintains its own vector clock, and insertion times and
;; positions for each element of the vector. Vector entries and
;; insertions are correlated positionally (as the vector may contain
;; the same item multiple times.) Insertion times and indices dictate
;; ordering; elements inserted at the tail of the vector are recorded
;; as being inserted with index -1. The vector clock determines if an
;; entry has been removed.

(declare cvector-conj cvector-pop cvector-empty cvector-assoc)

#?(:clj (deftype ConvergentVector [data vclock insertions]
          Counted
          (count [this] (.count ^Counted (.data this)))

          IPersistentCollection
          (cons [this o] (cvector-conj this o))
          (empty [this] (cvector-empty this))
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
            (seq ^Seqable (.data this)))

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
          (^"[Ljava.lang.Object;" toArray [this ^"[Ljava.lang.Object;" a]
           (.toArray ^java.util.List (.-data this) a))


          IObj
          (withMeta [this meta]
            (ConvergentVector. (with-meta ^IObj (.data this)
                                 meta)
                               (.vclock this)
                               (.insertions this)))

          IMeta
          (meta [this]
            (.meta ^IMeta (.data this)))

          IReduce
          (reduce [this f]
            (.reduce ^IReduce (.data this) f))

          IKVReduce
          (kvreduce [this f init]
            (.kvreduce ^IKVReduce (.data this) f init))

          IPersistentStack
          (peek [this] (.peek ^IPersistentStack (.data this)))
          (pop [this]
            (cvector-pop this))

          IPersistentVector
          (assocN [this i v]
            (cvector-assoc this i v))

          Associative
          (containsKey [this k]
            (.containsKey ^Associative (.data this) k))
          (entryAt [this k]
            (.entryAt ^Associative (.data this) k))
          (assoc [this k v]
            (cvector-assoc this k v))

          Indexed
          (nth [this i]
            (.indexed ^Indexed (.data this) i))
          (nth [this i not-found]
            (.indexed ^Indexed (.data this) i not-found))

          IFn
          (invoke [this k]
            (.invoke ^IFn (.data this) k))
          (invoke [this k not-found]
            (.invoke ^IFn (.data this) k not-found)))
   :cljs (deftype ConvergentVector [data vclock insertions]
           ICounted
           (-count [this] (-count (.-data this)))

           IEmptyableCollection
           (-empty [this] (cvector-empty this))

           ICollection
           (-conj [this o] (cvector-conj this o))

           IEquiv
           (-equiv [this other]
             (-equiv (.-data this) other))

           IPrintWithWriter
           (-pr-writer [o writer opts]
             (-write writer "#schism/vector [")
             (-write writer (pr-str (.-data o)))
             (-write writer ", ")
             (-write writer (pr-str (.-vclock o)))
             (-write writer ", ")
             (-write writer (pr-str (.-insertions o)))
             (-write writer "]"))

           IHash
           (-hash [this] (-hash (.-data this)))

           ISeqable
           (-seq [this] (-seq (.-data this)))

           Object
           (toString [this] (.toString (.-data this)))

           IMeta
           (-meta [this]
             (-meta (.-data this)))

           IWithMeta
           (-with-meta [this meta]
             (ConvergentVector. (-with-meta (.-data this)
                                            meta)
                                (.-vclock this)
                                (.-insertions this)))

           IFn
           (-invoke [this k]
             (-invoke (.-data this) k))
           (-invoke [this k not-found]
             (-invoke (.-data this) k not-found))

           IIndexed
           (-nth [this n]
             (-nth (.-data this) n))
           (-nth [this n not-found]
             (-nth (.-data this) n not-found))

           ILookup
           (-lookup [this k]
             (-lookup (.-data this) k))
           (-lookup [this k not-found]
             (-lookup (.-data this) k not-found))

           IAssociative
           (-contains-key? [this k]
             (-contains-key? (.-data this) k))
           (-assoc [this k v]
             (cvector-assoc this k v))

           IFind
           (-find [this k]
             (-find (.-data this) k))

           IStack
           (-peek [this]
             (-peek (.-data this)))
           (-pop [this]
             (cvector-pop this))

           IVector
           (-assoc-n [this n v]
             (cvector-assoc this n v))

           IReduce
           (-reduce [this f]
             (-reduce (.-data this) f))
           (-reduce [this f start]
             (-reduce (.-data this) f start))

           IKVReduce
           (-kv-reduce [this f init]
             (-kv-reduce (.-data this) f init))))

(defn cvector-conj [^ConvergentVector cvector o]
  (vc/update-clock now
                   (ConvergentVector. (conj (.-data cvector) o)
                                      (.-vclock cvector)
                                      (conj (.-insertions cvector) [node/*current-node* -1 now]))))

(defn cvector-empty [^ConvergentVector cvector]
  (vc/update-clock _
                   (ConvergentVector. (vector)
                                      (hash-map)
                                      (vector))))

(defn cvector-pop [^ConvergentVector cvector]
  (vc/update-clock _
                   (ConvergentVector. (pop (.-data cvector))
                                      (.-vclock cvector)
                                      (pop (.-insertions cvector)))))

(defn cvector-assoc [^ConvergentVector cvector k v]
  (vc/update-clock now
                   (ConvergentVector. (assoc (.-data cvector) k v)
                                      (.-vclock cvector)
                                      (assoc (.-insertions cvector) k [node/*current-node* k now]))))

(defn assoc-n-with-tail-support
  "Assoc with support to place `v` at the tail of `a` when `n` is -1."
  [a n v]
  (if (= n -1)
    (conj a v)
    (assoc a n v)))

(def tail-insertion-sort-value
  "The value to use when sorting insertions by index, and the recorded
  index was -1, indicating the element was inserted at the tail."
  #?(:clj Long/MAX_VALUE
     :cljs (.-MAX_SAFE_INTEGER js/Number)))

(defn- elemental-data
  [^ConvergentVector v]
  {:vector-clock (.vclock v)
   :elements (into []
                   (for [[element [author-node insert-index record-time]]
                         (map vector (.-data v) (.-insertions v))]
                     {:data {:element element
                             :insert-index insert-index}
                      :author-node author-node
                      :record-time record-time}))})

(extend-type ConvergentVector
  proto/Vclocked
  (get-clock [this] (.-vclock this))
  (with-clock [this new-clock] (ConvergentVector. (.-data this)
                                                  new-clock
                                                  (.-insertions this)))

  proto/Convergent
  (synchronize [this ^ConvergentVector other]
    (let [own-meta (-> this .-data meta)
          own-data (elemental-data this)
          other-data (elemental-data other)
          retain (filter (ic/common-elements own-data other-data)
                         (:elements own-data))
          completed-elements (->> retain
                                  (concat (apply ic/retain-elements
                                                 (ic/distinct-data own-data other-data)))
                                  (sort-by (fn [{:keys [author-node data record-time]}]
                                             (let [{:keys [insert-index]} data]
                                               [(if (= -1 insert-index)
                                                  tail-insertion-sort-value
                                                  insert-index) record-time]))))
          ;; Given the potential for insertion at arbitrary indexes,
          ;; trying to find a common contiguous chunk is less fruitful
          ;; than taking our [element, node, index, timestamp] tuples
          ;; and treating them as discrete insertion
          ;; instructions. Removal is still indicated by vector clock,
          ;; AND it is reasonable to expect much of the vector to be
          ;; shared, so it's important to get to the right set of such
          ;; instructions. As time and index dictate the overall state
          ;; of convergence, it is not important to preserve ordering
          ;; through convergence, which affords using set logic to
          ;; find the common insertions.
          completed-data (reduce (fn [m element]
                                   (let [{:keys [element insert-index]} (:data element)]
                                     (assoc-n-with-tail-support m insert-index element)))
                                 []
                                 completed-elements)
          completed-insertions (reduce (fn [m {:keys [author-node record-time]
                                               {:keys [insert-index]} :data}]
                                         (assoc-n-with-tail-support m insert-index
                                                                    [author-node insert-index record-time]))
                                       []
                                       completed-elements)
          completed-vclock (ic/merged-clock completed-elements own-data other-data)]
      (vc/update-clock _
                       (ConvergentVector. (with-meta completed-data own-meta)
                                          completed-vclock
                                          completed-insertions)))))

#?(:clj (defmethod print-method ConvergentVector
          [^ConvergentVector v ^Writer writer]
          (.write writer "#schism/vector [")
          (.write writer (pr-str (.data v)))
          (.write writer ", ")
          (.write writer (pr-str (.vclock v)))
          (.write writer ", ")
          (.write writer (pr-str (.insertions v)))
          (.write writer "]")))

(defn read-edn-vector
  [read-object]
  (let [[data vclock insertions] read-object]
    (ConvergentVector. data vclock insertions)))

#?(:cljs (cljs.reader/register-tag-parser! 'schism/vector read-edn-vector))

(defn new-vector
  ([] (ConvergentVector. (vector)
                         (hash-map)
                         (vector)))
  ([& args] (vc/update-clock now
                             (ConvergentVector. (apply vector args)
                                                (hash-map)
                                                (apply vector (for [i (range (count args))]
                                                                [node/*current-node* i now]))))))
