(ns schism.impl.types.nested-vector
  "Definition and support for Schism's Deeply Convergent Vector
  type. The convergent vector is a timestamped log of entries with a
  vector clock & insertion index. Convergence places entries into the
  resultant vector in insertion order, with insertions occurring by
  replaying insertions operations in order. The vector clock conveys
  that an item has been removed from the vector on another node. This
  variant provides rich support for serialization and convergence of
  deeply nested structures, at the cost that all modification
  operations take linear time instead of constant or log time."
  (:require [schism.impl.types.nesting-util :as nu]
            [schism.impl.protocols :as proto]
            [schism.impl.vector-clock :as vc]
            [schism.impl.core :as ic]
            [schism.node :as node]
            [clojure.set :as set]
            [clojure.data :refer [diff]]
            #?(:cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [schism.impl.vector-clock :as vc]))
  #?(:clj (:import (clojure.lang IPersistentCollection IPersistentStack IPersistentVector Reversible IReduce IKVReduce Indexed Associative Counted IHashEq Seqable IObj IMeta IFn ILookup)
                   (java.io Writer)
                   (java.util Date Collection)
                   (java.lang Object Long))))

(declare nested-vector-conj nested-vector-pop nested-vector-empty nested-vector-assoc)

#?(:clj (deftype NestedVector [data vclock insertions]
          Counted
          (count [this] (.count ^Counted (.-data this)))

          IPersistentCollection
          (cons [this o] (nested-vector-conj this o))
          (empty [this] (nested-vector-empty this))
          (equiv [this other] (.equiv ^IPersistentCollection (.-data this) other))

          Object
          (equals [this o]
            (.equals (.-data this) o))
          (hashCode [this]
            (.hashCode (.-data this)))
          (toString [this]
            (.toString (.-data this)))

          IHashEq
          (hasheq [this]
            (.hasheq ^IHashEq (.-data this)))

          Seqable
          (seq [this]
            (seq ^Seqable (.-data this)))

          java.util.List
          (add [this o] (.add ^java.util.List (.-data this) o))
          (add [this index o] (.add ^java.util.List (.-data this) index o))
          (addAll [this c] (.addAll ^java.util.List (.-data this) c))
          (clear [this] (.clear ^java.util.List (.-data this)))
          (contains [this o] (.contains ^java.util.List (.-data this) o))
          (containsAll [this c] (.containsAll ^java.util.List (.-data this) c))
          (get [this i] (.get ^java.util.List (.-data this) i))
          (indexOf [this o] (.indexOf ^java.util.List (.-data this) o))
          (isEmpty [this] (.isEmpty ^java.util.List (.-data this)))
          (iterator [this] (.iterator ^java.util.List (.-data this)))
          (lastIndexOf [this o] (.lastIndexOf ^java.util.List (.-data this) o))
          (listIterator [this] (.listIterator ^java.util.List (.-data this)))
          (^Object remove [this ^int i] (.remove ^java.util.List (.-data this) i))
          (^boolean remove [this ^Object o] (.remove ^java.util.List (.-data this) o))
          (removeAll [this c] (.removeAll ^java.util.List (.-data this) c))
          (replaceAll [this op] (.replaceAll ^java.util.List (.-data this) op))
          (retainAll [this c] (.retainAll ^java.util.List (.-data this) c))
          (set [this i e] (.set ^java.util.List (.-data this) i e))
          (size [this] (.size ^java.util.List (.-data this)))
          (sort [this c] (.sort ^java.util.List (.-data this) c))
          (spliterator [this] (.spliterator ^java.util.List (.-data this)))
          (subList [this i j] (.subList ^java.util.List (.-data this) i j))
          (toArray [this] (.toArray ^java.util.List (.-data this)))
          (^"[Ljava.lang.Object;" toArray [this ^"[Ljava.lang.Object;" a]
           (.toArray ^java.util.List (.-data this) a))


          IObj
          (withMeta [this meta]
            (NestedVector. (with-meta ^IObj (.-data this)
                                 meta)
                               (.-vclock this)
                               (.-insertions this)))

          IMeta
          (meta [this]
            (.meta ^IMeta (.-data this)))

          IReduce
          (reduce [this f]
            (.reduce ^IReduce (.-data this) f))

          IKVReduce
          (kvreduce [this f init]
            (.kvreduce ^IKVReduce (.-data this) f init))

          IPersistentStack
          (peek [this] (.peek ^IPersistentStack (.-data this)))
          (pop [this]
            (nested-vector-pop this))

          IPersistentVector
          (assocN [this i v]
            (nested-vector-assoc this i v))

          ILookup
          (valAt [this k]
            (.valAt (.-data this) k))
          (valAt [this k not-found]
            (.valAt (.-data this) k not-found))

          Associative
          (containsKey [this k]
            (.containsKey ^Associative (.-data this) k))
          (entryAt [this k]
            (.entryAt ^Associative (.-data this) k))
          (assoc [this k v]
            (nested-vector-assoc this k v))

          Indexed
          (nth [this i]
            (.indexed ^Indexed (.-data this) i))
          (nth [this i not-found]
            (.indexed ^Indexed (.-data this) i not-found))

          IFn
          (invoke [this k]
            (.invoke ^IFn (.-data this) k))
          (invoke [this k not-found]
            (.invoke ^IFn (.-data this) k not-found)))
   :cljs (deftype NestedVector [data vclock insertions]
           ICounted
           (-count [this] (-count (.-data this)))

           IEmptyableCollection
           (-empty [this] (nested-vector-empty this))

           ICollection
           (-conj [this o] (nested-vector-conj this o))

           IEquiv
           (-equiv [this other]
             (-equiv (.-data this) other))

           IPrintWithWriter
           (-pr-writer [o writer opts]
             (-write writer "#schism/nested-vector [")
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
             (NestedVector. (-with-meta (.-data this)
                                            meta)
                                (.-vclock this)
                                (.-insertions this)))

           IFn
           (-invoke [this k]
             ((.-data this) k))
           (-invoke [this k not-found]
             ((.-data this) k not-found))

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
             (nested-vector-assoc this k v))

           IFind
           (-find [this k]
             (-find (.-data this) k))

           IStack
           (-peek [this]
             (-peek (.-data this)))
           (-pop [this]
             (nested-vector-pop this))

           IVector
           (-assoc-n [this n v]
             (nested-vector-assoc this n v))

           IReduce
           (-reduce [this f]
             (-reduce (.-data this) f))
           (-reduce [this f start]
             (-reduce (.-data this) f start))

           IKVReduce
           (-kv-reduce [this f init]
             (-kv-reduce (.-data this) f init))))

(defn nested-vector-conj [^NestedVector nvector o]
  (vc/update-clock now nvector
                   (let [[updated updated-dots] (nu/nested-update (.-data nvector)
                                                                  (.-insertions nvector)
                                                                  #(conj % o)
                                                                  node/*current-node*
                                                                  now)]
                     (NestedVector. updated
                                    (.-vclock nvector)
                                    updated-dots))))

(defn nested-vector-empty [^NestedVector nvector]
  (vc/update-clock _ nvector
                   (NestedVector. (vector)
                                  (hash-map)
                                  (vector))))

(defn nested-vector-pop [^NestedVector nvector]
  (vc/update-clock _ nvector
                   (NestedVector. (pop (.-data nvector))
                                  (.-vclock nvector)
                                  (pop (.-insertions nvector)))))

(defn nested-vector-assoc [^NestedVector nvector k v]
  (vc/update-clock now nvector
                   (let [[updated updated-dots] (nu/nested-update (.-data nvector)
                                                                  (.-insertions nvector)
                                                                  #(assoc % k v)
                                                                  node/*current-node*
                                                                  now)]
                    (NestedVector. updated
                                   (.-vclock nvector)
                                   updated-dots))))

(defn- elemental-data
  [^NestedVector v]
  (let [flat-data (nu/flat (.-data v))]
    {:vector-clock (.-vclock v)
     :elements (into []
                     (for [datum flat-data]
                       (let [dot (get-in (.-insertions v) (nu/access-path (key datum)))]
                        {:data {:entry datum
                                :insert-index (:i dot)}
                         :author-node (:a dot)
                         :record-time (:t dot)})))}))

(extend-type NestedVector
  proto/Vclocked
  (get-clock [this] (.-vclock this))
  (with-clock [this new-clock] (NestedVector. (.-data this)
                                                  new-clock
                                                  (.-insertions this)))

  proto/Convergent
  (synchronize [this ^NestedVector other]
    (let [own-meta (-> this .-data meta)
          own-data (elemental-data this)
          other-data (elemental-data other)
          retain (filter (ic/common-elements own-data other-data)
                         (:elements own-data))
          completed-elements (->> (apply ic/retain-elements
                                         (ic/distinct-data own-data other-data))
                                  (concat retain)
                                  (sort-by :record-time)
                                  (map nu/finalize-projection-key))
          completed-flat-data (map (comp :entry :data) completed-elements)
          completed-flat-insertions (map (fn [{:keys [author-node record-time]
                                          {:keys [insert-index entry] :as data} :data}]
                                      (let [dot {:a author-node :t record-time}]
                                        [(first entry) (if insert-index
                                                         (assoc dot :i insert-index)
                                                         dot)]))
                                       completed-elements)
          completed-vclock (ic/merged-clock completed-elements own-data other-data)]
      (vc/update-clock _ this
                       (NestedVector. (with-meta (nu/project completed-flat-data) own-meta)
                                      completed-vclock
                                      (nu/project completed-flat-insertions))))))

#?(:clj (defmethod print-method NestedVector
          [^NestedVector v ^Writer writer]
          (.write writer "#schism/nested-vector [")
          (.write writer (pr-str (.-data v)))
          (.write writer ", ")
          (.write writer (pr-str (.-vclock v)))
          (.write writer ", ")
          (.write writer (pr-str (.-insertions v)))
          (.write writer "]")))

(defn read-edn-vector
  [read-object]
  (let [[data vclock insertions] read-object]
    (NestedVector. data vclock insertions)))

#?(:cljs (cljs.reader/register-tag-parser! 'schism/nested-vector read-edn-vector))

(defn new-vector
  ([] (NestedVector. (vector)
                     (hash-map)
                     (vector)))
  ([& args] (vc/update-clock now nil
                             (let [[updated updated-dots] (nu/nested-update []
                                                                  []
                                                                  #(into % args)
                                                                  node/*current-node*
                                                                  now)]
                               (NestedVector. updated
                                              (hash-map)
                                              updated-dots)))))
