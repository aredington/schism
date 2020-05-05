(ns schism.impl.types.nested-map
  "Definition and support for Schism's Deeply Convergent Map type, an
  ORMWOT implemented on top of Clojure's persistent maps and a Schism
  Vector Clock. Contrasted with schism.impl.types.map/Map, this incurs
  substantially greater computational costs for assoc type operations
  and cannot guarantee linear time results."
  (:require [schism.impl.types.nesting-util :as nu]
            [schism.impl.protocols :as proto]
            [schism.impl.vector-clock :as vc]
            [schism.impl.core :as ic]
            [schism.node :as node]
            [clojure.set :as set]
            [clojure.data :refer [diff]]
            #?(:cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [schism.impl.vector-clock :as vc]))
  #?(:clj (:import (clojure.lang IPersistentCollection IPersistentMap IHashEq Associative ILookup Counted Seqable IMapIterable IKVReduce IFn IObj IMeta)
                   (java.io Writer)
                   (java.util Date Collection)
                   (java.lang Object))))

(declare nested-map-conj nested-map-empty nested-map-assoc nested-map-dissoc)

(def not-found-sym (gensym :not-found))

#?(:clj (deftype NestedMap [data vclock birth-dots]
          Counted
          (count [this] (.count ^Counted (.-data this)))

          IPersistentCollection
          (cons [this o] (nested-map-conj this o))
          (empty [this] (nested-map-empty this))
          (equiv [this other] (.equiv ^IPersistentCollection (.-data this) other))

          IPersistentMap
          (assoc [this k v] (nested-map-assoc this k v))
          (assocEx [this k v]
            (when (not= (get this k not-found-sym) not-found-sym)
              (throw (ex-info "Attempt to assocEx on map with key" {:map this
                                                                    :key k
                                                                    :value v})))
            (nested-map-assoc this k v))
          (without [this k] (nested-map-dissoc this k))

          Object
          (equals [this o]
            (.equals (.-data this) o))
          (hashCode [this]
            (.hashCode (.-data this)))
          (toString [this]
            (.toString (.-data this)))

          ILookup
          (valAt [this k]
            (.valAt ^ILookup (.-data this) k))
          (valAt [this k not-found]
            (.valAt ^ILookup (.-data this) k not-found))

          IMapIterable
          (keyIterator [this]
            (.keyIterator ^IMapIterable (.-data this)))
          (valIterator [this]
            (.valIterator ^IMapIterable (.-data this)))

          IKVReduce
          (kvreduce [this f init]
            (.kvreduce ^IKVReduce (.-data this) f init))

          IHashEq
          (hasheq [this]
            (.hasheq ^IHashEq (.-data this)))

          Seqable
          (seq [this]
            (.seq ^Seqable (.-data this)))

          java.util.Map
          (clear [this] (.clear ^java.util.Map (.-data this)))
          (compute [this k f] (.compute ^java.util.Map (.-data this) k f))
          (computeIfAbsent [this k f] (.computeIfAbsent ^java.util.Map (.-data this) k f))
          (computeIfPresent [this k f] (.computeIfPresent ^java.util.Map (.-data this) k f))
          (containsKey [this k] (.containsKey ^java.util.Map (.-data this) k))
          (containsValue [this v] (.containsValue ^java.util.Map (.-data this) v))
          (entrySet [this] (.entrySet ^java.util.Map (.-data this)))
          (get [this k] (.get ^java.util.Map (.-data this) k))
          (getOrDefault [this k not-found] (.getOrDefault ^java.util.Map (.-data this) k not-found))
          (isEmpty [this] (.isEmpty ^java.util.Map (.-data this)))
          (keySet [this] (.keySet ^java.util.Map (.-data this)))
          (merge [this k v f] (.merge ^java.util.Map (.-data this) k v f))
          (put [this k v] (.put ^java.util.Map (.-data this) k v))
          (putAll [this m] (.putAll ^java.util.Map (.-data this) m))
          (putIfAbsent [this k v] (.putIfAbsent ^java.util.Map (.-data this) k v))
          (remove [this k] (.remove ^java.util.Map (.-data this) k))
          (remove [this k v] (.remove ^java.util.Map (.-data this) k v))
          (replace [this k v] (.replace ^java.util.Map (.-data this) k v))
          (replace [this k ov nv] (.replace ^java.util.Map (.-data this) k ov nv))
          (replaceAll [this f] (.replaceAll ^java.util.Map (.-data this) f))
          (size [this] (.size ^java.util.Map (.-data this)))
          (values [this] (.values ^java.util.Map (.-data this)))

          IFn
          (invoke [this k]
            (.invoke ^IFn (.-data this) k))
          (invoke [this k not-found]
            (.invoke ^IFn (.-data this) k not-found))

          IObj
          (withMeta [this meta]
            (NestedMap. (with-meta ^IObj (.-data this)
                          meta)
                        (.-vclock this) (.-birth-dots this)))

          IMeta
          (meta [this]
            (.meta ^IMeta (.-data this))))

   :cljs (deftype NestedMap [data vclock birth-dots]
           ICounted
           (-count [this] (-count (.-data this)))

           IEmptyableCollection
           (-empty [this] (nested-map-empty this))

           ICollection
           (-conj [this o] (nested-map-conj this o))

           IAssociative
           (-contains-key? [this k]
             (-contains-key? (.-data this) k))
           (-assoc [this k v]
             (nested-map-assoc this k v))

           IFind
           (-find [this k]
             (-find (.-data this) k))

           IMap
           (-dissoc [this k]
             (nested-map-dissoc this k))

           IKVReduce
           (-kv-reduce [this f init]
             (-kv-reduce (.-data this) f init))

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
             (-write writer "#schism/nested-map [")
             (-write writer (pr-str (.-data o)))
             (-write writer ", ")
             (-write writer (pr-str (.-vclock o)))
             (-write writer ", ")
             (-write writer (pr-str (.-birth-dots o)))
             (-write writer "]"))

           IHash
           (-hash [this] (-hash (.-data this)))

           IFn
           (-invoke [this o] ((.-data this) o))
           (-invoke [this o not-found] ((.-data this) o not-found))

           ISeqable
           (-seq [this] (-seq (.-data this)))

           Object
           (toString [this] (.toString (.-data this)))

           IMeta
           (-meta [this]
             (-meta (.-data this)))

           IWithMeta
           (-with-meta [this meta]
             (NestedMap. (-with-meta (.-data this)
                                     meta)
                         (.-vclock this)
                         (.-birth-dots this)))))

(defn nested-map-conj
  [^NestedMap nm pair]
  (vc/update-clock now nm
                   (let [[updated updated-dots] (nu/nested-update (.-data nm)
                                                                  (.-birth-dots nm)
                                                                  #(conj % pair)
                                                                  node/*current-node*
                                                                  now)]
                     (NestedMap. updated
                                 (.-vclock nm)
                                 updated-dots))))

(defn nested-map-empty
  [^NestedMap nm]
  (vc/update-clock _ nm
                   (NestedMap. (hash-map)
                               (hash-map)
                               (hash-map))))

(defn nested-map-assoc
  [^NestedMap nm k v]
  (vc/update-clock now nm
                   (let [[updated updated-dots] (nu/nested-update (.-data nm)
                                                                  (.-birth-dots nm)
                                                                  #(assoc % k v)
                                                                  node/*current-node*
                                                                  now)]
                     (NestedMap. updated
                                 (.-vclock nm)
                                 updated-dots))))

(defn nested-map-dissoc
  [^NestedMap nm k]
  (vc/update-clock now nm
                   (let [[updated updated-dots] (nu/nested-update (.-data nm)
                                                                  (.-birth-dots nm)
                                                                  #(dissoc % k)
                                                                  node/*current-node*
                                                                  now)]
                     (NestedMap. updated
                                 (.-vclock nm)
                                 updated-dots))))

(defn- elemental-data
  [^NestedMap nm]

  (let [flat-data (nu/flat (.-data nm))]
    {:vector-clock (.-vclock nm)
     :elements (into []
                     (for [datum flat-data]
                       (let [dot (get-in (.-birth-dots nm) (nu/access-path (key datum)))]
                         {:data {:entry datum
                                 :insert-index (:i dot)}
                          :author-node (:a dot)
                          :record-time (:t dot)})))}))

(extend-type NestedMap
  proto/Vclocked
  (get-clock [this] (.-vclock this))
  (with-clock [this new-clock] (NestedMap. (.-data this)
                                           new-clock
                                           (.-birth-dots this)))
  proto/Convergent
  (synchronize [this ^NestedMap other]
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
          completed-flat-birth-dots (map (fn [{:keys [author-node record-time]
                                               {:keys [insert-index entry] :as data} :data}]
                                           (let [dot {:a author-node :t record-time}]
                                             [(first entry) (if insert-index
                                                              (assoc dot :i insert-index)
                                                              dot)]))
                                         completed-elements)
          completed-vclock (ic/merged-clock completed-elements own-data other-data)]
      (vc/update-clock _ this
                       (NestedMap. (with-meta (nu/project completed-flat-data)
                                     own-meta)
                                   completed-vclock
                                   (nu/project completed-flat-birth-dots))))))

#?(:clj (defmethod print-method NestedMap
          [^NestedMap nm ^Writer writer]
          (.write writer "#schism/nested-map [")
          (.write writer (pr-str (.-data nm)))
          (.write writer ", ")
          (.write writer (pr-str (.-vclock nm)))
          (.write writer ", ")
          (.write writer (pr-str (.-birth-dots nm)))
          (.write writer "]")))

(defn read-edn-map
  [read-object]
  (let [[data vclock birth-dots] read-object]
    (NestedMap. data vclock birth-dots)))

#?(:cljs (cljs.reader/register-tag-parser! 'schism/map read-edn-map))

(defn new-map
  ([] (NestedMap. (hash-map)
                  (hash-map)
                  (hash-map)))
  ([& args] (vc/update-clock now nil
                             (let [[updated updated-dots] (nu/nested-update {}
                                                                            {}
                                                                            (fn [_] (apply hash-map args))
                                                                            node/*current-node*
                                                                            now)]
                               (NestedMap. updated
                                           (hash-map)
                                           updated-dots)))))
