(ns schism.impl.types.map
  "Definition and support for Schism's Convergent Map type, an ORMWOT
  implemented on top of Clojure's persistent maps and a Schism Vector
  Clock."
  (:require [schism.impl.protocols :as proto]
            [schism.impl.vector-clock :as vc]
            [schism.impl.core :as ic]
            [schism.node :as node]
            [clojure.set :as set]
            #?(:cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [schism.impl.vector-clock :as vc]))
  #?(:clj (:import (clojure.lang IPersistentCollection IPersistentMap IHashEq Associative ILookup Counted Seqable IMapIterable IKVReduce IFn IObj IMeta)
                   (java.io Writer)
                   (java.util Date Collection)
                   (java.lang Object))))

;; A CLJS and CLJ imeplementation of ORMWOT (Observed-Removed Map without Tombstones)

;; Each map maintains its own vector clock, and birth dots for each
;; map entry by key. A birth dot consists of the node adding the
;; entry, and the date at which it was added. The vector clock
;; determines if an entry has been removed: a compared map absent an
;; entry, but with a newer vector clock than the own birthdot on the
;; entry indicates that it was removed; a compared map absent an entry
;; with an older vector clock indicates that the birthdot was never
;; seen and should be in the merged map. Birthdots also arbitrate
;; entries: for each entry the last writer wins.

(declare ormwot-conj ormwot-empty ormwot-assoc ormwot-dissoc)

(def not-found-sym (gensym :not-found))

#?(:clj (deftype Map [data vclock birth-dots]
          Counted
          (count [this] (.count ^Counted (.data this)))

          IPersistentCollection
          (cons [this o] (ormwot-conj this o))
          (empty [this] (ormwot-empty this))
          (equiv [this other] (.equiv ^IPersistentCollection (.data this) other))

          IPersistentMap
          (assoc [this k v] (ormwot-assoc this k v))
          (assocEx [this k v]
            (when (not= (get this k not-found-sym) not-found-sym)
              (throw (ex-info "Attempt to assocEx on map with key" {:map this
                                                                    :key k
                                                                    :value v})))
            (ormwot-assoc this k v))
          (without [this k] (ormwot-dissoc this k))

          Object
          (equals [this o]
            (.equals (.data this) o))
          (hashCode [this]
            (.hashCode (.data this)))
          (toString [this]
            (.toString (.data this)))

          ILookup
          (valAt [this k]
            (.valAt ^ILookup (.data this) k))
          (valAt [this k not-found]
            (.valAt ^ILookup (.data this) k not-found))

          IMapIterable
          (keyIterator [this]
            (.keyIterator ^IMapIterable (.data this)))
          (valIterator [this]
            (.valIterator ^IMapIterable (.data this)))

          IKVReduce
          (kvreduce [this f init]
            (.kvreduce ^IKVReduce (.data this) f init))

          IHashEq
          (hasheq [this]
            (.hasheq ^IHashEq (.data this)))

          Seqable
          (seq [this]
            (.seq ^Seqable (.data this)))

          java.util.Map
          (clear [this] (.clear ^java.util.Map (.data this)))
          (compute [this k f] (.compute ^java.util.Map (.data this) k f))
          (computeIfAbsent [this k f] (.computeIfAbsent ^java.util.Map (.data this) k f))
          (computeIfPresent [this k f] (.computeIfPresent ^java.util.Map (.data this) k f))
          (containsKey [this k] (.containsKey ^java.util.Map (.data this) k))
          (containsValue [this v] (.containsValue ^java.util.Map (.data this) v))
          (entrySet [this] (.entrySet ^java.util.Map (.data this)))
          (get [this k] (.get ^java.util.Map (.data this) k))
          (getOrDefault [this k not-found] (.getOrDefault ^java.util.Map (.data this) k not-found))
          (isEmpty [this] (.isEmpty ^java.util.Map (.data this)))
          (keySet [this] (.keySet ^java.util.Map (.data this)))
          (merge [this k v f] (.merge ^java.util.Map (.data this) k v f))
          (put [this k v] (.put ^java.util.Map (.data this) k v))
          (putAll [this m] (.putAll ^java.util.Map (.data this) m))
          (putIfAbsent [this k v] (.putIfAbsent ^java.util.Map (.data this) k v))
          (remove [this k] (.remove ^java.util.Map (.data this) k))
          (remove [this k v] (.remove ^java.util.Map (.data this) k v))
          (replace [this k v] (.replace ^java.util.Map (.data this) k v))
          (replace [this k ov nv] (.replace ^java.util.Map (.data this) k ov nv))
          (replaceAll [this f] (.replaceAll ^java.util.Map (.data this) f))
          (size [this] (.size ^java.util.Map (.data this)))
          (values [this] (.values ^java.util.Map (.data this)))

          IFn
          (invoke [this k]
            (.invoke ^IFn (.data this) k))
          (invoke [this k not-found]
            (.invoke ^IFn (.data this) k not-found))

          IObj
          (withMeta [this meta]
            (Map. (with-meta ^IObj (.data this)
                    meta)
                  (.vclock this) (.birth-dots this)))

          IMeta
          (meta [this]
            (.meta ^IMeta (.data this))))
   :cljs (deftype Map [data vclock birth-dots]
           ICounted
           (-count [this] (-count (.-data this)))

           IEmptyableCollection
           (-empty [this] (ormwot-empty this))

           ICollection
           (-conj [this o] (ormwot-conj this o))

           IAssociative
           (-contains-key? [this k]
             (-contains-key? (.-data this) k))
           (-assoc [this k v]
             (ormwot-assoc this k v))

           IFind
           (-find [this k]
             (-find (.-data this) k))

           IMap
           (-dissoc [this k]
             (ormwot-dissoc this k))

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
             (-write writer "#schism/map [")
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
             (Map. (-with-meta (.-data this)
                               meta)
                   (.-vclock this)
                   (.-birth-dots this)))))

(defn ormwot-conj
  [^Map ormwot pair]
  (vc/update-clock now ormwot
                   (Map. (conj (.-data ormwot) pair)
                         (.-vclock ormwot)
                         (assoc (.-birth-dots ormwot) (first pair) [node/*current-node* now]))))

(defn ormwot-empty
  [^Map ormwot]
  (vc/update-clock _ ormwot
                   (Map. (hash-map)
                         (hash-map)
                         (hash-map))))

(defn ormwot-assoc
  [^Map ormwot k v]
  (vc/update-clock now ormwot
                   (Map. (assoc (.-data ormwot) k v)
                         (.-vclock ormwot)
                         (assoc (.-birth-dots ormwot) k [node/*current-node* now]))))

(defn ormwot-dissoc
  [^Map ormwot k]
  (vc/update-clock _ ormwot
                   (Map. (dissoc (.-data ormwot) k)
                         (.-vclock ormwot)
                         (dissoc (.-birth-dots ormwot) k))))

(defn- elemental-data
  [^Map m]
  {:vector-clock (.vclock m)
   :elements (into []
                   (for [datum (.-data m)]
                     (let [dot (get (.-birth-dots m) (key datum))]
                       {:data datum
                        :author-node (first dot)
                        :record-time (last dot)})))})

(extend-type Map
  proto/Vclocked
  (get-clock [this] (.-vclock this))
  (with-clock [this new-clock] (Map. (.-data this)
                                     new-clock
                                     (.-birth-dots this)))

  proto/Convergent
  (synchronize [this ^Map other]
    (let [own-meta (-> this .-data meta)
          own-data (elemental-data this)
          other-data (elemental-data other)
          retain (filter (ic/common-elements own-data other-data)
                         (:elements own-data))
          completed-elements (concat (apply ic/retain-elements
                                            (ic/distinct-data own-data other-data))
                                     retain)
          completed-data (into {} (map :data completed-elements))
          completed-birth-dots (->> completed-elements
                                    (map (fn [{:keys [data author-node record-time]}]
                                           [(key data) [author-node record-time]]))
                                    (into {}))
          completed-vclock (ic/merged-clock completed-elements own-data other-data)]
      (vc/update-clock _ this
                       (Map. (with-meta completed-data
                               own-meta)
                             completed-vclock
                             completed-birth-dots)))))

#?(:clj (defmethod print-method Map
          [^Map m ^Writer writer]
          (.write writer "#schism/map [")
          (.write writer (pr-str (.data m)))
          (.write writer ", ")
          (.write writer (pr-str (.vclock m)))
          (.write writer ", ")
          (.write writer (pr-str (.birth-dots m)))
          (.write writer "]")))

(defn read-edn-map
  [read-object]
  (let [[data vclock birth-dots] read-object]
    (Map. data vclock birth-dots)))

#?(:cljs (cljs.reader/register-tag-parser! 'schism/map read-edn-map))

(defn new-map
  ([] (Map. (hash-map)
            (hash-map)
            (hash-map)))
  ([& args] (vc/update-clock now nil
                             (Map. (apply hash-map args)
                                   (hash-map)
                                   (apply hash-map
                                          (mapcat (fn [[k _]]
                                                    [k [node/*current-node* now]])
                                                  (partition 2 args)))))))
