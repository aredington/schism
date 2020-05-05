(ns schism.core-test
  (:require [schism.core :as schism :include-macros true]
            #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            [clojure.test.check.clojure-test #?(:clj :refer
                                                :cljs :refer-macros) [defspec]]))

#?(:cljs
   (deftest collections-with-NaN
     (is (not= (hash-map js/NaN [])
               (hash-map js/NaN []))
         "Because NaN cannot be compared for equality, two maps with NaN cannot be equal.")
     (is (not= (list js/NaN)
               (list js/NaN))
         "Because NaN cannot be compared for equality, two lists with NaN cannot be equal.")
     (is (not= (hash-set js/NaN)
               (hash-set js/NaN))
         "Because NaN cannot be compared for equality, two hash-sets with NaN cannot be equal.")
     (is (not= (vector js/NaN)
               (vector js/NaN))
         "Because NaN cannot be compared for equality, two vectors with NaN cannot be equal.")))

(def collection-any
  "CLJS any will sometimes return NaN, but property tests using
  equality of collections cannot pass with NaN as an element (see
  above), so explicitly filter out NaN"
  #?(:cljs (gen/such-that #(not (js/Number.isNaN %)) gen/any)
     :clj gen/any))



(defspec convergent-set-is-equivalent-to-hash-set
  50
  (prop/for-all [v (gen/vector collection-any)]
                (= (apply schism/convergent-set v)
                   (apply hash-set v))))

(defspec convergent-map-is-equivalent-to-hash-map
  50
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq) (gen/map collection-any collection-any))]
                (= (apply schism/convergent-map entries)
                   (apply hash-map entries))))

(defspec nested-map-is-equivalent-to-hash-map
  50
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq) (gen/map collection-any collection-any))]
                (= (apply schism/nested-map entries)
                   (apply hash-map entries))))

(defspec convergent-vector-is-equivalent-to-vector
  50
  (prop/for-all [v (gen/vector collection-any)]
                (= (apply schism/convergent-vector v)
                   (apply vector v))))

(defspec nested-vector-is-equivalent-to-vector
  50
  (prop/for-all [v (gen/vector collection-any)]
                (= (apply schism/nested-vector v)
                   (apply vector v))))

(defspec convergent-list-is-equivalent-to-list
  50
  (prop/for-all [v (gen/list collection-any)]
                (= (apply schism/convergent-list v)
                   (apply list v))))

(defspec conj-equivalence-for-sets
  50
  (prop/for-all [v (gen/vector collection-any)
                 e collection-any]
                (= (conj (apply schism/convergent-set v) e)
                   (conj (apply hash-set v) e))))

(defspec conj-equivalence-for-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq) (gen/map collection-any collection-any))
                 e (gen/tuple collection-any collection-any)]
                (= (conj (apply schism/convergent-map entries) e)
                   (conj (apply hash-map entries) e))))

(defspec conj-equivalence-for-nested-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq) (gen/map collection-any collection-any))
                 e (gen/tuple collection-any collection-any)]
                (= (conj (apply schism/nested-map entries) e)
                   (conj (apply hash-map entries) e))))


(defspec conj-equivalence-for-vectors
  30
  (prop/for-all [v (gen/vector collection-any)
                 e collection-any]
                (= (conj (apply schism/convergent-vector v) e)
                   (conj (apply vector v) e))))

(defspec conj-equivalence-for-nested-vectors
  30
  (prop/for-all [v (gen/vector collection-any)
                 e collection-any]
                (= (conj (apply schism/nested-vector v) e)
                   (conj (apply vector v) e))))

(defspec conj-equivalence-for-lists
  30
  (prop/for-all [v (gen/vector collection-any)
                 e collection-any]
                (= (conj (apply schism/convergent-list v) e)
                   (conj (apply list v) e))))

(defspec rest-equivalence-for-lists
  30
  (prop/for-all [v (gen/vector collection-any)]
                (= (rest (apply schism/convergent-list v))
                   (rest (apply list v)))))

(defspec assoc-equivalence-for-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq) (gen/map collection-any collection-any))
                 k collection-any
                 v collection-any]
                (= (assoc (apply schism/convergent-map entries) k v)
                   (assoc (apply hash-map entries) k v))))

(defspec assoc-equivalence-for-nested-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq) (gen/map collection-any collection-any))
                 k collection-any
                 v collection-any]
                (= (assoc (apply schism/nested-map entries) k v)
                   (assoc (apply hash-map entries) k v))))

(defspec assoc-equivalence-for-vectors
  30
  (prop/for-all [v (gen/such-that #(< 0 (count %))
                                  (gen/vector collection-any))
                 e collection-any]
                (gen/let [index (gen/choose 0 (dec (count v)))]
                 (= (assoc (apply schism/convergent-vector v) index e)
                    (assoc (apply vector v) index e)))))

(defspec assoc-equivalence-for-nested-vectors
  30
  (prop/for-all [v (gen/such-that #(< 0 (count %))
                                  (gen/vector collection-any))
                 e collection-any]
                (gen/let [index (gen/choose 0 (dec (count v)))]
                 (= (assoc (apply schism/nested-vector v) index e)
                    (assoc (apply vector v) index e)))))

(defspec pop-equivalence-for-vectors
  30
  (prop/for-all [v (gen/such-that #(< 0 (count %))
                                  (gen/vector collection-any))]
                (= (pop (apply schism/convergent-vector v))
                   (pop (apply vector v)))))

(defspec pop-equivalence-for-nested-vectors
  30
  (prop/for-all [v (gen/such-that #(< 0 (count %))
                                  (gen/vector collection-any))]
                (= (pop (apply schism/nested-vector v))
                   (pop (apply vector v)))))

(defspec dissoc-present-key-for-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq)
                                   (gen/map collection-any collection-any {:min-elements 1}))]
                (gen/let [key (gen/elements (keys (apply hash-map entries)))]
                  (= (dissoc (apply schism/convergent-map entries) key)
                     (dissoc (apply hash-map entries) key)))))

(defspec dissoc-present-key-for-nested-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq)
                                   (gen/map collection-any collection-any {:min-elements 1}))]
                (gen/let [key (gen/elements (keys (apply hash-map entries)))]
                  (= (dissoc (apply schism/nested-map entries) key)
                     (dissoc (apply hash-map entries) key)))))

(defspec dissoc-random-value-for-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq)
                                   (gen/map collection-any collection-any {:min-elements 1}))
                 key gen/any]
                (= (dissoc (apply schism/convergent-map entries) key)
                   (dissoc (apply hash-map entries) key))))

(defspec dissoc-random-value-for-nested-maps
  30
  (prop/for-all [entries (gen/fmap (comp (partial mapcat identity) seq)
                                   (gen/map collection-any collection-any {:min-elements 1}))
                 key gen/any]
                (= (dissoc (apply schism/nested-map entries) key)
                   (dissoc (apply hash-map entries) key))))


(defspec disj-included-element-for-sets
  5
  (prop/for-all [s (gen/set collection-any {:min-elements 1})]
                (gen/let [e (gen/elements (apply hash-set s))]
                  (= (disj (apply schism/convergent-set s) e)
                     (disj (apply hash-set s) e)))))

(defspec disj-random-element-for-sets
  5
  (prop/for-all [s (gen/set collection-any {:min-elements 1})
                 e collection-any]
                (= (disj (apply schism/convergent-set s) e)
                   (disj (apply hash-set s) e))))

(defspec converge-after-ops-equivalent-for-sets
  50
  (prop/for-all [s (gen/set collection-any {:min-elements 3})
                 ops (gen/let [operants (gen/list collection-any)
                               operands (gen/vector (gen/elements [conj disj]) (count operants))]
                       (map vector operands operants))]
                (let [basis-cset (schism/with-node :start
                                   (apply schism/convergent-set s))
                      vanilla-result (reduce (fn [memo [f operant]]
                                               (f memo operant))
                                             s
                                             ops)
                      schism-result (schism/with-node :end
                                      (reduce (fn [memo [f operant]]
                                                (f memo operant))
                                              basis-cset
                                              ops))
                      converge-result (schism/with-node :start
                                        (schism/converge basis-cset schism-result))]
                  (= vanilla-result schism-result converge-result))))
