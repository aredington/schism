(ns schism.impl.types.nested-vector-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            #?(:cljs [cljs.reader :as reader])
            [schism.impl.types.nested-vector :as nvector]
            [schism.node :as node]
            [schism.impl.protocols :as proto])
  #?(:clj (:import schism.impl.types.nested_vector.NestedVector)))

(deftest basic-IPC-ops
  (testing "Equiv for maps"
    (is (= (nvector/new-vector) []))
    (is (= (nvector/new-vector :a true) [:a true])))
  (testing "Empty for maps"
    (is (= (empty (nvector/new-vector :a true)) (empty [:a true]))))
  (testing "Count for maps"
    (is (= (count (nvector/new-vector :a true)) (count [:a true]))))
  (testing "Conj for maps"
    (is (= (conj (nvector/new-vector) [:a true]) (conj [] [:a true])))
    (is (= (conj (nvector/new-vector :a true) [:a false]) (conj [:a true] [:a false])))))

(deftest basic-vector-ops
  (testing "peek"
    (is (= (peek (nvector/new-vector :a true)) (peek [:a true]))))
  (testing "pop"
    (is (= (pop (nvector/new-vector :a true)) (pop [:a true])))))

(deftest converge-test
  (testing "Converge after concurrent additions on another node."
    (node/initialize-node! :converge-test-origin)
    ;; Can only rely on millisecond time scales, so sleep 1 second
    ;; between ops so that there's some non-zero passage of time
    (let [transfer (-> (nvector/new-vector :a true)
                       (conj 3))
          other (node/with-node :converge-test-other-node
                  (conj transfer :d))
          result (proto/synchronize transfer other)]
      (is (= result [:a true 3 :d]))
      (is (= [:a true 3 :d] (.-data result)))
      (doseq [[k v] (.-vclock result)]
        (is (#{:converge-test-origin :converge-test-other-node} k))
        (is (instance? #?(:clj java.util.Date
                          :cljs js/Date) v)))
      (is (= #{:converge-test-origin :converge-test-other-node} (set (keys (.-vclock result)))))
      (is (= (count (.-data result)) (count (.-insertions result))))
      (doseq [{node :a time :t} (.-insertions result)]
        (is (#{:converge-test-origin :converge-test-other-node} node))
        (is (instance? #?(:clj java.util.Date
                          :cljs js/Date) time)))))
  (testing "Pop on another node mirrored locally after converge."
    (node/initialize-node! :converge-test-origin)
    (let [transfer (nvector/new-vector :a true :b 3 :c :d)
          other (node/with-node :converge-test-other-node
                  (pop transfer))
          result (proto/synchronize transfer other)]
      (is (= other [:a true :b 3 :c]))
      (is (= result [:a true :b 3 :c]))
      (is (= (count (.-insertions other)) 5))
      (is (= (count (.-insertions result)) 5)))))

(deftest seqable-test
  (testing "Can turn an nested vector into a seq"
    (is (= (seq (nvector/new-vector :a true :b 3 :c :d))
           (seq (vector :a true :b 3 :c :d))))))

(deftest ifn-test
  (testing "Can invoke a vector"
    (is (= ((nvector/new-vector :a true :b 3 :c :d) 0)
           ([:a true :b 3 :c :d] 0)
           :a))))

(deftest string-test
  (testing "Prints to console readably, even though edn is verbose"
    (is (= (str (nvector/new-vector :a true :b 3 :c :d))
           (str (vector :a true :b 3 :c :d))))))

(deftest serialization-test
  (testing "Round trip serialization generates the same structure."
    (let [^NestedVector origin (-> (nvector/new-vector :a true :b 3 :c :d)
                                   (conj [:d :quux])
                                   pop)
          ^NestedVector round-tripped (-> origin
                                          pr-str
                                          #?(:clj read-string
                                             :cljs reader/read-string))]
      (is (= (.-data origin) (.-data round-tripped)))
      (is (= (.-vclock origin) (.-vclock round-tripped)))
      (is (= (.-insertions origin) (.-insertions round-tripped))))))

(deftest hashing-test
  (testing "Hashes to the same value as an equivalent vector"
    (is (= (hash (into (nvector/new-vector) [[:a true] [:b 3] [:c :d]]))
           (hash (into [] [[:a true] [:b 3] [:c :d]]))))))

(deftest meta-test
  (testing "Metadata on nested vectors"
    (is (= (meta (with-meta (nvector/new-vector) {:test :data}))
           {:test :data}))))

(deftest path-atomicity-test
  (testing "Concurrent modification of a subtree by two nodes converges the atomic values"
    (node/initialize-node! :path-atomicity-origin)
    (let [original (-> (nvector/new-vector :a true)
                       (assoc-in [2 :b] true)
                       (#(node/with-node :derivation-node-a
                           (assoc-in % [3 :c] {:e 3 :f "frog"}))))
          derivation-a (node/with-node :derivation-node-a
                         (assoc-in original [3 :c :f] "hog"))
          derivation-b (update-in original [3 :c :e] inc)
          result (proto/synchronize derivation-b derivation-a)]
      (is (= original [:a true {:b true} {:c {:e 3 :f "frog"}}]))
      (is (= result [:a true {:b true} {:c {:e 4 :f "hog"}}])))))

(deftest vector-conjs-compose
  (testing "Two conjs on different nodes yield both their conjs on convergence and do not overwrite"
    (node/initialize-node! :vector-conjs-origins)
    (let [original (-> (nvector/new-vector :a [1])
                       (#(node/with-node :derivation-node-a
                           (assoc-in % [2] [2]))))
          derivation-a (node/with-node :derivation-node-a
                         (update-in original [1] conj 2))
          derivation-b (node/with-node :derivation-node-b
                         (update-in original [1] conj 3))
          result (proto/synchronize derivation-b derivation-a)]
      (is (= original [:a [1] [2]]))
      (is (= derivation-a [:a [1 2] [2]]))
      (is (= derivation-b [:a [1 3] [2]]))
      (is (= result [:a [1 2 3] [2]])))))

(deftest interesting-vector-inits
  (testing "Empty vector stacked 2 deep"
    (node/initialize-node! :interesting-vector-inits)
    (let [v (nvector/new-vector [[]])]
      (is (= [[[]]] (.-data v)))
      (is (= :interesting-vector-inits (get-in (.-insertions v) [0 0 :a])))
      (is (= -1 (get-in (.-insertions v) [0 0 :i])))
      (is (instance? #?(:clj java.util.Date
                        :cljs js/Date) (get-in (.-insertions v) [0 0 :t]))))))
