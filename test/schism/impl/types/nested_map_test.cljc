(ns schism.impl.types.nested-map-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            #?(:cljs [cljs.reader :as reader])
            [schism.impl.types.nested-map :as nmap]
            [schism.node :as node]
            [schism.impl.protocols :as proto])
  #?(:clj (:import schism.impl.types.nested_map.NestedMap)))

(deftest basic-IPC-ops
  (testing "Equiv for maps"
    (is (= (nmap/new-map) {}))
    (is (= (nmap/new-map :a true) {:a true})))
  (testing "Empty for maps"
    (is (= (empty (nmap/new-map :a true)) (empty {:a true}))))
  (testing "Count for maps"
    (is (= (count (nmap/new-map :a true)) (count {:a true}))))
  (testing "Conj for maps"
    (is (= (conj (nmap/new-map) [:a true]) (conj {} [:a true])))
    (is (= (conj (nmap/new-map :a true) [:a false]) (conj {:a true} [:a false])))))

(deftest basic-IPS-ops
  (testing "dissoc"
    (is (= (dissoc (nmap/new-map :a true) :a) (dissoc {:a true} :a))))
  (testing "contains"
    (is (= (contains? (nmap/new-map :a true) :a) (contains? {:a true} :a))))
  (testing "get"
    (is (= (get (nmap/new-map :a true) :a) (get {:a true} :a)))))

(deftest converge-test
  (testing "Converge after concurrent additions on another node."
    (node/initialize-node! :converge-test-origin)
    ;; Can only rely on millisecond time scales, so sleep 1 second
    ;; between ops so that there's some non-zero passage of time
    (let [transfer (-> (nmap/new-map :a true)
                       (conj [:b 3]))
          other (node/with-node :converge-test-other-node
                  (conj transfer [:c :d]))
          result (proto/synchronize transfer other)]
      (is (= result {:a true :b 3 :c :d}))
      (is (= {:a true :b 3 :c :d} (.-data result)))
      (doseq [[k v] (.-vclock result)]
        (is (#{:converge-test-origin :converge-test-other-node} k))
        (is (instance? #?(:clj java.util.Date
                          :cljs js/Date) v)))
      (is (= #{:converge-test-origin :converge-test-other-node} (set (keys (.-vclock result)))))
      (is (= (set (keys (.-data result))) (set (keys (.-birth-dots result)))))
      (doseq [[key {node :a time :t} :as entry] (.-birth-dots result)]
        (is (#{:a :b :c} key))
        (is (#{:converge-test-origin :converge-test-other-node} node))
        (is (instance? #?(:clj java.util.Date
                          :cljs js/Date) time)))))
  (testing "Dissoc on another node mirrored locally after converge."
    (node/initialize-node! :converge-test-origin)
    (let [transfer (nmap/new-map :a true :b 3 :c :d)
          other (node/with-node :converge-test-other-node
                  (dissoc transfer :c))
          result (proto/synchronize transfer other)]
      (is (= other {:a true :b 3}))
      (is (= result {:a true :b 3}))
      (is (= (contains? (.-birth-dots other) :c) false))
      (is (= (contains? (.-birth-dots result) :c) false)))))

(deftest seqable-test
  (testing "Can turn an ORMWOT into a seq"
    (is (= (seq (nmap/new-map :a true :b 3 :c :d))
           (seq (hash-map :a true :b 3 :c :d))))))

(deftest ifn-test
  (testing "Can invoke an ORMWOT"
    (is (= ((nmap/new-map :a true :b 3 :c :d) :c)
           ({:a true :b 3 :c :d} :c)))))

(deftest string-test
  (testing "Prints to console readably, even though edn is verbose"
    (is (= (str (nmap/new-map :a true :b 3 :c :d))
           (str (hash-map :a true :b 3 :c :d))))))

(deftest serialization-test
  (testing "Round trip serialization generates the same structure."
    (let [^NestedMap origin (-> (nmap/new-map :a true :b 3 :c :d)
                          (conj [:d :quux])
                          (dissoc :c))
          ^NestedMap round-tripped (-> origin
                                 pr-str
                                 #?(:clj read-string
                                    :cljs reader/read-string))]
      (is (= (.-data origin) (.-data round-tripped)))
      (is (= (.-vclock origin) (.-vclock round-tripped)))
      (is (= (.-birth-dots origin) (.-birth-dots round-tripped))))))

(deftest hashing-test
  (testing "Hashes to the same value as an equivalent hash-map"
    (is (= (hash (into (nmap/new-map) [[:a true] [:b 3] [:c :d]]))
           (hash (into {} [[:a true] [:b 3] [:c :d]]))))))

(deftest meta-test
  (testing "Metadata on ORMWOTs"
    (is (= (meta (with-meta (nmap/new-map) {:test :data}))
           {:test :data}))))

(deftest path-atomicity-test
  (testing "Concurrent modification of a subtree by two nodes converges the atomic values"
    (node/initialize-node! :path-atomicity-origin)
    (let [original (-> (nmap/new-map :a true)
                       (assoc-in [:b :c] true)
                       (#(node/with-node :derivation-node-a
                           (assoc-in % [:b :d] {:e 3 :f "frog"}))))
          derivation-a (node/with-node :derivation-node-a
                         (assoc-in original [:b :d :f] "hog"))
          derivation-b (update-in original [:b :d :e] inc)
          result (proto/synchronize derivation-b derivation-a)]
      (is (= original {:a true
                       :b {:c true
                           :d {:e 3
                               :f "frog"}}}))
      (is (= result {:a true
                     :b {:c true
                         :d {:e 4
                             :f "hog"}}})))))

(deftest vector-conjs-compose
  (testing "Two conjs on different nodes yield both their conjs on convergence and do not overwrite"
    (node/initialize-node! :vector-conjs-origins)
    (let [original (-> (nmap/new-map :a [1])
                       (#(node/with-node :derivation-node-a
                           (assoc-in % [:b] [2]))))
          derivation-a (node/with-node :derivation-node-a
                         (update-in original [:a] conj 2))
          derivation-b (node/with-node :derivation-node-b
                         (update-in original [:a] conj 3))
          result (proto/synchronize derivation-b derivation-a)]
      (is (= original {:a [1]
                       :b [2]}))
      (is (= derivation-a {:a [1 2]
                           :b [2]}))
      (is (= derivation-b {:a [1 3]
                           :b [2]}))
      (is (= result {:a [1 2 3]
                     :b [2]})))))
