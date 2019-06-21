(ns schism.impl.types.map-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            #?(:cljs [cljs.reader :as reader])
            [schism.impl.types.map :as smap]
            [schism.node :as node]
            [schism.impl.protocols :as proto])
  #?(:clj (:import schism.impl.types.map.Map)))

(deftest basic-IPC-ops
  (testing "Equiv for maps"
    (is (= (smap/new-map) {}))
    (is (= (smap/new-map :a true) {:a true})))
  (testing "Empty for maps"
    (is (= (empty (smap/new-map :a true)) (empty {:a true}))))
  (testing "Count for maps"
    (is (= (count (smap/new-map :a true)) (count {:a true}))))
  (testing "Conj for maps"
    (is (= (conj (smap/new-map) [:a true]) (conj {} [:a true])))
    (is (= (conj (smap/new-map :a true) [:a false]) (conj {:a true} [:a false])))))

(deftest basic-IPS-ops
  (testing "dissoc"
    (is (= (dissoc (smap/new-map :a true) :a) (dissoc {:a true} :a))))
  (testing "contains"
    (is (= (contains? (smap/new-map :a true) :a) (contains? {:a true} :a))))
  (testing "get"
    (is (= (get (smap/new-map :a true) :a) (get {:a true} :a)))))

(deftest converge-test
  (testing "Converge after concurrent additions on another node."
    (node/initialize-node! :converge-test-origin)
    ;; Can only rely on millisecond time scales, so sleep 1 second
    ;; between ops so that there's some non-zero passage of time
    (let [transfer (-> (smap/new-map :a true)
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
      (doseq [[key [node time]] (.-birth-dots result)]
        (is (#{:a :b :c} key))
        (is (#{:converge-test-origin :converge-test-other-node} node))
        (is (instance? #?(:clj java.util.Date
                          :cljs js/Date) time)))))
  (testing "Dissoc on another node mirrored locally after converge."
    (node/initialize-node! :converge-test-origin)
    (let [transfer (smap/new-map :a true :b 3 :c :d)
          other (node/with-node :converge-test-other-node
                  (dissoc transfer :c))
          result (proto/synchronize transfer other)]
      (is (= other {:a true :b 3}))
      (is (= result {:a true :b 3})))))

(deftest seqable-test
  (testing "Can turn an ORMWOT into a seq"
    (is (= (seq (smap/new-map :a true :b 3 :c :d))
           (seq (hash-map :a true :b 3 :c :d))))))

(deftest ifn-test
  (testing "Can invoke an ORMWOT"
    (is (= ((smap/new-map :a true :b 3 :c :d) :c)
           ({:a true :b 3 :c :d} :c)))))

(deftest string-test
  (testing "Prints to console readably, even though edn is verbose"
    (is (= (str (smap/new-map :a true :b 3 :c :d))
           (str (hash-map :a true :b 3 :c :d))))))

(deftest serialization-test
  (testing "Round trip serialization generates the same structure."
    (let [^Map origin (-> (smap/new-map :a true :b 3 :c :d)
                          (conj [:d :quux])
                          (dissoc :c))
          ^Map round-tripped (-> origin
                                 pr-str
                                 #?(:clj read-string
                                    :cljs reader/read-string))]
      (is (= (.-data origin) (.-data round-tripped)))
      (is (= (.-vclock origin) (.-vclock round-tripped)))
      (is (= (.-birth-dots origin) (.-birth-dots round-tripped))))))

(deftest hashing-test
  (testing "Hashes to the same value as an equivalent hash-map"
    (is (= (hash (into (smap/new-map) [[:a true] [:b 3] [:c :d]]))
           (hash (into {} [[:a true] [:b 3] [:c :d]]))))))

(deftest meta-test
  (testing "Metadata on ORMWOTs"
    (is (= (meta (with-meta (smap/new-map) {:test :data}))
           {:test :data}))))
