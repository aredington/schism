(ns schism.types.vector-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            #?(:cljs [cljs.reader :as reader])
            [schism.types.vector :as svector]
            [schism.node :as node]
            [schism.protocols :as proto])
  #?(:clj (:import schism.types.vector.ConvergentVector)))


(defn clock-ahead [n f]
  #?(:clj (do (Thread/sleep n)
              (f))
     :cljs (js/setTimeout n f)))

(deftest basic-IPC-ops
  (testing "Equiv for vectors"
    (is (= (svector/new-vector) []))
    (is (= (svector/new-vector :a true) [:a true])))
  (testing "Empty for vectors"
    (is (= (empty (svector/new-vector :a true)) (empty [:a true]))))
  (testing "Count for vectors"
    (is (= (count (svector/new-vector :a true)) (count [:a true]))))
  (testing "Conj for vectors"
    (is (= (conj (svector/new-vector) [:a true]) (conj [] [:a true])))
    (is (= (conj (svector/new-vector :a true) [:a false]) (conj [:a true] [:a false])))))

(deftest basic-vector-ops
  (testing "conj"
    (is (= (conj (svector/new-vector :a true) :a) (conj [:a true] :a))))
  (testing "peek"
    (is (= (peek (svector/new-vector :a true)) (peek [:a true]))))
  (testing "pop"
    (is (= (pop (svector/new-vector :a true)) (pop [:a true])))))

(deftest converge-test
  (testing "Converge after concurrent additions on another node."
    (node/initialize-node! :converge-test-origin)
    ;; Can only rely on millisecond time scales, so sleep 1 second
    ;; between ops so that there's some non-zero passage of time
    (let [transfer (-> (svector/new-vector true :a)
                       (conj [:b 3]))]
      (clock-ahead 1 #(let [other (node/with-node :converge-test-other-node
                                    (conj transfer [:c :d]))
                            result (proto/synchronize transfer other)]
                        (is (= other [true :a [:b 3] [:c :d]]))
                        (is (= result [true :a [:b 3] [:c :d]]))))))
  (testing "Pop on another node mirrored locally after converge."
    (node/initialize-node! :converge-test-origin)
    (let [transfer (svector/new-vector :a true :b 3 :c :d)]
      (clock-ahead 1 #(let [other (node/with-node :converge-test-other-node
                                    (pop transfer))
                            result (proto/synchronize transfer other)]
                        (is (= other [:a true :b 3 :c]))
                        (is (= result [:a true :b 3 :c])))))))

(deftest seqable-test
  (testing "Can turn a CVECTOR into a seq"
    (is (= (seq (svector/new-vector :a true :b 3 :c :d))
           (seq (vector :a true :b 3 :c :d))))))

(deftest string-test
  (testing "Prints to console readably, even though edn is verbose"
    (is (= (str (svector/new-vector :a true :b 3 :c :d))
           (str (vector :a true :b 3 :c :d))))))

(deftest serialization-test
  (testing "Round trip serialization generates the same structure."
    (let [^ConvergentVector origin (-> (svector/new-vector :a true :b 3 :c :d)
                                       (conj [:d :quux])
                                       pop)
          ^ConvergentVector round-tripped (-> origin
                                              pr-str
                                              #?(:clj read-string
                                                 :cljs reader/read-string))]
      (is (= (.-data origin) (.-data round-tripped)))
      (is (= (.-vclock origin) (.-vclock round-tripped)))
      (is (= (.-insertions origin) (.-insertions round-tripped))))))

(deftest hashing-test
  (testing "Hashes to the same value as an equivalent vector"
    (is (= (hash (into (svector/new-vector) [[:a true] [:b 3] [:c :d]]))
           (hash (into [] [[:a true] [:b 3] [:c :d]]))))))

(deftest meta-test
  (testing "Metadata on CVECTORs"
    (is (= (meta (with-meta (svector/new-vector) {:test :data}))
           {:test :data}))))
