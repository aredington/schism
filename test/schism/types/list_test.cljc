(ns schism.types.list-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            #?(:cljs [cljs.reader :as reader])
            [schism.types.list :as slist]
            [schism.node :as node]
            [schism.impl.protocols :as proto])
  #?(:clj (:import schism.types.list.ConvergentList)))


(defn clock-ahead [n f]
  #?(:clj (do (Thread/sleep n)
              (f))
     :cljs (js/setTimeout n f)))

(deftest basic-IPC-ops
  (testing "Equiv for lists"
    (is (= (slist/new-list) '()))
    (is (= (slist/new-list :a true) '(:a true))))
  (testing "Empty for lists"
    (is (= (empty (slist/new-list :a true)) (empty '(:a true)))))
  (testing "Count for lists"
    (is (= (count (slist/new-list :a true)) (count '(:a true)))))
  (testing "Conj for lists"
    (is (= (conj (slist/new-list) [:a true]) (conj '() [:a true])))
    (is (= (conj (slist/new-list :a true) [:a false]) (conj '(:a true) [:a false])))))

(deftest basic-seq-ops
  (testing "conj"
    (is (= (conj (slist/new-list :a true) :a) (conj '(:a true) :a))))
  (testing "rest"
    (is (= (rest (slist/new-list :a true)) (rest '(:a true))))))

(deftest converge-test
  (testing "Converge after concurrent additions on another node."
    (node/initialize-node! :converge-test-origin)
    ;; Can only rely on millisecond time scales, so sleep 1 second
    ;; between ops so that there's some non-zero passage of time
    (let [transfer (-> (slist/new-list true :a)
                       (conj [:b 3]))]
      (clock-ahead 1 #(let [other (node/with-node :converge-test-other-node
                                    (conj transfer [:c :d]))
                            result (proto/synchronize transfer other)]
                        (is (= result '([:c :d] [:b 3] true :a)))))))
  (testing "Rest on another node mirrored locally after converge."
    (node/initialize-node! :converge-test-origin)
    (let [transfer (slist/new-list :a true :b 3 :c :d)]
      (clock-ahead 1 #(let [other (node/with-node :converge-test-other-node
                                    (rest transfer))
                            result (proto/synchronize transfer other)]
                        (is (= other '(true :b 3 :c :d)))
                        (is (= result '(true :b 3 :c :d))))))))

(deftest seqable-test
  (testing "Can turn a CLIST into a seq"
    (is (= (seq (slist/new-list :a true :b 3 :c :d))
           (seq (list :a true :b 3 :c :d))))))

(deftest string-test
  (testing "Prints to console readably, even though edn is verbose"
    (is (= (str (slist/new-list :a true :b 3 :c :d))
           (str (list :a true :b 3 :c :d))))))

(deftest serialization-test
  (testing "Round trip serialization generates the same structure."
    (let [^ConvergentList origin (-> (slist/new-list :a true :b 3 :c :d)
                                     (conj [:d :quux])
                                     rest)
          ^ConvergentList round-tripped (-> origin
                                            pr-str
                                            #?(:clj read-string
                                               :cljs reader/read-string))]
      (is (= (.-data origin) (.-data round-tripped)))
      (is (= (.-vclock origin) (.-vclock round-tripped)))
      (is (= (.-insertions origin) (.-insertions round-tripped))))))

(deftest hashing-test
  (testing "Hashes to the same value as an equivalent list"
    (is (= (hash (into (slist/new-list) [[:a true] [:b 3] [:c :d]]))
           (hash (into '() [[:a true] [:b 3] [:c :d]]))))))

(deftest meta-test
  (testing "Metadata on CLISTs"
    (is (= (meta (with-meta (slist/new-list) {:test :data}))
           {:test :data}))))
