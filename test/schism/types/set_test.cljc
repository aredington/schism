(ns schism.types.set-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            #?(:cljs [cljs.reader :as reader])
            [schism.types.set :as sset]
            [schism.node :as node]
            [schism.protocols :as proto])
  #?(:clj (:import schism.types.set.Set)))


(defn clock-ahead [n f]
  #?(:clj (do (Thread/sleep n)
              (f))
     :cljs (js/setTimeout n f)))

(deftest basic-IPC-ops
  (testing "Equiv for sets"
    (is (= (sset/new-set) #{}))
    (is (= (sset/new-set :a) #{:a})))
  (testing "Empty for sets"
    (is (= (empty (sset/new-set :a)) (empty #{:a}))))
  (testing "Count for sets"
    (is (= (count (sset/new-set :a)) (count #{:a}))))
  (testing "Conj for sets"
    (is (= (conj (sset/new-set) :a) (conj #{} :a)))
    (is (= (conj (sset/new-set :a) :a) (conj #{:a} :a)))))

(deftest basic-IPS-ops
  (testing "disjoin"
    (is (= (disj (sset/new-set :a) :a) (disj #{:a} :a))))
  (testing "contains"
    (is (= (contains? (sset/new-set :a) :a) (contains? #{:a} :a))))
  (testing "get"
    (is (= (get (sset/new-set :a) :a) (get #{:a} :a)))))

(deftest converge-test
  (testing "Converge after concurrent additions on another node."
    (node/initialize-node! :converge-test-origin)
    ;; Can only rely on millisecond time scales, so sleep 1 second
    ;; between ops so that there's some non-zero passage of time
    (let [transfer (-> (sset/new-set :a)
                       (conj :b))]
      (clock-ahead 1 #(let [other (node/with-node :converge-test-other-node
                                    (conj transfer :c))
                            result (proto/synchronize transfer other)]
                        (is (= result #{:a :b :c}))))))
  (testing "Disj on another node mirrored locally after converge."
    (node/initialize-node! :converge-test-origin)
    (let [transfer (sset/new-set :a :b :c)]
      (clock-ahead 1 #(let [other (node/with-node :converge-test-other-node
                                    (disj transfer :c))
                            result (proto/synchronize transfer other)]
                        (is (= other #{:a :b}))
                        (is (= result #{:a :b})))))))

(deftest seqable-test
  (testing "Can turn an ORSWOT into a seq"
    (is (= (seq (sset/new-set :a :b :c))
           (seq (hash-set :a :b :c))))))

(deftest ifn-test
  (testing "Can invoke an ORSWOT"
    (is (= ((sset/new-set :a :b :c) :c)
           (#{:a :b :c} :c)))))

(deftest string-test
  (testing "Prints to console readably, even though edn is verbose"
    (is (= (str (sset/new-set :a :b :c))
           (str (hash-set :a :b :c))))))

(deftest serialization-test
  (testing "Round trip serialization generates the same structure."
    (let [^Set origin (-> (sset/new-set :a :b :c)
                          (conj :d)
                          (disj :c))
          ^Set round-tripped (-> origin
                                 pr-str
                                 #?(:clj read-string
                                    :cljs reader/read-string))]
      (is (= (.-data origin) (.-data round-tripped)))
      (is (= (.-vclock origin) (.-vclock round-tripped)))
      (is (= (.-birth-dots origin) (.-birth-dots round-tripped))))))

(deftest hashing-test
  (testing "Hashes to the same value as an equivalent hash-set"
    (is (= (hash (into (sset/new-set) [:a :b :c]))
           (hash (into #{} [:a :b :c]))))))

(deftest meta-test
  (testing "Metadata on ORSWOTs"
    (is (= (meta (with-meta (sset/new-set) {:test :data}))
           {:test :data}))))
