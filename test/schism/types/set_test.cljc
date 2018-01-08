(ns schism.types.set-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            #?(:cljs [cljs.reader :as reader])
            [schism.types.set :as sset]
            [schism.node :as node]
            [schism.protocols :as proto]))


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

(deftest serialization-test
  (testing "Round trip serialization generates the same structure."
    (let [origin (-> (sset/new-set :a :b :c)
                     (conj :d)
                     (disj :c))
          round-tripped (-> origin
                            pr-str
                            #?(:clj read-string
                               :cljs reader/read-string))]
      (is (= (.-data origin) (.-data round-tripped)))
      (is (= (.-vclock origin) (.-vclock round-tripped)))
      (is (= (.-birth-dots origin) (.-birth-dots round-tripped))))))
