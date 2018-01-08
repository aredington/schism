(ns schism.types.set-test
  (:require [clojure.test :refer :all]
            [schism.types.set :as sset]
            [schism.node :as node]
            [schism.protocols :as proto]))


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
  (testing "Converge after concurrent additions on own and another node."
    (node/initialize-node! :converge-test-origin)
    (let [transfer (-> (sset/new-set :a)
                       (conj :b))
          other (node/with-node :converge-test-other-node
                  (conj transfer :c))
          further (conj transfer :d)
          result (proto/synchronize further other)]
      (is (= result #{:a :b :c :d})))))
