(ns schism.node-test
  (:require [clojure.test :refer :all]
            [schism.node :as node]))

(deftest initialize-node!-test
  (testing "With no arg"
    (node/initialize-node!)
    (is (some? node/*current-node*)))
  (testing "with an arg"
    (node/initialize-node! "a string node id")
    (is (= "a string node id" node/*current-node*))))

(deftest with-node-test
  (testing "with-node clobbers other set values for current-node"
    (node/initialize-node! :with-node-id)
    (is (= :with-node-id node/*current-node*))
    (node/with-node :another-id
      (is (not= :with-node-id node/*current-node*))
      (is (= :another-id node/*current-node*)))))
