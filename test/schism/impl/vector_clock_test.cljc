(ns schism.impl.vector-clock-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            [schism.impl.vector-clock :as vc]
            [schism.impl.protocols :as proto]
            [schism.node :as node]))


(defrecord SimpleClocked [last-time vclock]
  proto/Vclocked
  (get-clock [_] vclock)
  (with-clock [this new-clock]
    (assoc this :vclock new-clock)))

(deftest update-clock-test
  (node/initialize-node! :clock-test-node)
  (let [test (->SimpleClocked nil {})
        updated (vc/update-clock time
                                 (assoc test :last-time time))
        time (:last-time updated)]
    (is (= {:clock-test-node time} (proto/get-clock updated)))
    (is #?(:clj (instance? java.util.Date time)
           :cljs true))))
