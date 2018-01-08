(ns schism.node
  (:import (java.util UUID)))

(def ^:dynamic *current-node* nil)

(defn initialize-node!
  "Initialize the current node id special var to the passed in value,
  or a new random UUID. While any serializable value suffices as the
  current node, it should be unique within the cluster; a repeated
  node value may result in incorrect behaviors under convergence."
  ([] (initialize-node! (UUID/randomUUID)))
  ([id] (alter-var-root #'*current-node* (constantly id))))

(defmacro with-node
  [id & body]
  `(binding [*current-node* ~id]
     ~@body))
