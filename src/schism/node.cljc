(ns schism.node
  #?(:clj (:import (java.util UUID))))

(def ^:dynamic *current-node* nil)

(defn initialize-node!
  "Initialize `schism.node/*current-node*` to the passed in value,
  or a new random UUID. While any serializable value suffices as the
  current node, it should be unique within the cluster; a repeated
  node value may result in incorrect behaviors during convergence."
  ([] (initialize-node!
       #?(:clj (UUID/randomUUID)
          :cljs (random-uuid))))
  ([id] #?(:clj (alter-var-root #'*current-node* (constantly id))
           :cljs (set! *current-node* id))))

(defmacro with-node
  "Override the value of `schism.node/*current-node*` for the scope of
  `body`."
  [id & body]
  `(binding [*current-node* ~id]
     ~@body))
