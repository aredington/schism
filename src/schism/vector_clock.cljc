(ns schism.vector-clock
  "Utility functions for working with the vector clock of a value that
  participates in the Vclocked protocol."
  (:require [schism.protocols :as sp]
            [schism.node :as node])
  #?(:clj (:import (java.util Date))))

(defn now
  []
  #?(:clj (Date.)
     :cljs (js/Date.)))

(defmacro update-clock
  "Binds the current time to `binding`, executes body, then updates
  the body's return value which participates in
  schism.protocols/Vclocked, so that the vector clock contains the
  same time bound to `binding` for the current node."
  [binding & body]
  `(let [now# (now)
         ~binding now#
         ret# (do ~@body)
         prev-clock# (sp/get-clock ret#)]
     (sp/with-clock ret# (assoc prev-clock# node/*current-node* now#))))
