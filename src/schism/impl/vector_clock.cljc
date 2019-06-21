(ns schism.impl.vector-clock
  "Utility functions for working with the vector clock of a value that
  participates in the Vclocked protocol."
  (:require [schism.impl.protocols :as sp]
            [schism.impl.core :as ic]
            [schism.node :as node])
  #?(:clj (:import (java.util Date))))

(defmacro update-clock
  "Binds the current time to `binding`, executes body, then updates
  the body's return value which participates in
  schism.impl.protocols/Vclocked, so that the vector clock contains the
  same time bound to `binding` for the current node."
  [binding clocked & body]
  `(let [now# (ic/to-date
               (max (ic/to-millis (ic/now))
                    (inc (apply max 0 (map ic/to-millis (vals (sp/get-clock ~clocked)))))))
         ~binding now#
         ret# (do ~@body)
         ret-clock# (sp/get-clock ret#)]
     (sp/with-clock ret# (assoc ret-clock# node/*current-node* now#))))
