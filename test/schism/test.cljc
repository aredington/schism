(ns schism.test
  (:require #?(:cljs [doo.runner :refer-macros [doo-tests]])
            schism.node-test
            schism.vector-clock-test
            schism.types.set-test
            schism.types.map-test
            schism.types.list-test
            schism.types.vector-test))

#?(:cljs (doo-tests 'schism.node-test
                    'schism.vector-clock-test
                    'schism.types.set-test
                    'schism.types.map-test
                    'schism.types.list-test
                    'schism.types.vector-test))
