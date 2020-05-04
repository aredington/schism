(ns schism.test
  (:require #?(:cljs [doo.runner :refer-macros [doo-tests]])
            schism.node-test
            schism.core-test
            schism.impl.vector-clock-test
            schism.impl.types.set-test
            schism.impl.types.map-test
            schism.impl.types.list-test
            schism.impl.types.vector-test
            schism.impl.types.nested-map-test
            schism.impl.types.nested-vector-test))

#?(:cljs (doo-tests 'schism.node-test
                    'schism.core-test
                    'schism.impl.vector-clock-test
                    'schism.impl.types.set-test
                    'schism.impl.types.map-test
                    'schism.impl.types.list-test
                    'schism.impl.types.vector-test
                    'schism.impl.types.nested-map-test
                    'schism.impl.types.nested-vector-test))
