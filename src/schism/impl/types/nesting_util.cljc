(ns schism.impl.types.nesting-util
  "Utility functions for supporting nested collections.")

(defn flat
  "Flattens a structure of associatives and sequentials to an
  associative of paths to leaf values. Each step in a path will retain
  both the type and the edge value, maps will be flattened to
  associatives. Vectors, lists, and other seqs will be flattened to
  sequentials."
  [c]
  (if-not (coll? c)
    c
    (let [marker (if (map? c) 'a 's)
          m (if (map? c)
              c
              (map-indexed (fn [i e] [i e]) c))]
      (into {}
            (mapcat (fn [[k v]]
                      (let [child-flat (flat v)]
                        (if (coll? child-flat)
                          (for [[path element] child-flat]
                            [(apply vector [marker k] path) element])
                          [[[[marker k]] child-flat]]))))
            m))))

(defn access-path
  "`flat` returns a reconstitution path of both type and key. This is
  useful for reprojecting the flat version up into a nested structure,
  but is not amenable to `get-in`, `assoc-in`, et al. `access-path` will
  convert a reconstitution path into a more conventional access path."
  [reconstitution-path]
  (map second reconstitution-path))

(defn- assoc*
  [m [type key] v]
  (assoc (if m
           m
           (cond
             (= type 'a) {}
             (= type 's) []))
         key v))

(defn- assoc-in*
  [m [[type key :as kspec] & kspecs] v]
  (if kspecs
    (assoc* m kspec (assoc-in* (get m key) kspecs v))
    (assoc* m kspec v)))

(defn project
  "Constitutes a structure as produced by `flat` up into a nested
  collection of maps and vectors. As all sequential items are coerced
  to vectors, this is not reflexive of `flat`."
  [m]
  (reduce (fn [m [k v]]
            (assoc-in* m k v))
          nil
          m))
