(ns relation.combi)

(defrecord Dataset [column-names rows])

(defn dataset
  "
  Returns a map of type incanter.core.dataset constructed from the given column-names and
  data. The data is either a sequence of sequences or a sequence of hash-maps.
  "
  ([column-names & data]
    (let [dat (cond
                (or (map? (ffirst data)) (coll? (ffirst data)))
                  (first data)
                (map? (first data))
                  data
                :else
                  (map vector (first data)))
          rows (cond
                 (map? dat)
                   [dat]
                 (map? (first dat))
                   dat
                 :else
                   (map #(apply assoc {} (interleave column-names %)) dat))]
      (Dataset. (into [] column-names) rows))))



(def r (dataset [:x1 :x2 :x3]
 [[1 2 3]
 [4 5 6]
 [7 8 9]]))

(defmacro relfn
  "Behaves like fn, but stores the source code in the metadata to allow
  optimisation."
  [args body]
  (with-meta (list 'fn args body)
             {:body (list 'quote body)}))


 (defn project [relation attributes]
    (if (map? attributes)
      ;TODO attributes is a hash map
      (let [head (vec (keys attributes))
            ; seq with functions that return the correct value for the position
            new-rel (set (map (fn [t]
                                (apply merge (map (fn [[k v]] {k (if (or (keyword? v) (fn? v))
                                                                   (v t)
                                                                   v)}) attributes)))
                              (:rows relation)))]
        (dataset new-rel))

      ; attributes is a set/vector/list
      (let [value-tuples (set (map #(vec (map (fn [p] (% p)) attributes))
                                (:rows relation)))]
        (dataset attributes value-tuples))))

  r
 (project r [:x1 :x3])
 (project r {:x1 :x1, :new-x3 (relfn [t] (* 2 (:x3 t)))})

 (seq r)
