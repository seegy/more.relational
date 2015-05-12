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
   "Returns the relation with only the attributes specified in pmap. That is a
    hash map where the key is the final name and the value is what shall be
    projected. This can be an attribute (can be renamed) or a list, representing
    a function. Attributes must be written as a keyword and every keyword will
    be interpreted as an attribute, if it appears in the original relation. If
    it is a function, it takes a single argument representing a tuple.

    For convenience, project-map can also be a set of attributes, that shall be
    contained in the resulting relation, but now functions or new names can be
    given.

    Examples:
      (project r {:sno :sno, :supplier-city :city})
      (project r #{:sno :city})
      (project r {:sno :sno, :new-status (relfn [t] (* 2 (:status t)))})"
    (if (map? attributes)
      ; attributes is a hash map
      (let [head (vec (keys attributes))
            ; seq with functions that return the correct value for the position
            new-rel (set
                     (map (fn [t]
                            (apply merge
                                   (map (fn [[k v]] {k (if (or
                                                            (keyword? v) (fn? v))
                                                         (v t) v)}) attributes))) (:rows relation)))]
        (dataset (keys attributes) new-rel))
      ; attributes is a set/vector/list
      (let [value-tuples (set (map #(vec (map (fn [p] (% p)) attributes))
                                (:rows relation)))]
        (dataset attributes value-tuples))))

  r


  (project r [:x1 :x3])


 (project r {:x1 :x1, :new-x3 (relfn [t] (* 2 (:x3 t)))})



 (defn project- [relation attributes]
    "Projects the relation with all original attributes, but the one specified.
    Think of it as \"remove\".

    Example:
      (project- r #{:sno})  ; relation r without :sno"
    (let [attrs (if (set? attributes) attributes (set attributes))
          pos (remove nil? (map #(if (contains? attrs %)
                                    nil
                                    %)
                                (:column-names relation)))]
      (project relation pos)))



 (let [attrs (if (set? [:x3]) [ :x3] (set [ :x3]))
          pos (remove nil? (map #(if (contains? attrs %)
                                    nil
                                     %)
                                (:column-names r)))]
   project r pos)


 (project- r [:x3])




 (defn project+ [relation extend-map]
   "Extends the relation with the attributes specified in extend-map. In this,
    a key is a new attribute and the value a tuple function. The same effect can
    be achieved with project.

    Examples:
      (project+ r {:new-price (relfn [t] (* 1.05 (:price t)))})
      ; same as
      (project r {:a1 :a1, :a2 :a2, ..., :an :an,
                  :new-price (relfn [t] (* 1.05 (:price t))"
   (let [atts (reduce #(fn [t](assoc )) {}) ] ; TODO keys von relation zu map, extend-map dazu, dann 'project' aufrufen
     ))

 (project r {:new-x3 (relfn [t] (* 2 (:x3 t)))})
