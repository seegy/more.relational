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




 (defn project+ [relation extend-map]
   "Extends the relation with the attributes specified in extend-map. In this,
    a key is a new attribute and the value a tuple function. The same effect can
    be achieved with project.

    Examples:
      (project+ r {:new-price (relfn [t] (* 1.05 (:price t)))})
      ; same as
      (project r {:a1 :a1, :a2 :a2, ..., :an :an,
                  :new-price (relfn [t] (* 1.05 (:price t))"
   (let [head (set (apply merge (:column-names relation) (map first extend-map)))
         data (set (map (fn [t]
                            (apply merge t
                                   (map (fn [[k v]] {k (if (or
                                                            (keyword? v) (fn? v))
                                                         (v t) v)}) extend-map))) (:rows relation)))]
     (dataset head data)))




 (defn rename [relation smap]
   "Given a substitution map, it renames the attributes.

    Example:
      (rename r {:pname :product-name})"
    (Relation. (replace smap (.head relation)) (.body relation)))


 ; TODO BAUSTELLE
(replace {:x1 :x1v2} (:column-names r))
(map #(replace {:x1 :x1v2} %) (:rows r))

  (defn rename* [relation match-exp replace-str]
    "Renames all attributes that match match-regexp with replace-str. Semantics
    are the same as clojure.string/replace.

    Example:
      (rename* r #\"(.+)\" \"prefix-$1\""
    (rel (vec (map (fn [a]
                                 (-> a name (str/replace match-exp replace-str) keyword))
                               (.head relation)))
                     (.body relation)))

  (defn restrict [relation predicate]
    "Returns a relation with only the tuples that satisfy the predicate pred?.
    That is a usual function, but fn shall be replaced with relfn, so that
    optimization can be done.

    Examples:
      (restrict r (relfn [t] (= (:sno t) \"S12\")))"
    (rel (set (filter predicate (seq relation)))))






 ;############################################################################################
 ;########################                   USAGE TEST                     ##################
 ;############################################################################################






 (def r (dataset [:x1 :x2 :x3]
           [[1 2 3]
           [4 5 6]
           [7 8 9]]))

 r

 (project r [:x1 :x3])


 (project r {:x1 :x1, :new-x3 (relfn [t] (* 2 (:x3 t)))})


 (project- r [:x3])


 (project+ r {:new-x3 (relfn [t] (* 2 (:x3 t))), :new-x2 (relfn [t] (+ 1 (:x2 t)))})
