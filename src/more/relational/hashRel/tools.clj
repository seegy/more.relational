(ns more.relational.hashRel.tools)

(defmacro relfn
  "Behaves like fn, but stores the source code in the metadata to allow
  optimisation."
  [args body]
  (with-meta (list 'fn args body)
             {:body (list 'quote body)}))

(defn same-type?
  "Checks if two relations have the same type, i.e. have the same header."
  [relation1 relation2]
  (=
    (sort (.head relation1))
    (sort (.head relation2))))

(defn same-attr-order?
  "Checks if the attributes of the relations are in the same order. If not or
  if the headers are not equal, it returns false."
  [relation1 relation2]
  (if (and (same-type? relation1 relation2)
        (= (.head relation1) (.head relation2)))
    true
    false))

(defn index-of
  "Finds the position of the item in the collection. Nil if not in there."
  [coll item]
  (let [index (.indexOf coll item)]
    (if (>= index 0)
      index
      nil)))


  (defn common-attr
  "Returns a vector of all attributes that both relations have in common.
  Order is that in relation 1."
  [relation1 relation2]
  (remove nil? (map #(some #{%} (.head relation2))
                 (.head relation1))))


  (defn diverging-attr
  "Returns a vector of all attributes that occur in relation 1, but are not
  common with relation 2. Order is that in relation 1."
  [relation1 relation2]
  (let [common (common-attr relation1 relation2)]
    (remove nil? (map #(if (some #{%} (.head relation2))
                         nil
                         %)
                 (.head relation1)))))

(defn sort-vec
  "Creates a vector showing the positions of the attributes in the second
  relation in the order of the first relation header. So the second relation
  can be ordered like the first one. Example:

  header of rel1 = [id name phone]
  header of rel2 = [name phone id]
  (sort-vec rel1 rel2) => [2 0 1]

  Reads like: The new first attribute of rel2 is currently on position 2, etc.

  If the predicate same-attr-order? is true, it will always return [0 1 2 ...]"
  [relation1 relation2]
  (when-not (same-type? relation1 relation2)
    (throw (IllegalArgumentException. "The two relations have different types.")))

  (let [h1 (.head relation1)
        h2 (.head relation2)]
    (vec (map #(index-of h2 %) h1))))


(defn attr-complement
  "Returns a vector of all attributes of the relation, except the one(s)
  specified. Parameter attributes may be a single keyword or a collection.
  Unknown attributes are ignored. Result may be empty."
  [relation attributes]
  (if (and (not (keyword? attributes)) (empty? attributes))
      (.head relation)
      (let [attrs (if (coll? attributes) (set attributes) #{attributes})]
        (vec (remove #(if (contains? attrs %) true false) (.head relation))))))
