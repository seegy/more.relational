(ns relation.newRel.tools)

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
  #_(let [res (count (take-while (partial not= item) coll))]
     (if (>= res (count coll))
       nil
       res))

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
