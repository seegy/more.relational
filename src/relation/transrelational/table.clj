(ns relation.transrelational.table)


(deftype Transrelation [keyorder fvt rrt]
   clojure.lang.Counted
    (count [this]
      (count (first (.rrt this))))
  )




(defn keyorder
  "Returns key order of the transrelational table"
  [tr]
    (.keyorder tr))



(defn fieldValues
  "Returns field value table of the transrelational table"
  [tr]
    (.fvt tr))



(defn recordReconst
  "Returns record reconstrution table order of the transrelational table"
  [tr]
   (.rrt tr))




(defmethod print-method Transrelation
  [tr writer]
  (.write writer (str "#TR Key order: " (keyorder  tr)
                      "\n\n Field Value Table:\n" (fieldValues  tr)
                      "\n\n Record Reconstruction Table:\n" (vec (recordReconst  tr)))))




(defn fieldValueOf
  "Returns the value of the cell by given positionl. The column can be defined as number or attribute name."
  [tr row column]
  (let [columnName (if (contains? (set (keyorder tr)) column) column (get (keyorder tr) column))
        xf (comp #(:value %) #(nth % row) #(get % columnName))]
     (xf (fieldValues tr))))



(defn- keyorder-of-table
  "creates vector of all keys in the table."
  [table]
  (into [] (distinct (flatten (map keys table)))))



(defn- create-raw-permutation
  "Creates a map of groups of attributes and their index of the origin data row."
  [table keyorder]
  (let [recordtuples (map (fn[record] (into {}
                                            (map (fn[k]
                                                   (let[v (get record k)]
                                                     [k [v (.indexOf table record)]]))
                                            keyorder))) table)]
    (into {} (map (fn[head] [head (sort-by first(map #(get % head) recordtuples))])) keyorder)))



(defn- create-fvt-by-raw-perm
  "Creates the field value table by the permutation map."
  [permutation]
  (into {} (map (fn[[k v]] [ k (let [ values (map first v)]
                                              (sequence (comp
                                                           (distinct)
                                                           (map (fn[v] {:value v
                                                                        :from (.indexOf values v)
                                                                        :to (.lastIndexOf values v)})))
                                                        values))]) permutation)))


(defn- create-rrt
  "Creates the record reconstruction table by the permutation map, predefined key order and the field value table."
  [permutation keyorder fvt]
  (let [perm  (into {} (map (fn[[k v]] [k (map second v)]) permutation))
        zigzagTo (fn [[a b]] (mapv (fn[recnr] (.indexOf b recnr)) a))
        fromPerm (mapv (fn[x](get perm x)) keyorder)
        toPerm (conj (vec (rest fromPerm)) (first fromPerm))
        mergePerms (loop [m []
                          from fromPerm
                          to toPerm]
                     (if (empty? from)
                       m
                       (let [ffrom (first from)
                             fto (first to)]
                         (recur (conj m [ffrom fto]) (rest from) (rest to)))))
        pre-consist-rrt (mapv (fn[a b] [b (zigzagTo a)]) mergePerms (range (count mergePerms)))]
                (map (fn [[index column]]
                       (let[columnName (nth keyorder index)]
                                (map (fn[cell] [(let[valueColumn (get fvt columnName)
                                                     index (.indexOf column cell)]
                                                     (.indexOf valueColumn
                                                           (first (filter #(and (<= index (:to %)) (>= index (:from %))) valueColumn))))
                                                cell]) column))) pre-consist-rrt)))




(defn tr
  "Creates a transrelational table by a collection of hash-maps with the same keys or by a
  prebuildes structure of keyorder, field value table and record reconstrution table."
  ([table]
   (let [table (distinct (vec table))
          keyorder  (keyorder-of-table table)
          permutation (create-raw-permutation table keyorder)
          fvt (create-fvt-by-raw-perm permutation)
          rrt (create-rrt permutation keyorder fvt)]
     (Transrelation. keyorder fvt rrt)))
  ([keyorder table]
    (let [table (distinct (vec table))
          permutation (create-raw-permutation table keyorder)
          fvt (create-fvt-by-raw-perm permutation)
          rrt (create-rrt permutation keyorder fvt)]
     (Transrelation. keyorder fvt rrt)))
  ([keyorder fvt rrt]
   (Transrelation. keyorder fvt rrt)))




