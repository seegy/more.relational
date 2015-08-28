(ns relation.transrelational.table)




(deftype Transrelation [keyorder fvt rrt]
   clojure.lang.Counted
    (count [this]
      (count (first (.rrt this))))
  )




(defn keyorder [tr]
  (.keyorder tr))



(defn fieldValues [tr]
  (.fvt tr))



(defn recordReconst [tr]
  (.rrt tr))




(defmethod print-method Transrelation
  [tr writer]
  (.write writer (str "#TR Key order: " (keyorder  tr)
                      "\n\n Field Value Table:\n" (fieldValues  tr)
                      "\n\n Record Reconstruction Table:\n" (vec (recordReconst  tr)))))



(defn fieldValueOf
  ""
  [tr row column]
  (let [columName (if (keyword? column) column (get (keyorder tr) column))]
     (:value (nth (get (fieldValues tr) columName) row))))




(defn tr
  ""
  ([table]
  (let [keyorder  (vec (keys (first table)))
        recordtuples (map (fn[record](reduce (fn[m [k v]]
                                               (assoc m k [v (.indexOf table record)]))
                                             {} record)) table)
        permutation (reduce (fn[ m head]
                              (assoc m head (sort-by first
                                                     (map (fn[record](get record head))
                                                          recordtuples))))
                            {} keyorder)
        fvt (reduce (fn[m [k v]](assoc m k
                                  (let [ values (map (fn[cell] (first cell)) v)]
                                    (distinct (map (fn[v] {:value v :from (.indexOf values v) :to (.lastIndexOf values v)}) values))))) {} permutation)

        rrt (let [perm  (reduce (fn[m [k v]](assoc m k  (map second v))) {} permutation)
                  zigzagTo (fn [[a b]](vec (map (fn[recnr] (.indexOf b recnr)) a)))
                  fromPerm (vec (map (fn[x](get perm x)) keyorder))
                  toPerm (conj (vec (rest fromPerm)) (first fromPerm))
                  mergePerms (fn [m from to]
                               (if (empty? from)
                                 m
                                 (let [ffrom (first from)
                                       fto (first to)]
                                   (recur (conj m [ffrom fto]) (rest from) (rest to)))))
                  pre-consist-rrt (into [] (comp (map zigzagTo) ) (mergePerms [] fromPerm toPerm))]
              (map (fn[column](let[columnName (nth keyorder (.indexOf pre-consist-rrt column))]
                                (map (fn[cell] [(let[valueColumn (get fvt columnName)]
                                                  (.indexOf valueColumn
                                                           (first (filter (fn [x] (let [index (.indexOf column cell)](and (<= index (:to x)) (>= index (:from x))))) valueColumn))
                                                            )
                                                  )
                                                cell]) column))) pre-consist-rrt))]
     (Transrelation. keyorder fvt rrt)))
  ([keyorder fvt rrt]
   (Transrelation. keyorder fvt rrt)))




(defn- convert
  ""
  [trans-table]
  (let [ rrt (recordReconst trans-table)
         recResort (fn [a l result]
                     (if (empty? l)
                       result
                       (let[toSort (first l)
                            sorted (seq (sort-by (fn[x] (.indexOf a (.indexOf toSort x) )) toSort))]
                         (recur sorted (rest l) (conj result sorted)))))
         newRRT (apply conj [(first rrt)]  (recResort (first rrt) (drop-last (rest rrt)) []))
         recApplySort (fn [values sortVectors result]
                        (if (empty? values)
                          result
                          (recur (rest values)
                                 (rest sortVectors)
                                 (conj result (map (fn[x] (nth (first values) x)) (first sortVectors))))))
         fieldValues (map (fn[k] (get (fieldValues trans-table) k)) (keyorder trans-table))
         fieldValues (apply conj [(first fieldValues) ] (recApplySort (rest fieldValues) newRRT []))
         mergeBack (fn [ sortedValues result ]
                     (if (empty? (first sortedValues))
                         result
                         (recur (map rest sortedValues) (conj result (map first sortedValues)))))
         restructruedValues (mergeBack fieldValues [])]
    (vec (map (fn[tuple] (zipmap (keyorder trans-table) tuple)) restructruedValues))))


