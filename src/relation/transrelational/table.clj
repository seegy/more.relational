(ns relation.transrelational.table)



(deftype Transrelation [keyorder fvt rrt])


(defn keyorder [tr]
  (.keyorder tr))

(defn fieldValues [tr]
  (.fvt tr))

(defn recordReconst [tr]
  (.rrt tr))

(defmethod print-method Transrelation
  [tr writer]
  (.write writer (str "#TR "  "Key order: " (keyorder  tr) " Field Value Table: " (fieldValues  tr) "Record Reconstruction Table: " (recordReconst  tr))))



(defn tr
  ""
  [table]
  (let [keyorder  (vec (keys (first table)))
        recordtuples (map (fn[record](reduce (fn[m [k v]]
                                               (assoc m k [v (.indexOf table record)]))
                                             {} record)) table)
        permutation (reduce (fn[ m head]
                              (assoc m head (sort-by first
                                                     (map (fn[record](get record head))
                                                          recordtuples))))
                            {} keyorder)
        fvt (reduce (fn[m [k v]](assoc m k  (map first v))) {} permutation)
        rrt (let [perm  (reduce (fn[m [k v]](assoc m k  (map second v))) {} permutation)
                  zigzagTo (fn [[a b]](map (fn[recnr] (.indexOf b recnr)) a))
                  fromPerm (vec (map (fn[x](get perm x)) keyorder))
                  toPerm (conj (vec (rest fromPerm)) (first fromPerm))
                  mergePerms (fn [m from to]
                               (if (empty? from)
                                 m
                                 (let [ffrom (first from)
                                       fto (first to)]
                                   (recur (conj m [ffrom fto]) (rest from) (rest to)))))]
              (map zigzagTo (mergePerms [] fromPerm toPerm)))]
    (Transrelation. keyorder fvt rrt)))
