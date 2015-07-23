(ns relation.tr.table)



(deftype Transrelation [keyorder fvt rrt])


(defn keyorder [tr]
  (.keyorder tr))

(defn fieldValues [tr]
  (.fvt tr))

(defn recordReconst [tr]
  (.rrt tr))

(defmethod print-method Transrelation
  [tr writer]
  (.write writer (str "#TR " (pr-str  (keyorder  tr) (fieldValues  tr) (recordReconst  tr)))))



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
                  heads (conj keyorder (first keyorder))]
              ())]
    (Transrelation. keyorder fvt rrt)))


(tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}])
