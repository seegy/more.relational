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
  (.write writer (str "#TR "  "Key order: " (keyorder  tr) "\n Field Value Table: " (fieldValues  tr) "\n Record Reconstruction Table: " (recordReconst  tr))))



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

(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
      {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
      {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }]))


(keyorder people)
(fieldValues people)
(count (first (recordReconst people)))




(defn convert
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
         fieldValues (map (fn[k] (get (fieldValues people) k)) (keyorder people))
         fieldValues (apply conj [(first fieldValues) ] (recApplySort (rest fieldValues) newRRT []))
         mergeBack (fn [ sortedValues result ]
                     (if (empty? (first sortedValues))
                         result
                         (recur (map rest sortedValues) (conj result (map first sortedValues)))))
         restructruedValues (mergeBack fieldValues [])]
    (vec (map (fn[tuple] (zipmap (keyorder trans-table) tuple)) restructruedValues))))



(time (convert people))

(defn zigzag
  ""
  [trans-table column row]
  (let [attrs  (keyorder trans-table)
        attrs (apply conj (vec (drop column attrs)) (drop-last (- (count attrs) column ) attrs))
        rrt (recordReconst trans-table)
        rrt (drop-last (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt)))
        getValueBy (fn[attrName row] (nth (get (fieldValues trans-table) attrName) row))
        recMakeTuple (fn[attrs rrt row result]
                       (if (empty? attrs)
                         result
                         (let [value (getValueBy (first attrs) row)
                               nextRow (nth (first rrt) row)]
                           (recur (rest attrs) (rest rrt) nextRow (assoc result (first attrs) value)))))
        ]
   (recMakeTuple attrs rrt row {})))


(zigzag people 2 3)
(zigzag people 0 0)
(recordReconst people)



(defn better-convert
  ""
  [trans-table]
 (map (fn[row] (zigzag trans-table 0 row)) (range (count trans-table))))


(time (convert people))
(time (better-convert people))
