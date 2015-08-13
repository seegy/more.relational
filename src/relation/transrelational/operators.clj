(ns relation.transrelational.operators
  (:use [relation.transrelational.table]))


(defn retrieve
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

