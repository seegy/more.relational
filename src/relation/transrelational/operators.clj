(ns relation.transrelational.operators
  (:use [relation.transrelational.table]))


;; #######################################################################################################################################
;; Tools
;; #######################################################################################################################################



(defn- melt [f a b & more]
  (let [args (filter #(not (nil? %)) (conj [] a b more))]
    (when-not (= (map count args))
      (throw (IllegalArgumentException. "Data sets have not the same length.")))
    (let [melter (fn [result a b]
                   (if (empty? a)
                     result
                     (recur (conj result (f (first a) (first b))) (rest a) (rest b))))]
    (apply melter [] args))))

(defn- drop-index [col idx]
  (filter identity (map-indexed #(if (not= %1 idx) %2) col)))




;; #######################################################################################################################################
;; Basic operations
;; #######################################################################################################################################



(defn retrieve
  "Get a row of data by the position of one of the cells in the transrelational table."
  [trans-table row column]
  (let [attrs  (keyorder trans-table)
        attrs (apply conj (vec (drop column attrs)) (drop-last (- (count attrs) column ) attrs))
        rrt (recordReconst trans-table)
        rrt (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt))
        recMakeTuple (fn[attrs rrt row result]
                       (if (empty? attrs)
                         result
                         (let [rrt-cell (nth (first rrt) row)
                               value (fieldValueOf trans-table (first rrt-cell) (first attrs) )
                               nextRow (second rrt-cell)]
                           (recur (rest attrs) (rest rrt) nextRow (assoc result (first attrs) value)))))]
  (recMakeTuple attrs rrt row {})))






(defn convert
  "Get a collection of all reconstructed rows by a transrelational table ordered by the first attribute."
  ([trans-table]
   (map (fn[row] (retrieve trans-table  row 0)) (range (count trans-table))))
  ([trans-table order]
   (if (empty? order)
     (convert trans-table)
     (if (not-any? #(contains? (set (keyorder trans-table))) order)
       (throw (IllegalArgumentException. "Order attribute not part of relation"))
       (let [attrs (keyorder trans-table)
             row-of-last (.indexOf attrs (last order))
             preorderd-tr (map (fn[row] (retrieve trans-table  row row-of-last)) (range (count trans-table)))]
         (sort-by (apply juxt (drop-last 1 order) ) preorderd-tr ))))))



(defn insert
  "Returns the given transrelational table with the included datarow. The datarow has to be a map."
  [trans-table data-row]
  (when-not (= (set (keys data-row)) (set (keyorder trans-table)))
    (throw (IllegalArgumentException. "DataRow has not the same schema as the table")))
  (let [fvt-manipulation (reduce (fn [m [k v]](assoc m k
                                       (let[old-column (get  (fieldValues trans-table) k)
                                            untouched (filter #(neg? (compare (:value %) v)) old-column)
                                            increased (sequence (comp
                                                                 (map (fn[cell] (merge-with + cell {:from 1 :to 1})))
                                                                 (filter #(pos? (compare (:value %) v)))
                                                                 ) old-column)
                                            target (let[found (first (filter #(= (:value %) v) old-column))]
                                                     (if (nil? found)
                                                       (let [index (if (empty? untouched) 0 (inc (:to (last untouched))))]{:value v :from index :to index})
                                                       (merge-with + found {:to 1})))]
                                          [(concat untouched [target] increased) [target (count untouched)]]))) {}  data-row )
        new-fvt (reduce (fn[m [k [ v _ ]]] (assoc m k v)) {} fvt-manipulation)
        new-rrt (let [inserts (reduce (fn[m [k [ _ v ]]] (assoc m k v)) {} fvt-manipulation)
                      inserts-in-order (map #(get inserts %) (keyorder trans-table))
                      entry-infos (melt (fn [a b] [ (second a)
                                                    (:to (first a))
                                                    (:to (first b))
                                                    (= (:to (first a)) (:from (first a)))] )
                                        inserts-in-order (conj (vec (rest inserts-in-order)) (first inserts-in-order)))]
                  (melt (fn[column [a-value-index a-next-pointer b-next-pointer a-is-new]]
                          (let [prepared-column (map (fn [[ a b ]] [ (if (and (>= a a-value-index) a-is-new) (inc a) a)
                                                                     (if (>= b b-next-pointer) (inc b) b) ]) column)]
                                                 (concat (take a-next-pointer prepared-column) [[a-value-index b-next-pointer]] (drop a-next-pointer prepared-column) )))
                        (recordReconst trans-table) entry-infos))]
    (tr (keyorder trans-table) new-fvt new-rrt)))







(defn delete
  ""
  [trans-table  row column]
  (let [attrs (let [attrs (keyorder trans-table)]
                (apply conj (vec (drop column attrs)) (drop-last (- (count attrs) column ) attrs)))
        rrt (let [ rrt (recordReconst trans-table)]
              (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt)))
        recMakeTuple (fn[attrs rrt row result]
                       (if (empty? attrs)
                         result
                         (let [ cell  (nth (first rrt) row)]
                           (recur (rest attrs) (rest rrt) (second cell) (assoc result (first attrs) cell)))))
        indizes (recMakeTuple attrs rrt row {})
        delete-Value (fn [attr index]
                       (let[untouched (take index (get (fieldValues trans-table) attr))
                            target (let [zwerg (merge-with - (nth (get (fieldValues trans-table) attr) index) {:to 1} )]
                                     (if (< (:to zwerg ) (:from zwerg))
                                       '()
                                       [zwerg]))
                            tail (map (fn [m] (merge-with - m {:from 1 :to 1} )) (drop (inc index) (get (fieldValues trans-table) attr))) ]
                         [(concat untouched target  tail)
                          (empty? target)]))
        fvt-manipulation (map (fn [[attr [index _]]] (delete-Value attr index)) indizes)
        new-fvt (apply merge (melt (fn [attr [column _]] {attr column}) attrs fvt-manipulation))
        entry-infos (let [indizes (map #(get indizes %) attrs)]
                      (melt (fn [[_ a] [b c d ]] [a b c d])
                            (apply conj [(last indizes)] (drop-last indizes))
                            (melt (fn [[a b] c] [a b c])  indizes (map second fvt-manipulation))))
        new-rrt (let [filtered-rrt (melt (fn [[index _ _ _] column]
                                           (concat (take index column) (drop (inc index) column)))
                                         entry-infos rrt)
                      new-rrt (melt (fn [[_ value-link next-link is-deleted] column]
                                     ( map (fn[[cell-value cell-next]]
                                            [(if (and is-deleted (> cell-value value-link)) (dec cell-value) cell-value)
                                             (if (> cell-next next-link) (dec cell-next) cell-next)]) column)) entry-infos filtered-rrt)]
                  (apply conj (vec (drop (- (count rrt) column ) new-rrt)) (drop-last  column new-rrt)))]
   (tr (keyorder trans-table)  new-fvt new-rrt)))









(defn update
  ""
  [trans-table  row column update-map]
  (when-not (not-any? #(contains? (keyorder trans-table) %) (keys update-map))
    (throw (IllegalArgumentException. "Update map contains illegal attribute.")))
  (let [old-row (retrieve trans-table row column)
        new-row (merge old-row update-map)]
    (insert (delete trans-table row column) new-row) ))







;; #######################################################################################################################################
;; Algera operations
;; #######################################################################################################################################


(defn restrict
  ""
  [trans-table pred])

