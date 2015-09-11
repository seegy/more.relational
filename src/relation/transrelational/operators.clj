(ns relation.transrelational.operators
  (:use [relation.transrelational.table]))


;; #######################################################################################################################################
;; Tools
;; #######################################################################################################################################




(defn- drop-index [col idx]
  (filter identity (map-indexed #(if (not= %1 idx) %2) col)))

(defmacro tr-fn
  "Behaves like fn, but stores the source code in the metadata to allow
  optimisation."
  [args body]
  (with-meta (list 'fn args body)
              {:origBody (list 'quote body)}))


(tr-fn [t] (and (>= (:status t) 30) (= (:city t) "Paris") (= (:name t) (:city t))))


(defn zigzag
  "Get a row of data by the position of one of the cells in the transrelational table."
  [trans-table row column]
  (let [rrt (recordReconst trans-table)
        rrt (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt))
        recMakeTuple (fn[rrt row result]
                       (if (empty? rrt)
                         result
                         (let [[value-link next-row] (nth (first rrt) row)]
                           (recur (rest rrt) next-row (conj result value-link)))))]
  (recMakeTuple rrt row [])))



;; #######################################################################################################################################
;; Basic operations
;; #######################################################################################################################################



(defn retrieve
  "Get a row of data by the position of one of the cells in the transrelational table."
  [trans-table row column]
  (let [attrs  (keyorder trans-table)
        attrs (apply conj (vec (drop column attrs)) (drop-last (- (count attrs) column ) attrs))
        indizes (zigzag trans-table row column)]
 (into {} (map (fn [attr index][attr (fieldValueOf trans-table index attr)] ) attrs indizes))))



(defn convert
  "Get a collection of all reconstructed rows by a transrelational table ordered by the first attribute."
  ([trans-table]
   (map (fn[row] (retrieve trans-table  row 0)) (range (count trans-table))))
  ([trans-table order]
   (if (empty? order)
     (convert trans-table)
     (if (not-any? #(contains? (set (keyorder trans-table)) %) order)
       (throw (IllegalArgumentException. "Order attribute not part of relation"))
       (let [attrs (keyorder trans-table)
             row-of-last (.indexOf attrs (last order))
             preorderd-tr (map (fn[row] (retrieve trans-table  row row-of-last)) (range (count trans-table)))]
         (if (empty? (drop-last 1 order))
           preorderd-tr
           (sort-by (apply juxt (drop-last 1 order) ) preorderd-tr )))))))







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
        new-fvt (apply merge (mapv (fn [attr [column _]] {attr column}) attrs fvt-manipulation))
        entry-infos (let [indizes (map #(get indizes %) attrs)]
                      (mapv (fn [[_ a] [b c d ]] [a b c d])
                            (apply conj [(last indizes)] (drop-last indizes))
                            (mapv (fn [[a b] c] [a b c])  indizes (map second fvt-manipulation))))
        new-rrt (let [filtered-rrt (mapv (fn [[index _ _ _] column]
                                           (concat (take index column) (drop (inc index) column)))
                                         entry-infos rrt)
                      new-rrt (mapv (fn [[_ value-link next-link is-deleted] column]
                                     ( map (fn[[cell-value cell-next]]
                                            [(if (and is-deleted (> cell-value value-link)) (dec cell-value) cell-value)
                                             (if (> cell-next next-link) (dec cell-next) cell-next)]) column)) entry-infos filtered-rrt)]
                  (apply conj (vec (drop (- (count rrt) column ) new-rrt)) (drop-last  column new-rrt)))]
   (tr (keyorder trans-table)  new-fvt new-rrt)))



(defn distinct-tr
  [trans-table]
  (let [indezes (vals (clojure.set/map-invert (into (sorted-map-by >) (map (fn [x] [x (zigzag trans-table x 0)]) (range (count trans-table))))))
        to-delete (filter #(not (contains? (set indezes) %)) (range (count trans-table)))]
    (reduce (fn [tr index] (delete tr index 0)) trans-table to-delete)))




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
                      entry-infos (mapv (fn [a b] [ (second a)
                                                    (:to (first a))
                                                    (:to (first b))
                                                    (= (:to (first a)) (:from (first a)))] )
                                        inserts-in-order (conj (vec (rest inserts-in-order)) (first inserts-in-order)))]
                  (mapv (fn[column [a-value-index a-next-pointer b-next-pointer a-is-new]]
                          (let [prepared-column (map (fn [[ a b ]] [ (if (and (>= a a-value-index) a-is-new) (inc a) a)
                                                                     (if (>= b b-next-pointer) (inc b) b) ]) column)]
                                                 (concat (take a-next-pointer prepared-column) [[a-value-index b-next-pointer]] (drop a-next-pointer prepared-column) )))
                        (recordReconst trans-table) entry-infos))]
    (distinct-tr (tr (keyorder trans-table) new-fvt new-rrt))))


(defn update
  ""
  [trans-table  row column update-map]
  (when (not-any? #(contains? (set (keyorder trans-table)) %) (keys update-map))
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

(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))

people


(defn project
    ""
  [trans-table attrs]
  (when (not-any? #(contains? (set (keyorder trans-table)) %) attrs)
    (throw (IllegalArgumentException. "Update map contains illegal attribute.")))
  (let [to-delete (filterv #(not (contains? (set attrs) %)) (keyorder trans-table))
        new-fvt (reduce (fn[m attr] (dissoc m attr)) (fieldValues trans-table) to-delete)
        new-rrt (let [delete-indizes (map (fn[attr] (.indexOf (keyorder trans-table) attr)) to-delete)
                      change-indizes (map #(mod (dec %) (count (keyorder trans-table))) delete-indizes)
                      melted (sort  (mapv (fn [a b] [a b]) change-indizes delete-indizes ))
                      merged (reduce (fn[m [a b]] (if (contains?  m b)
                                                    (assoc (dissoc m b) a (get m b))
                                                    (if (contains? (set (vals m)) a)
                                                      (assoc m (get (clojure.set/map-invert m) a)  b)
                                                      (assoc m a b)))) {} melted )
                      replace-link (fn[column-index steps]
                                     (let [columns (map #(nth (recordReconst trans-table) (mod %  (count (keyorder trans-table))))
                                                        (range (inc column-index) (+ column-index steps 1)))
                                           rec-replace (fn[base columns]
                                                         (if (empty? columns)
                                                           base
                                                           (recur
                                                            (map (fn [[a b]] [a (second (nth (first columns) b))]) base)
                                                            (rest columns))))]
                                       (rec-replace (nth (recordReconst trans-table) column-index) columns)))
                      manipulated-columns (reduce (fn[m [a b] ] (assoc m a (replace-link a (mod (- b a) (count (keyorder trans-table)))))) {} merged)
                      new-rrt (let [ with-new-colums (reduce (fn[m [k v]](assoc m k v)) (vec (recordReconst trans-table)) manipulated-columns )]
                                (filter #(not (contains? (set delete-indizes) (.indexOf with-new-colums %))) with-new-colums))]
                  new-rrt)
        new-ko (filterv #(contains? (set attrs) %) (keyorder trans-table))]
     (distinct-tr (tr new-ko new-fvt new-rrt))))




(defn project+
    ""
  [trans-table attrs]
  (when (not-any? #(contains? (set (keyorder trans-table)) %) attrs)
    (throw (IllegalArgumentException. "Update map contains illegal attribute.")))
  (let [to-delete (filterv #(not (contains? (set attrs) %)) (keyorder trans-table))
        converted (convert trans-table)
        new-converted (distinct (map (fn [m](sort-by (apply juxt attrs) (apply dissoc m to-delete))) converted))]
     (tr new-converted)))





(time (project people [:name :city ]))
(time (project+ people [:name :city ]))

(time (project people [ :id :name ]))

(time (project people [:status :city ]))
(time (project+ people [:status :city ]))

(time (project people [:id :name :city]))

(time (project people [ :city :name]))
(time (project+ people [ :city :name]))


(defn extend
  ""
  [trans-table preds]
  (let[table (convert trans-table)
       extended (map (fn [row]( reduce )) table)] ;TODO
    extended))

(extend people {:backwardNames (tr-fn [t] (reverse (:name t)))})
