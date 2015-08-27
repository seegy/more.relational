(ns relation.transrelational.operators
  (:use [relation.transrelational.table]))





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




(time (def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
      {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
      {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }])))



(defn convert
  "Get a collection of all reconstructed rows by a transrelational table ordered by the first attribute."
  [trans-table]
 (map (fn[row] (retrieve trans-table  row 0)) (range (count trans-table))))





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
                  (melt (fn[column [a-value-index a-next-pointer b-next-pointer a-is-new]] (let [prepared-column (map (fn [[ a b ]] [ (if (and (>= a a-value-index) a-is-new) (inc a) a)
                                                                                                                                      (if (>= b b-next-pointer) (inc b) b) ]) column)]
                                                 (concat (take a-next-pointer prepared-column) [[a-value-index b-next-pointer]] (drop a-next-pointer prepared-column) )))
                        (recordReconst trans-table) entry-infos))]
    (tr (keyorder trans-table) new-fvt new-rrt)))









(defn delete
  ""
  [trans-table  row column]
  (let [attrs  (keyorder trans-table)
        attrs (apply conj (vec (drop column attrs)) (drop-last (- (count attrs) column ) attrs))
        rrt (recordReconst trans-table)
        rrt  (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt))
        recMakeTuple (fn[attrs rrt row result]
                       (if (empty? attrs)
                         result
                         (let [ nextRow (nth (first rrt) row)]
                           (recur (rest attrs) (rest rrt) nextRow (assoc result (first attrs) [row nextRow])))))
        indizes (recMakeTuple attrs rrt row {})
        new-fvt (reduce (fn[fft [k [index next-index]]] (assoc fft k (drop-index (get fft k) index))) (fieldValues trans-table) indizes)
        new-rrt (melt (fn[column attr] (vec (map (fn[x] (if (> x (second (get indizes attr))) (dec x) x))
                                            (drop-index column (first (get indizes attr)))))) (recordReconst trans-table) (keyorder trans-table))]
     (tr (keyorder trans-table) new-fvt new-rrt)))



(defn update
  ""
  [trans-table  row column update-map]
  (let [old-row (retrieve trans-table column row)
        new-row (merge old-row update-map)]
    (insert (delete trans-table column row) new-row)))





(defn order
  ""
  [trans-table attr]
  (let []
    attr))


