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
        rrt (drop-last (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt)))
        recMakeTuple (fn[attrs rrt row result]
                       (if (empty? attrs)
                         result
                         (let [rrt-cell (nth (first rrt) row)
                               value (fieldValueOf trans-table (first rrt-cell) (first attrs) )
                               nextRow (second rrt-cell)]
                           (println rrt-cell)
                           (println rrt)
                           (println result)
                           (println attrs)
                           (recur (rest attrs) (rest rrt) nextRow (assoc result (first attrs) value)))))]
  (recMakeTuple attrs rrt row {})))

(retrieve people 2 3)


(defn convert
  "Get a collection of all reconstructed rows by a transrelational table ordered by the first attribute."
  [trans-table]
 (map (fn[row] (retrieve trans-table 0 row)) (range (count trans-table))))






(defn insert
  "Returns the given transrelational table with the included datarow. The datarow has to be a map."
  [trans-table data-row]
  (when-not (= (set (keys data-row)) (set (keyorder trans-table)))
    (throw (IllegalArgumentException. "DataRow has not the same schema as the table")))
  (let [new-fvt (reduce (fn [m [k v]](assoc m k (sort (conj (get  (fieldValues trans-table) k) v)))) {}  data-row )
        indizes (map (fn[[k v]] (.lastIndexOf (get new-fvt k) v)) data-row)
        indizes-to (conj (vec (rest indizes)) (first indizes))
        merged-indizes (melt (fn [a b] [a b]) indizes indizes-to )
        new-rrt  (melt (fn[a [k v]] (map #(if (>= % v) (inc %) %) a)) (recordReconst trans-table) merged-indizes)
        new-rrt (melt (fn[a [k v]] (flatten (conj  (drop k a) v (take k a)))) new-rrt merged-indizes)]
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


