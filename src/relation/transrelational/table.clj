(ns relation.transrelational.table)



 (defn travel
   [rrt row column steps]
   (let [columns (mapv #(get rrt (mod %  (count rrt)))
                      (range  column (+ column steps 1)))
         rec-travel (fn [row columns ]
                      (if (empty? (rest columns))
                        (get (first columns) row)
                        (recur (second (get (first columns) row))  (rest columns))))]
     (rec-travel row columns)))



 (defprotocol TR_Protocol
   (fieldValueOf [this row column])
   (zigzag [this row column])
   (retrieve [this row column])
   (convert [this] [this order]))



(deftype Transrelation [keyorder fvt rrt]
   clojure.lang.Counted
    (count [this]
      (count (first (.rrt this))))


    TR_Protocol
    (fieldValueOf [this row column]
      (let [columnName (if (contains? (set (.keyorder this)) column) column (get (.keyorder this) column))
          xf (comp #(:value %) #(get % row) #(get % columnName))]
       (xf (.fvt this))))

    (zigzag [this row column]
      (let [rrt (.rrt this)
          rrt (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt))]
       (loop [rrt rrt
              row row
              result []]
         (if (empty? rrt)
             result
             (let [[value-link next-row] (get (first rrt) row)]
                  (recur (rest rrt) next-row (conj result value-link)))))))

    (retrieve [this row column]
      (let [orig-attrs  (.keyorder this)
            attrs (apply conj (vec (drop column orig-attrs)) (drop-last (- (count orig-attrs) column ) orig-attrs))
            indizes (zigzag this row column)]
       (conj {} (select-keys (into {} (map (fn [attr index][attr (fieldValueOf this index attr)] ) attrs indizes)) orig-attrs))))


    (convert [this]
     (map (fn[row] (retrieve this row 0)) (range (count this))))

    (convert [this order]
     (if (empty? order)
       (convert this)
       (if (not-any? #(contains? (set (.keyorder this)) %) order)
         (throw (IllegalArgumentException. "Order attribute not part of relation"))
         (let [attrs (.keyorder this)
               row-of-last (.indexOf attrs (last order))
               preorderd-tr (map (fn[row] (retrieve this  row row-of-last)) (range (count this)))]
           (if (empty? (drop-last 1 order))
             preorderd-tr
             (sort-by (apply juxt (drop-last 1 order) ) preorderd-tr ))))))

    clojure.lang.Seqable
    (seq [this]
         (convert this))
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
  (.write writer (str "#TR Key order:\n" (keyorder  tr)
                      "\n\n Field Value Table:\n" (clojure.string/join "\n" (map (fn [[k v]] (str k "\n" (clojure.string/join "\n" v) "\n")) (fieldValues  tr)))
                      "\n\n Record Reconstruction Table:\n" (clojure.string/join "\n" (apply mapv vector (recordReconst tr))))))




(defn- keyorder-of-table
  "creates vector of all keys in the table."
  [table]
    (vec (distinct (reduce (fn[v ks] (apply conj v ks)) [] (mapv keys table)))))




(defn- create-raw-permutation
  "Creates a map of groups of attributes and their index of the origin data row."
  [table keyorder]
  (let [recordtuples  (map-indexed (fn[ index record ] (into {}
                                            (map (fn[k]
                                                   (let[v (get record k)]
                                                     [k [v index]]))
                                            keyorder))) table)]
     (into {} (map (fn[head] [head (sort-by first (map #(get % head) recordtuples))])) keyorder)))





(defn- create-fvt-by-raw-perm
  "Creates the field value table by the permutation map."
  [permutation]
   (into {} (map (fn[[k v]] [ k (let [ values  (map-indexed (fn[index [value _]] {:value value  :index index}) v)
                                       indexed   (clojure.set/index values [:value])]
                                  (into [](sort-by :from (mapv (fn[[k entry]] {:value (:value k)
                                                     :from (:index (apply min-key :index entry))
                                                     :to (:index (apply max-key :index entry)) }) indexed))))]) permutation)))

(defn- create-rrt
  "Creates the record reconstruction table by the permutation map, predefined key order and the field value table."
  [permutation keyorder fvt]
  (let [perm  (into {} (map (fn[[k v]] [k (map second v)]) permutation))
        zigzagTo (fn [[a b]] (let[prepared-b (into {} (map-indexed (fn[i x] [x i]) b)) ]
                               (mapv (fn[recnr] (get prepared-b recnr)) a)))
        fromPerm  (mapv (fn[x](get perm x)) keyorder)
        toPerm  (conj (vec (rest fromPerm)) (first fromPerm))
        mergePerms  (loop [m []
                          from fromPerm
                          to toPerm]
                     (if (empty? from)
                       m
                       (let [ffrom (first from)
                             fto (first to)]
                         (recur (conj m [ffrom fto]) (rest from) (rest to)))))
        pre-consist-rrt  (vec (map-indexed (fn[b a] [b (zigzagTo a)]) mergePerms))]
                 (mapv (fn [[index column]]
                       (let[ columnName (get keyorder index)
                             valueColumn (map-indexed (fn [i x] [ i x ]) (get fvt columnName))
                             extended-value-table  (reduce (fn [hm entry]
                                                                 (reduce (fn [hm index] (assoc hm index (first entry))) hm (range (:from (second entry)) (inc (:to (second entry)))))
                                                                 ) {} valueColumn)]
                            (vec (map-indexed (fn[index cell] [(get extended-value-table index)
                                                              cell]) column)))) pre-consist-rrt)))




(defn- format-table
  "resolve input table format to xrel format."
  [keyorder table]
  (remove nil? (map (fn[tuple] (cond
                                   (map? tuple)     (select-keys tuple keyorder)
                                   (vector? tuple)  (zipmap keyorder tuple)
                                   (seq? tuple)     (zipmap keyorder tuple)
                                   :else nil))
        table)))



(defn tr
  "Creates a transrelational table by a collection of hash-maps with the same keys or by a
  prebuildes structure of keyorder, field value table and record reconstrution table."
  ([table]
   (let [table (distinct (vec table))
          keyorder   (keyorder-of-table table)
          permutation (create-raw-permutation table keyorder)
          fvt (create-fvt-by-raw-perm permutation)
          rrt (create-rrt permutation keyorder fvt)]
     (Transrelation. keyorder fvt rrt)))
  ([keyorder table]
    (let [table (distinct (vec (format-table keyorder table)))
          permutation (create-raw-permutation table keyorder)
          fvt (create-fvt-by-raw-perm permutation)
          rrt (create-rrt permutation keyorder fvt)]
     (Transrelation. keyorder fvt rrt)))
  ([keyorder fvt rrt]
   (Transrelation. keyorder fvt rrt)))

