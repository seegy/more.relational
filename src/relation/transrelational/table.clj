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



(defn fieldValueOf
  "Returns the value of the cell by given positionl. The column can be defined as number or attribute name."
  [tr row column]
  (let [columnName (if (contains? (set (keyorder tr)) column) column (get (keyorder tr) column))
        xf (comp #(:value %) #(get % row) #(get % columnName))]
     (xf (fieldValues tr))))



(defn- keyorder-of-table
  "creates vector of all keys in the table."
  [table]
  (into [] (distinct (flatten (map keys table)))))



(defn- create-raw-permutation
  "Creates a map of groups of attributes and their index of the origin data row."
  [table keyorder]
  (let [recordtuples (map (fn[record] (into {}
                                            (map (fn[k]
                                                   (let[v (get record k)]
                                                     [k [v (.indexOf table record)]]))
                                            keyorder))) table)]
    (into {} (map (fn[head] [head (sort-by first(map #(get % head) recordtuples))])) keyorder)))



(defn- create-fvt-by-raw-perm
  "Creates the field value table by the permutation map."
  [permutation]
  (into {} (map (fn[[k v]] [ k (let [ values (map first v)]
                                              (into [] (comp
                                                           (distinct)
                                                           (map (fn[v] {:value v
                                                                        :from (.indexOf values v)
                                                                        :to (.lastIndexOf values v)})))
                                                        values))]) permutation)))


(defn- create-rrt
  "Creates the record reconstruction table by the permutation map, predefined key order and the field value table."
  [permutation keyorder fvt]
  (let [perm  (into {} (map (fn[[k v]] [k (map second v)]) permutation))
        zigzagTo (fn [[a b]] (mapv (fn[recnr] (.indexOf b recnr)) a))
        fromPerm (mapv (fn[x](get perm x)) keyorder)
        toPerm (conj (vec (rest fromPerm)) (first fromPerm))
        mergePerms (loop [m []
                          from fromPerm
                          to toPerm]
                     (if (empty? from)
                       m
                       (let [ffrom (first from)
                             fto (first to)]
                         (recur (conj m [ffrom fto]) (rest from) (rest to)))))
        pre-consist-rrt (mapv (fn[a b] [b (zigzagTo a)]) mergePerms (range (count mergePerms)))]
                (mapv (fn [[index column]]
                       (let[columnName (nth keyorder index)]
                                (mapv (fn[cell] [(let[valueColumn (get fvt columnName)
                                                     index (.indexOf column cell)]
                                                     (.indexOf valueColumn
                                                           (first (filter #(and (<= index (:to %)) (>= index (:from %))) valueColumn))))
                                                cell]) column))) pre-consist-rrt)))




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
          keyorder  (keyorder-of-table table)
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


#_(

(def employees-data (take 10000 (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" )))))
(def xrel (map #(zipmap [:emp_no :birth_date :first_name :last_name :gender :hire_date] %) employees-data))
(def tr-employees (tr xrel))

 (seq tr-employees)
 (set tr-employees)

(defn- create-raw-permutation
  "Creates a map of groups of attributes and their index of the origin data row."
  [table keyorder]
  (let [recordtuples (map (fn[record] (into {}
                                            (map (fn[k]
                                                   (let[v (get record k)]
                                                     [k [v (.indexOf table record)]]))
                                            keyorder))) table)]
    (into {} (map (fn[head] [head (sort-by first(map #(get % head) recordtuples))])) keyorder)))

(def keyorder (keyorder-of-table xrel))

(time (create-raw-permutation xrel keyorder))


)
