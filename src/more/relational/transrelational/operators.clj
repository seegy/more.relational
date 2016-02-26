(ns more.relational.transrelational.operators
  (:use [more.relational.transrelational.table])
   (:refer-clojure :exclude [extend update max min ]))



;; #######################################################################################################################################
;; Tools
;; #######################################################################################################################################




(defn- drop-index [col idx]
  (filter identity (map-indexed #(if (not= %1 idx) %2) col)))


(defmacro get-env
  []
  (into {} (for [k (keys &env)]
             [(name k) k])))

#_(defmacro tr-fn
  "Behaves like fn, but stores the source code in the metadata to allow
  optimisation."
  [args body]
      (with-meta  (list 'fn args body)
                  {:args (list 'quote args)
                   :body (list 'quote body)
                   :env (into {}
                              (for [k (keys &env)]
                                [(name k) k]))}))

(defmacro tr-fn
  "Behaves like fn, but stores the source code in the metadata to allow
  optimisation."
  [args body]
      (with-meta  (list 'fn args body)
                  {:args (list 'quote args)
                   :body (list 'quote body)
                   :env (let [symbols (loop [l body s #{}]
                                        (do
                                          (if (coll? l)
                                            (if (empty? l)
                                              s
                                              (cond
                                               (symbol? (first l)) (recur (rest l) (conj s (name (first l))))
                                               (coll? (first l)) (recur (apply conj (rest l) (first l)) s)
                                               :else (recur (rest l) s)))
                                            (recur (list l) s))))]
                          (into {}
                              (for [k (keys &env)]
                                (if (contains? symbols (name k))
                                  [(name k) k]))))}))



#_(let [asd 20
      r (ref asd :meta {})
       people (tr #{{:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}})
      trfn (tr-fn [t] (< asd (:status t)))]
 (restriction people trfn)
  (meta trfn))



;; #######################################################################################################################################
;; Basic operations
;; #######################################################################################################################################





(defn delete
  "Deletes one related data row by the numeric position of one of it cells and returns the resulting transrelational table."
  [trans-table  row column]
  (let [attrs (let [attrs (keyorder trans-table)]
                (apply conj (vec (drop column attrs)) (drop-last (- (count attrs) column ) attrs)))
        rrt (let [ rrt (recordReconst trans-table)]
              (apply conj (vec (drop column rrt)) (drop-last  (- (count rrt) column ) rrt)))
        indizes (loop [attrs attrs
                       rrt rrt
                       row row
                       result {}]
                       (if (empty? attrs)
                         result
                         (let [ cell  (get (first rrt) row)]
                           (recur (rest attrs) (rest rrt) (second cell) (assoc result (first attrs) [(first cell) row])))))
        delete-Value (fn [attr index]
                       (let[untouched (take index (get (fieldValues trans-table) attr))
                            target (let [zwerg (merge-with - (get (get (fieldValues trans-table) attr) index) {:to 1} )]
                                     (if (< (:to zwerg ) (:from zwerg))
                                       '()
                                       [zwerg]))
                            tail (map (fn [m] (merge-with - m {:from 1 :to 1} )) (drop (inc index) (get (fieldValues trans-table) attr))) ]
                         [(into [] (concat untouched target  tail))
                          (empty? target)]))
        fvt-manipulation (map (fn [[attr [index _]]] (delete-Value attr index)) indizes)
        new-fvt (apply merge (map (fn [attr [column _]] {attr column}) attrs fvt-manipulation))
        entry-infos (let [indizes (map #(get indizes %) attrs)]
                      (mapv (fn [[_ a] [b c d ]] [c b a d])
                            (vec (concat (rest indizes) [(first indizes)]))
                            (mapv (fn [[a b] c] [a b c])  indizes (map second fvt-manipulation))))
        new-rrt (let [filtered-rrt (mapv (fn [[index _ _ _] column]
                                           (concat (take index column) (drop (inc index) column)))
                                         entry-infos rrt)
                      new-rrt (mapv (fn [[_ value-link next-link is-deleted] column]
                                     ( mapv (fn[[cell-value cell-next]]
                                            [(if (and is-deleted (> cell-value value-link))
                                               (dec cell-value)
                                               cell-value)
                                             (if (> cell-next next-link)
                                               (dec cell-next)
                                               cell-next
                                               )]) column)) entry-infos filtered-rrt)]
                  (apply conj (vec (drop (- (count rrt) column ) new-rrt)) (drop-last  column new-rrt)))]
   (tr (keyorder trans-table)  new-fvt new-rrt)))





(defn find-duplicates
  [trans-table]
  (let [column-duplicates (loop [fvt-columns (fieldValues trans-table)
                                 result []]
                            (if (empty? fvt-columns)
                              result
                              (let [column-duplicates  (filter #(not= (:from %) (:to %)) (second (first fvt-columns)))]
                                (if (empty? column-duplicates)
                                  '()
                                  (recur (rest fvt-columns) (conj result (map (fn [entry] (range (:from entry) (inc (:to entry)))) column-duplicates)))))))]
    (if (some empty? column-duplicates)
      [0 '()]
      (let [column-info  (map (fn [x] [ (.indexOf column-duplicates x) (apply + (map count  x)) x]) column-duplicates)
            few-duplicates (first (sort-by second column-info))]
        [(first few-duplicates) (flatten (map (fn [rows ] (map #(map first (rest (second %)))
                           (group-by second
                                   (map (fn[row][ row (zigzag trans-table row (first few-duplicates))]) rows)))) (last few-duplicates)))]))))



(defn distinct-tr [trans-table]
  (let [[column rows] (find-duplicates trans-table)]
    (if (empty? rows)
      trans-table
      (let [ row-set (set rows)
             unique-tuples (filter #(not (contains? row-set %))  (range (count trans-table)))]
    (tr (map #(retrieve trans-table % column) unique-tuples))))))




(defn- binary-search
  ""
  [column value]
  (loop [imin 0
         imax (dec (count column))]
    (if (< imax imin)
      -1
      (let [imid (long (/ (+ imin imax) 2))
            comp-res (compare (:value (get column imid)) value)]
        (cond
          (pos? comp-res) (recur imin (dec imid))
          (neg? comp-res) (recur (inc imid) imax)
          :else imid)))))



(defn point-search
  ""
  [trans-table attr value]
  (let [found (binary-search (get (fieldValues trans-table) attr) value)]
   (if (neg? found)
      #{}
      (let [entry (get  (get (fieldValues trans-table) attr) found)]
        (set (map #(retrieve trans-table % (.indexOf (keyorder trans-table) attr)) (range (:from entry) (inc (:to entry)))))))))





(defn- get-most-present-attr [tr]
  (let [counts (map #(first (last %)) (recordReconst tr))]
    (get (keyorder tr) (.indexOf counts (apply clojure.core/max counts)))))

(defn- tupel-in-tr
  [trans-table tupel]
  (let [mpa (get-most-present-attr trans-table)
        x (point-search trans-table mpa (get tupel mpa))]
    (contains? x tupel)))




(defn insert
  "Returns the given transrelational table with the included datarow. The datarow has to be a map."
  [trans-table data-row]
  (when-not (= (set (keys data-row)) (set (keyorder trans-table)))
    (throw (IllegalArgumentException. "DataRow has not the same schema as the table")))
  (if (tupel-in-tr trans-table data-row)
    trans-table
    (let [fvt-manipulation (reduce (fn [m [k v]](assoc m k
                                         (let[old-column (get  (fieldValues trans-table) k)
                                              untouched (filter #(neg? (compare (:value %) v)) old-column)
                                              increased (into [] (comp
                                                                   (map (fn[cell] (merge-with + cell {:from 1 :to 1})))
                                                                   (filter #(pos? (compare (:value %) v)))
                                                                   ) old-column)
                                              target (let[found (first (filter #(= (:value %) v) old-column))]
                                                       (if (nil? found)
                                                         (let [index (if (empty? untouched) 0 (inc (:to (last untouched))))]{:value v :from index :to index})
                                                         (merge-with + found {:to 1})))]
                                            [(into [] (concat untouched [target] increased)) [target (count untouched)]]))) {}  data-row )
          new-fvt (reduce (fn[m [k [ v _ ]]] (assoc m k v)) {} fvt-manipulation)
          new-rrt (let [inserts (reduce (fn[m [k [ _ v ]]] (assoc m k v)) {} fvt-manipulation)
                        inserts-in-order (map #(get inserts %) (keyorder trans-table))
                        entry-infos (mapv (fn [a b] [ (second a)
                                                      (:to (first a))
                                                      (:to (first b))
                                                      (= (:to (first a)) (:from (first a)))] )
                                          inserts-in-order (conj (vec (rest inserts-in-order)) (first inserts-in-order)))]
                    (mapv (fn[column [a-value-index a-next-pointer b-next-pointer a-is-new]]
                            (let [prepared-column (mapv (fn [[ a b ]] [ (if (and (>= a a-value-index) a-is-new) (inc a) a)
                                                                       (if (>= b b-next-pointer) (inc b) b) ]) column)]
                                                   (into [] (concat (take a-next-pointer prepared-column) [[a-value-index b-next-pointer]] (drop a-next-pointer prepared-column)))))
                          (recordReconst trans-table) entry-infos))]
       (tr (keyorder trans-table) new-fvt new-rrt))))




(defn update
  "Returns the given transrelational table with an updated data row.
  The row is specificated by the numeric position of one of it cells.
  The update is specified by a map of attributes as their keys and the new values following.

  Example: (update table 1 2 {:id 10}) to set the attribute :id of the data row relates to cell [1,2]."
  [trans-table  row column update-map]
  (when (not-any? #(contains? (set (keyorder trans-table)) %) (keys update-map))
    (throw (IllegalArgumentException. "Update map contains illegal attribute.")))
  (let [old-row (retrieve trans-table row column)
        new-row (merge old-row update-map)]
    (insert (delete trans-table row column) new-row) ))







;; #######################################################################################################################################
;; Algera operations
;; #######################################################################################################################################





(defn project
    ""
  [trans-table attrs]
  (when (not-any? #(contains? (set (keyorder trans-table)) %) attrs)
    (throw (IllegalArgumentException. "Update map contains illegal attribute.")))
  (let [to-delete (filterv #(not (contains? (set attrs) %)) (keyorder trans-table))
        new-ko (filterv #(contains? (set attrs) %) (keyorder trans-table))
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
                                     (let [columns (map #(get (recordReconst trans-table) (mod %  (count (keyorder trans-table))))
                                                        (range (inc column-index) (+ column-index steps 1)))]
                                       (loop [base (get (recordReconst trans-table) column-index)
                                             columns  columns]
                                         (if (empty? columns)
                                                           base
                                                           (recur
                                                            (mapv (fn [[a b]] [a (second (get (first columns) b))]) base)
                                                            (rest columns))))))
                      manipulated-columns (reduce (fn[m [a b] ] (assoc m a (replace-link a (mod (- b a) (count (keyorder trans-table)))))) {} merged)
                      new-rrt (let [ with-new-colums (reduce (fn[m [k v]](assoc m k v)) (recordReconst trans-table) manipulated-columns )]
                                (vec (filter #(not (contains? (set delete-indizes) (.indexOf with-new-colums %))) with-new-colums)))
                      ]
                  new-rrt)]
     (distinct-tr (tr attrs new-fvt new-rrt))))





(defn project+
    ""
  [trans-table attrs]
  (when (not-any? #(contains? (set (keyorder trans-table)) %) attrs)
    (throw (IllegalArgumentException. "Update map contains illegal attribute.")))
  (let [converted (convert trans-table)
        new-converted (distinct (map (fn [m] (select-keys m attrs)) converted))]
     (tr new-converted)))





(defn extend
  ""
  [trans-table preds]
  (let[table (convert trans-table)
       extended (map (fn [row]( reduce (fn [m [k v]] (assoc m k (if (fn? v)
                                                                  (v m)
                                                                  v))) row preds )) table)]
    (tr extended)))



(defn union
  ""
  [tr1 & more]
   (tr (keyorder tr1) (flatten (apply conj  (seq tr1) (map seq more)))))




(defn intersection
  ""
  [tr1 & more]
  (tr (keyorder tr1) (apply clojure.set/intersection (set  tr1) (map #(set  %) more))))




(defn difference
  ""
  [tr1 & more]
  (tr (keyorder tr1) (apply clojure.set/difference (set  tr1)  (map #(set  %) more))))





(def replace-map
  '{and clojure.set/intersection
    or clojure.set/union})





(defn down-to-up-scan
  ""
  [column f right-value]
  (vec (drop-while  #(not (f (:value %) right-value)) column)))



(defn up-to-down-scan
  ""
  [column f right-value]
  (loop [c column
         result []]
    (if (and (not-empty c) (f (:value (first c)) right-value))
      (recur (rest c) (conj result (first c)))
      result)))




(defn not=-scan
  ""
  [column value]
  (let [ops (if (string? value)
              [#(neg? (compare %1 %2)) #(pos? (compare %1 %2))]
              [< >])]
  (apply conj (up-to-down-scan column (first ops) value) (down-to-up-scan column (second ops) value))))



(defn area-search
  ""
  [trans-table attr f right-value]
  (let [up-to-down #{<= <}
        down-to-up #{>= >}
        column (get (fieldValues trans-table) attr)]
     (set (map #(retrieve trans-table % (.indexOf (keyorder trans-table) attr))
      (flatten (map (fn [n] (range (:from n) (inc (:to n))))
         (cond
           (contains? up-to-down f) (up-to-down-scan column f right-value)
           (contains? down-to-up f) (down-to-up-scan column f right-value)
           (= not= f) (not=-scan column right-value)
           :else [])))))))





(def flip-compare-map  {'<    '>
                        '>    '<
                        '>=   '<=
                        '<=   '>=
                        '=    '=
                        'not= 'not=})




(defn- unflat
  [ast]
  (let [op (first ast)
        args (rest ast)
        pairs (map (fn [arg1 arg2] (list op arg1 arg2)) args (rest args))
        new-ast (conj pairs 'and)]
    new-ast))




(defn- key-of-tr
  [tr-alias term]
  (cond
   (and (coll? term)
        (= 2 (count term))     ; (:something t)
        (keyword? (first term))
        (= tr-alias (second term))) (first term)
   (and (coll? term)
        (= 3 (count term))     ; (get t :something)
        (= 'get (first term))
        (= tr-alias (second term))) (last term)
   :else nil))




(defn inner-compare
  ""
  [trans-table  f left right]
  (when (not-any? #(contains? (set (keyorder trans-table)) %) [left right])
    (throw (IllegalArgumentException. "Key not exists in tr.")))
  (let [left-column (get (fieldValues trans-table) left)
        right-column (get (fieldValues trans-table) right)
        filtered-left (filter (fn[l] (loop [r right-column]
                                       (cond
                                          (empty? r) false
                                          (f (:value l) (:value (first r))) true
                                          :else (recur (rest r))))) left-column)
        filtered-right (filter (fn[r] (loop [l filtered-left]
                                        (cond
                                         (empty? l) false
                                         (f (:value (first l)) (:value r)) true
                                         :else (recur (rest l))))) right-column)
        keyorder (keyorder trans-table)
        l-index  (.indexOf keyorder  left)
        r-index  (.indexOf keyorder right)
        steps-left-to-right (mod (- r-index l-index) (count keyorder))
        steps-right-to-left (mod (- l-index r-index) (count keyorder))
        actual-numbers (fn[column] (flatten (map #(range (:from %) (inc (:to %))) column)))

        travel-check (fn[from-column from-idx from-left? to-column steps]
                       (let[start-set  (actual-numbers from-column)
                            target-set (into #{} (actual-numbers to-column))]
                         (filter (fn[row]
                                   (let[traveled (travel (recordReconst trans-table) row from-idx steps)]
                                    (and (contains? target-set (first traveled))
                                         (let[start-value (:value (first (filter #(and (<= row (:to %))
                                                                                       (<= (:from %) row))
                                                                                 from-column)))
                                              travel-value (:value (first (filter #(and (<= (first traveled) (:to %))
                                                                                        (<= (:from %) (first traveled)))
                                                                                  to-column)))]
                                         (if from-left?
                                            (f start-value travel-value )
                                            (f travel-value start-value ))))))
                                 start-set)))

        [result-column-index
         result-column-rows]  (if (< steps-left-to-right steps-right-to-left)
                                  [l-index (travel-check filtered-left l-index true filtered-right steps-left-to-right)]
                                  [r-index (travel-check filtered-right r-index false filtered-left steps-right-to-left)])
                      ]
     (reduce #(conj %1 (retrieve trans-table %2 result-column-index)) [] result-column-rows)))






(defn optimize
  ""
  [arg ast]
  (let []
    (cond
       (and (coll? ast) (contains? #{ 'and 'or} (first ast)))
            (reverse (into
              (case (first ast)
                and '(clojure.set/intersection)
                or '(clojure.set/union))
              (map #(optimize arg %) (rest ast)))),

       (and (coll? ast))
         (if
           (< 2 (count (rest ast)))
           (optimize arg (unflat ast))
           (let [key-map (map #(key-of-tr (first arg) %) (rest ast))]
             (cond
               (every? #(not (nil? %)) key-map)
                  (seq [ `inner-compare (first arg) (first ast) (first key-map) (second key-map)])

               (not-every? nil? key-map)
                (let [ f (if (last key-map)
                            (get flip-compare-map (first ast))
                            (first ast))
                      [left right] (if (last key-map)
                                     [(last key-map) (second ast)]
                                     [(first key-map) (last ast)])
                       ]
                              (cond
                                (contains? #{'< '> '<= '>=} f)
                                   (seq [`area-search (first arg) left f right])

                                (= '= f)
                                   (seq [ `point-search (first arg) left right])

                                (= 'not= f)
                                   (apply conj right left (first arg) `not=-scan)

                               :else (f left right)))

               :else '()))), ;TODO compare without tuple

      (true? ast)
          (seq [`convert  (first arg)])
      (false? ast)
          #{}
       :else ast))) ;TODO const



#_(defmacro optimize
  ""
  [arg ast]
  `(let [arg# '~arg
         ast# '~ast]
   (cond
     (and (seq? ast#) (contains? #{ "and" "or" } (str (first ast#))))
            (do
              (reverse (into
              (case (str (first ast#))
                "and" '(clojure.set/intersection)
                "or" '(clojure.set/union))
              (map #(eval (list `optimize arg# %)) (rest ast#)))))

     (and (seq? ast#))
      (if (< 2 (count (rest ast#)))
        (let [unflated# (unflat ast#)]
          (eval (list `optimize arg# unflated#)))

        (let [key-map# (map #(key-of-tr (first arg#) %) (rest ast#))]

           (cond
               (every? #(not (nil? %)) key-map#)
                  (seq [ `inner-compare (first arg#) (first ast#) (first key-map#) (second key-map#)])

                (not-every? nil? key-map#)
                (let [ [ f# left# right# ] (if (last key-map#)
                            [(get flip-compare-map (first ast#)) (last key-map#) (second ast#)]
                            [(first ast#) (first key-map#) (last ast#)])
                       right#  right#]
                              (cond
                                (contains? #{< > <= >= not=} (eval f#))
                                   (list `area-search (first arg#) left# f# right# )

                                (= = (eval f#))
                                   (list `point-search (first arg#) left# right#)

                                (= not= (eval f#))
                                   (list  `not=-scan (first arg#) left# right#)

                               :else (f# left# right#)))

               :else '()))), ;TODO compare without tuple

      (true? ast#)
          (seq [`convert  (first arg#)])
      (false? ast#)
          #{}
       :else ast#))) ;TODO const



(defmacro restrict-fn-analytic
  ""
  [args body]
  `~(let [optimized (optimize args body)]
    (with-meta (list 'quote optimized)
                {:body (list 'quote optimized)})))







(defn- pred-search
  ""
  [trans-table rfn]
  (let [pred (list 'fn (:args (meta rfn))
                  (optimize (:args (meta rfn)) (:body (meta rfn))))]
   ((binding [*ns* (the-ns 'more.relational.transrelational.operators)] (eval pred)) trans-table)))




#_(defn restriction
  ""
  [trans-table rfn]
  (let[tuples (pred-search trans-table rfn)]
      (tr (keyorder trans-table) tuples)))


(defn restriction
  ""
  [trans-table rfn]
  (let[meta-stuff (meta rfn)
       pred   (eval (list 'fn
                  (:args meta-stuff)
                  (list 'let
                               (reduce (fn[v [x y]] (conj v (read-string x) y)) [] (:env meta-stuff))
                    (optimize
                        (:args meta-stuff)
                        (:body meta-stuff)))))
        ]
       (tr (keyorder trans-table) (pred trans-table))))



(defn max
  ""
  [trans-table attr]
  (-> trans-table
      fieldValues
      (get attr)
      last
      :value))


(defn min
  ""
  [trans-table attr]
  (-> trans-table
      fieldValues
      (get attr)
      first
      :value))


(defn sum
  ""
  [trans-table attr]
  (reduce #(+ %1 (* (:value %2) (inc (- (:to %2) (:from %2))))) 0  (-> trans-table fieldValues (get attr))))





(defn join
  "Join relations r and s by their common attributes."
  [r s]
  (let [common-attrs (remove nil? (map #(some #{%} (keyorder r)) (keyorder s)))
        currently-relevant-attr (first common-attrs)
        r-pos (.indexOf (keyorder r) currently-relevant-attr)
        s-pos (.indexOf (keyorder s) currently-relevant-attr)
        match-indizes (loop [r (get (fieldValues r) currently-relevant-attr)
                             s (get (fieldValues s) currently-relevant-attr)
                             result []]
                        (if (or (empty? r) (empty? s))
                          result
                          (let [rf (first r)
                                s (drop-while #(->> % :value (compare (:value rf)) pos?)  s)
                                sf (first s)]
                            (if (= (:value rf) (:value sf))
                              (recur (rest r) (rest s) (conj result [(range (:from rf) (inc (:to rf)))
                                                                     (range (:from sf) (inc (:to sf)))]))
                              (recur (rest r) s result)))))
        r-tuples (->> match-indizes (map first) flatten set (map (fn [x] [x (retrieve r x r-pos)])) (into {}))
        s-tuples (->> match-indizes (map second) flatten set (map (fn[x] [x (retrieve s x s-pos)])) (into {}))
        pairs-to-join (reduce (fn[result [[r-index _] s-indezes]]
                         (reduce (fn[result s-index] (conj result [ (get r-tuples r-index)
                                                                    (get s-tuples s-index) ]))
                                 result s-indezes)) #{} match-indizes)
        merged (into #{} (comp (map #(apply merge %))
                                      (filter #(= (select-keys (first %) (rest common-attrs))
                                                  (select-keys (second %) (rest common-attrs)))))
                           pairs-to-join)]
    (tr merged)))


; ########################################################################################################################################################################
; ########################################################################################################################################################################

#_(
(defmacro optimize
  ""
  [arg ast]
  `(let [arg# '~arg
         ast# '~ast]
   (cond
     (and (seq? ast#) (contains? #{ "and" "or" } (str (first ast#))))
            (do
              (reverse (into
              (case (str (first ast#))
                "and" '(clojure.set/intersection)
                "or" '(clojure.set/union))
              (map #(eval (list `optimize arg# %)) (rest ast#)))))

     (and (seq? ast#))
      (if (< 2 (count (rest ast#)))
        (let [unflated# (unflat ast#)]
          (eval (list `optimize arg# unflated#)))

        (let [key-map# (map #(key-of-tr (first arg#) %) (rest ast#))]

           (cond
               (every? #(not (nil? %)) key-map#)
                  (seq [ `inner-compare (first arg#) (first ast#) (first key-map#) (second key-map#)])

                (not-every? nil? key-map#)
                (let [ [ f# left# right# ] (if (last key-map#)
                            [(get flip-compare-map (first ast#)) (last key-map#) (second ast#)]
                            [(first ast#) (first key-map#) (last ast#)])
                       right#  right#]
                              (cond
                                (contains? #{< > <= >= not=} (eval f#))
                                   (list `area-search (first arg#) left# f# right# )

                                (= = (eval f#))
                                   (list `point-search (first arg#) left# right#)

                                (= not= (eval f#))
                                   (list  `not=-scan (first arg#) left# right#)

                               :else (f# left# right#)))

               :else '()))), ;TODO compare without tuple

      (true? ast#)
          (seq [`convert  (first arg#)])
      (false? ast#)
          #{}
       :else ast#))) ;TODO const



(defmacro tr-fn
  "Behaves like fn, but stores the source code in the metadata to allow
  optimisation."
  [args body]
      (with-meta  (list 'fn args body)
                  {:args (list 'quote args)
                   :body (list 'quote body)
                   :env (list 'get-env)}))

(def people (tr #{{:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}}))

(let [asd 20]
   (meta (tr-fn [t] (<= asd (:status t)))))



(let [asd 20]
  (macroexpand-1 '(optimize [t] (= asd (:status t)))))

(let [asd 20]
  (optimize [t] (and true (= asd (:status t)))))







(defn restriction
  ""
  [trans-table rfn]
  (let[meta-stuff (meta rfn)
       pred (eval (list 'fn
                  (:args meta-stuff)
                  (list 'let
                               (reduce (fn[v [x y]] (conj v (read-string x) y)) [] (:env meta-stuff))
                  (eval (list `optimize
                        (:args meta-stuff)
                        (:body meta-stuff))))))
        ]
       (tr (keyorder trans-table) (pred trans-table))))



(let [asd 20
      trfn (tr-fn [t] (< asd (:status t)))]
  (restriction people trfn))

(let [asd 20
      trfn (tr-fn [t] (< asd (:status t)))]
 (let [trans-table people
       meta-stuff (meta trfn)
       pred (list 'fn
                  (:args meta-stuff)
                  (list 'optimize
                        (:args meta-stuff)
                        (:body meta-stuff)))]
   pred))


; ############################################
; StackOverflow




; Should work like fn, but save the code of args and body
(defmacro my-fn
  [args body]
      (with-meta  (list 'fn args body)
                  {:args (list 'quote args)
                   :body (list 'quote body)
                   :env (list 'get-env)}))


; in this macro, the predicate should be morphed later. Both args and body will be needed.
; to show my problem, i will skip this morphing and return just the body
(defmacro morph
  [args body]
  body)


; example data für this case
(def people  #{{:id "S1" :name "Smith" :status 20 }
               {:id "S2" :name "Jones" :status 10 }
               {:id "S3" :name "Blake" :status 30 }})



 (let[a 20
      f (fn [t] (<= a (:status t)))]
   (filter f people))


 (defmacro get-env
  []
  (into {} (for [k (keys &env)]
             [(name k) k])))




(let [a 20
      b 123
      f (my-fn [t] (<= a (:status t)))] ; pred should be used like fn
  (let [a 1
        d 5
        env (get-env)
        pred (eval (list 'fn    ; later this second let code for comes into a macro or somehting
                         (:args (meta f))
                         (list 'let
                               (reduce (fn[v [x y]] (conj v (read-string x) y)) [] (:env (meta f)))
                               (list 'morph
                                     (:args (meta f))
                                     (:body (meta f))))))]
   (filter pred people)))

(read-string "a")

(reduce (fn[v [x y]] (conj v (read-string x) y)) []  {"a" 20, "b" 123})
   )
