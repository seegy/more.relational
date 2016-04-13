(ns more.relational.bat.table)


(defn- hashCalcHelper
  ([a b] (+ (* 31 a) b))
  ([a b & more] (apply hashCalcHelper (hashCalcHelper a b) more)))



(deftype BAT [buns]

  clojure.lang.Seqable
  (seq [this]
       ; just make a sequence
       (seq (.buns this)))

  clojure.lang.Counted
  (count [this]
         (count (.buns this)))

  Object
  (hashCode [this]
            (let [hashs (conj (map hash (.buns this)) 17)]
              (apply hashCalcHelper  hashs))))




(defn bat
  "Creates a BAT by a collections or a bunch of BUNS "
  ([tuple-vec]
   (if (coll? tuple-vec)
     (let [data (into #{} (comp
                            (filter #( or (= (keys %) '(:head :tail)) (= (keys %) '(:tail :head))))
                            (filter map?))
                      tuple-vec)]
       (BAT. data))
     (BAT. #{})))
  ([one & more]
   (let [tails (map #(assoc {} :tail %) (conj (seq more) one))
         heads (map #(assoc {} :head %) (rest (take (inc (count tails)) (range))))
         both  (set (map (fn [pair] (apply merge pair)) (map vector heads tails)))]
     (BAT. both))))



(defn buns [bat]
  (.buns bat))



(defmethod print-method BAT
  [rel writer]
  (.write writer (apply str "#BAT \n"
                        (map #(str (:head %) " - " (:tail %) "\n") (buns rel)))))



(defn bat?
  "Returns true, if x is a BAT."
  [x]
  (= (type x) more.relational.bat.table.BAT))




(defn makeTable
  "Returns a xrel format by a sequence of attributes and collection or a bunch of BATs. Tuples are orderd by orderseq."
  ([orderseq keySeq columns]
   (let [colums (vec columns)
         idsOfFirst (vec (map (fn [bun] (dissoc bun :tail)) (first columns)))
         recurMakeTable (fn [table keySeq columns]
                          (if (or (empty? keySeq)
                                  (empty? columns))
                            (map (fn [row](dissoc row :head)) table)
                            (let [firstKey (first keySeq)
                                  firstColumn (map (fn[bun](clojure.set/rename-keys bun {:tail firstKey})) (first columns))
                                  ks #{:head}
                                  heads (clojure.set/index table ks)
                                  newTable (reduce (fn [ret x]
                                                     (let [found (heads (select-keys x ks))]
                                                       (if found
                                                         (reduce #(conj %1 (merge %2 x)) ret found)
                                                         ret)))
                                                   [] firstColumn)]
                              (recur newTable (drop 1 keySeq) (drop 1 columns)))))
         orderseq (if (empty? orderseq)
                    keySeq
                    orderseq)]
     (sort-by (apply juxt orderseq )(recurMakeTable idsOfFirst keySeq columns))))
  ([orderSeq keySeq one & moreColumns]
   (makeTable orderSeq keySeq (conj (seq moreColumns) one))))



(defn convertToBats
  "Creates a map of BATs by a given xrel table."
  ([table]
   (let [heads (reduce (fn [heads entry](apply conj heads (keys entry))) #{} table)]
     (convertToBats heads table)))
  ([heads table]
   (let [head-attr (loop [head-attr :head]
                     (if (contains? (set heads) head-attr)
                       (recur (keyword (gensym "head")))
                       head-attr))

         table (map-indexed (fn[i t] (assoc t head-attr i)) table)]
     (into {} (map (fn [ k]
                     [k (BAT. (clojure.set/rename
                                (into #{} (map #(select-keys % [head-attr k]) table))
                                (if (= head-attr :head)
                                  {k :tail}
                                  {k :tail, head-attr :head})))])
                   heads)))))












