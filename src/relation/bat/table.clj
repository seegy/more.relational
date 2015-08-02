(ns relation.bat.table)


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
  ([tuple-vec]
   (let [data (into #{} (comp
                          (filter #(= 2 (count %))))  (seq tuple-vec))]
     (BAT. data)))
  ([one & more]
   (let [tails (map #(assoc {} :tail %) (conj (seq more) one))
         heads (map #(assoc {} :head %) (take (count tails) (range)))
         both  (set (map (fn [pair] (apply merge pair)) (map vector heads tails)))]
     (BAT. both))))



(defn buns [bat]
  (.buns bat))

(defmethod print-method BAT
  [rel writer]
  (.write writer (str "#BAT " (pr-str  (buns rel)) )))

(defn bat?
  [x]
 (= (type x) relation.bat.table.BAT))



(defn makeTable
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
  ""
  [table]
  (let [heads (reduce (fn [heads entry](apply conj heads (keys entry))) #{} table)
        table (vec table)]
     (reduce (fn [m attr] (assoc m attr (bat (filter #(not (nil? (:tail %))) (map (fn [entry] {:head (.indexOf table entry), :tail (get entry attr)}) table))))) {} heads)))


