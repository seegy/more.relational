(ns relation.bat.bat)



(defn makeTable [orderseq keySeq & columns]
  ""
  (let [idsOfFirst (vec (map (fn [bun] (dissoc bun :tail)) (buns (first columns))))
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
                              (recur newTable (drop 1 keySeq) (drop 1 columns)))))]
    (sort-by (apply juxt orderseq )(recurMakeTable idsOfFirst keySeq columns))))

