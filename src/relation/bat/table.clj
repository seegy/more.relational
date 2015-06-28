(ns relation.bat.bat)




(deftype BAT [buns]

  clojure.lang.Seqable
  (seq [this]
      ; just make a sequence
      (seq (.buns this)))

  clojure.lang.Counted
  (count [this]
    (count (.buns this))))


(defn bat
  ([tuple-vec]
   (let [data (into [] (comp
          (filter #(= 2 (count %)))
         ; (filter #(not (nil? (:tail %))))
         ; (filter #(not (nil? (:head %))))
                        ) tuple-vec)]
     (BAT. data)))
  ([one & more]
   (let [tails (map #(assoc {} :tail %) (conj (seq more) one))
         heads (map #(assoc {} :head %) (take (count tails) (range)))
         both (map (fn [pair] (apply merge pair)) (map vector heads tails))]
     (BAT. both))))


(defn buns [bat]
  (.buns bat))

(defmethod print-method BAT
  [rel writer]
  (.write writer (str "#BAT " (pr-str  (buns rel)) )))

