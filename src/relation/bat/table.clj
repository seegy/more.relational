(ns relation.bat.bat)




(deftype BAT [buns])


(into [] (comp
          (filter #(= 2 (count %)))
          (filter #(not (nil? (:tail %))))
          (filter #(not (nil? (:head %))))) names)

(defn bat
  ([tuple-vec]
   (let [data (into [] (comp
          (filter #(= 2 (count %)))
          (filter #(not (nil? (:tail %))))
          (filter #(not (nil? (:head %))))) tuple-vec)]
     (BAT. (sort-by #(:head %) data)))))

(defn buns [bat]
  (.buns bat))

(defmethod print-method BAT
  [rel writer]
  (.write writer (str "#BAT " (pr-str  (buns rel)) )))

