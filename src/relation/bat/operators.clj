(ns relation.bat.bat)





(def names [{:head 1 :tail "John"}
            {:head 2 :tail "Jane"}
            {:head 3 :tail "Bob"}])

(def postal-codes [{:head 1 :tail "123"}
                 {:head 2 :tail "456"}
                 {:head 3 :tail "789"} ])

(def date-of-birth [{:head 1 :tail "01-01-2001"}
                    {:head 2 :tail "02-02-2001"}
                    {:head 3 :tail "03-03-2001"}])



(def named-tables {:name (bat names)
                   :postal-code (bat postal-codes)
                   :date-of-birth (bat date-of-birth)})
 (bat names)
 (bat postal-codes)
 (bat date-of-birth)

 (def nameBAT (bat names))

 (defn find [bat head]
   (:tail (first (filter #(= head (:head %)) (buns bat)))))


 (find nameBAT 2)


 (defn select [bat v1 & {:keys [v2 r1 r2] :or {v2 v1, r1 true, r2 r1}}]
   (into [] (comp
             (filter (true)))
         (buns bat)))

 (select nameBAT :bla :r1 false :r2 true)
