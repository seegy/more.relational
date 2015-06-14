(ns relation.bat.bat)





(def names [{:head 1 :tail "John"}
            {:head 2 :tail "Jane"}
            {:head 3 :tail "Bob"}])

(def postal-codes [{:head 1 :tail 123}
                 {:head 2 :tail 456}
                 {:head 3 :tail 789} ])

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
 (def NameRelationBAT (bat [{:head 1 :tail 2}
                           {:head 1 :tail 3}
                           {:head 2 :tail 3}
                           {:head 3 :tail 2} ]))

 (defn find
   [batObj head]
   (:tail (first (filter #(= head (:head %)) (buns batObj)))))


 (find nameBAT 2)


 (defn select
   [batObj v1 & {:keys [v2 r1 r2] :or {v2 v1, r1 true, r2 r1}}]
  (let [< (fn[a b] (neg? (compare a b)))
        > (fn[a b] (pos? (compare a b)))
        newBat (into []
             (filter #(let [b (:tail %)]
                       (or
                         (and (or (nil? v1) (< v1 b))
                              (or (nil? v2) (< b v2))
                              (not (and (nil? v1) (nil? v2))))
                         (and r1 (= v1 b))
                         (and r2 (= v2 b))
                         (and (nil? v1) r1 (nil? v2) r2 (nil? b))))
                     (buns batObj)))]
    (bat newBat)))

; (select nameBAT :bla :r1 false :r2 true)
(select nameBAT "Jane" :v2 "John" :r1 false)
(select (bat postal-codes) 123 :v2 789 :r1 false :r2 true)




(defn join
"(bat[H1,T1] AB, bat[T1,T2] CD,str f, ···pi···)"
  [batObjAB batObjCD]
   (bat (into [] (map (fn [tupleAB]
                   (first (filter not-empty (map (fn [tupleCD]
                         (if (= (:tail tupleAB) (:head tupleCD))
                           {:head (:head tupleAB)
                            :tail (:tail tupleCD)}))
                       (buns batObjCD)))))
                  (buns batObjAB)))))


(join  NameRelationBAT nameBAT)




(defn reverse
  [batObj]
  (bat (map (fn [tuple] {:head (:tail tuple)
                         :tail (:head tuple)}) (buns batObj))))


(reverse nameBAT)




(defn mirror
  [batObj]
  (bat (map (fn [tuple] {:head (:head tuple)
                         :tail (:head tuple)}) (buns batObj))))


(mirror nameBAT)
(mirror (reverse nameBAT))



(defn mark
  [batObj & {:keys [o] :or {o 0}}]
  (bat (map (fn [tuple] {:head (:head tuple)
                         :tail (dec (+ o (:head tuple)))}) (buns batObj))))

(mark nameBAT :o 10)
(mark nameBAT)



(defn project
  [batObj c]
  (bat (map (fn [tuple] {:head (:head tuple)
                         :tail c}) (buns batObj))))

(project nameBAT "Test")



(defn slice
  [batObj lo hi]
  (bat (into [] (comp (take (+ lo hi))
                      (drop lo ))(buns batObj))))

(slice nameBAT 1 1)
(slice nameBAT 0 0)
(slice nameBAT 2 1)
(slice nameBAT 0 2)


(count nameBAT)


