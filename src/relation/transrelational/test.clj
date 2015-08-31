(ns wayne
  (:use [relation.transrelational]
        [clojure.core.matrix]))





(defn- melt [f a b & more]
  (let [args (filter #(not (nil? %)) (conj [] a b more))]
    (when-not (= (map count args))
      (throw (IllegalArgumentException. "Data sets have not the same length.")))
    (let [melter (fn [result a b]
                   (if (empty? a)
                     result
                     (recur (conj result (f (first a) (first b))) (rest a) (rest b))))]
    (apply melter [] args))))

(defn- drop-index [col idx]
  (filter identity (map-indexed #(if (not= %1 idx) %2) col)))


(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))
people


(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
      {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
      {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }]))

people
(keyorder people)
(fieldValues people)
(recordReconst people)
(count (first (recordReconst people)))



(fieldValueOf people 0 0)
(fieldValueOf people 2 :name)



;; #######################################################################################################################################
;; OPERATIONEN
;; #######################################################################################################################################




(time (convert people))



(def testTR(tr [{:a "a1" :b "b1" :c "c1"}
     {:a "a2" :b "b1" :c "c2"}
     {:a "a3" :b "b1" :c "c1"}]))


(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
      {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
      {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }]))

(retrieve people 2 3)
(retrieve people 0 0)



(time (convert people))
(time (convert people))


(def after-insert (insert people  {:id "S11" :name "Jones" :status 20 :city "London" :gender "male" :size 21 :hair "blond" :eyes "blue" }))
after-insert



(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
      {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
      {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }]))

 (insert people  {:id "S11" :name "Jones" :status 20 :city "London" :gender "male" :size 21 :hair "blond" :eyes "blue" })




 (def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))
 (recordReconst people)

  (def toInsert {:id "S1" :name "Blake" :status 0 :city "Athens"})

 (def afterInsert (insert people toInsert))
 afterInsert
 (convert afterInsert)


(= (vec(convert afterInsert))
   [ {:id "S1" :name "Smith" :status 20 :city "London"}
     {:id "S1" :name "Blake" :status 0 :city "Athens"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}
     ])




#_(let [ people (tr [ {:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
      {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
      {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }])
      after-insert (insert people  {:id "S11" :name "Jones" :status 20 :city "London" :gender "male" :size 21 :hair "blond" :eyes "blue" })]
  (use 'clojure.data)
      (melt (fn [a b] (when-not (= a b) (diff a b)))  (convert after-insert) [ {:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
            {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }
            {:id "S11" :name "Jones" :status 20 :city "London" :gender "male" :size 21 :hair "blond" :eyes "blue" }
            {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
            {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
            {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
            {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
            {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
            {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
            {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
            {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
            ]))







(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))

(retrieve people 0 3)

(def afterdelete (delete people 1 2))
afterdelete
(convert afterdelete)


(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))

people

(delete people 1 0)
(convert (delete people 1 0))

(convert (delete people 3 1))



(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))

people

(retrieve people 0 3)

(retrieve people 1 2)

(retrieve people 0 0)
(retrieve people 1 0)
(retrieve people 0 1)

(delete people 1 0)
(convert (delete people 1 0))

(delete people 3 1)
(convert (delete people 3 1))


(delete people 1 2)
(convert (delete people 1 2))


(delete people 3 1)
(convert (delete people 3 1))




(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))


;; Should throw exception
(update people 0 0 {:sex "male"})
(update people 0 0 {:city "Berlin"})
(convert (update people 0 0 {:city "Berlin"}))

(update people 0 3 {:city "Berlin"})
(convert (update people 0 3 {:city "Berlin"}))



(update people 2 0 {:id "S7" :name "Müller" :status 0 :city "Frankfurt"})
(convert (update people 2 0 {:id "S7" :name "Müller" :status 0 :city "Frankfurt"}))







;; #######################################################################################################################################
;; Matrizen
;; #######################################################################################################################################



(use 'clojure.core.matrix.operators)
(set-current-implementation :vectorz)

(def v (transpose (array [[7 9 4 1 2 0 5 8 3 6] [6 5 2 8 0 7 4 1 3 9] [7 2 3 4 5 8 0 9 1 6] [6 8 4 1 2 3 9 0 5 7] [1 7 8 9 0 2 4 3 5 6] [0 3 1 2 4 5 6 7 8 9] [5 0 2 6 1 3 4 7 8 9] [3 5 6 8 1 0 2 4 7 9]])))
v
(transpose v)
(class v)

(slices v)
(dimensionality v)
(slice v 0)
(slice v 1 0)

(rows v)
(columns v)
(* v v)
(eseq v)

(def e (fill v 0))
e
v

(* v [2 2 2 2 2 2 2 2 ])

(def nullmatrix (fill v 1))
nullmatrix

(class (columns nullmatrix))

(emap (fn[number] (if (>= number 4) (inc number) number)) (second (columns v)))


(def transfM (array (transpose  (assoc (vec (columns nullmatrix)) 1 (fill (first (columns nullmatrix)) 100)))))
transfM

(* v transfM)

