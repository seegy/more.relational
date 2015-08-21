(ns wayne
  (:use [relation.transrelational]
        [clojure.core.matrix]))



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




(time (convert people))







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









  (def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))

  (def toInsert {:id "S6" :name "Miller" :status 20 :city "Berlin"})

  (def afterInsert (insert people toInsert))
  afterInsert

  (convert afterInsert)







(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))

(retrieve people 0 3)

(def afterdelete (delete people 1 2))
afterdelete
(convert afterdelete)







; #####################################################################################################################################################################


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

