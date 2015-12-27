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
(try
  (update people 0 0 {:sex "male"})
  (catch IllegalArgumentException e "Catched assert exception."))
(update people 0 0 {:city "Berlin"})
(convert (update people 0 0 {:city "Berlin"}))

(update people 0 3 {:city "Berlin"})
(convert (update people 0 3 {:city "Berlin"}))



(update people 2 0 {:id "S7" :name "Müller" :status 0 :city "Frankfurt"})
(convert (update people 2 0 {:id "S7" :name "Müller" :status 0 :city "Frankfurt"}))

(convert people [:name :id])
(convert people [])
(convert people [:city :id])
(convert people [:city :name])





(def people (tr [ {:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}]))

people



(def some-tupel (tr #{{:id "S1"  :name "Smith"    :status 20  :city "London"   :gender "male"  }
            {:id "S2"  :name "Jones"    :status 10  :city "Paris"    :gender "female"}
            {:id "S3"  :name "Blake"    :status 30  :city "Paris"    :gender "male"  }
            {:id "S4"  :name "Clark"    :status 20  :city "London"   :gender "female"}
            {:id "S5"  :name "Adams"    :status 30  :city "Athens"   :gender "male"  }
            {:id "S6"  :name "Miller"   :status 30  :city "Paris"    :gender "male"  }
            {:id "S7"  :name "Thomas"   :status 20  :city "London"   :gender "female"}
            {:id "S8"  :name "Enderson" :status 30  :city "Athens"   :gender "male"  }
            {:id "S9"  :name "Simpson"  :status 20  :city "London"   :gender "female"}
            {:id "S10" :name "Woods"    :status 30  :city "New York" :gender "male"  }}))
(def employees-data (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" ))))
(def many-tupel (tr (map #(zipmap [:emp_no :birth_date :first_name :last_name :gender :hire_date] %) (take 10000 employees-data))))

(time (project some-tupel [  :name :status]))
(time  (project many-tupel [ :gender]))


(convert (project many-tupel [:emp_no :birth_date :gender]))


(time (project people [ :id :name ]))

(time (project people [:status :city ]))
(time (project+ people [:status :city ]))

(time (project people [:id :name :city]))
(project people [ :city :name])
(project people [ :name :city ])
 (project people [ :city :name])

(convert(insert (project people [ :id :city :name]) {:id "GASD" :name "GGGGG"  :city "44444"}))
(convert(delete (project people [ :id :city :name]) 1 0))

(convert (time (project people [:name :city ])))
 (time (project+ people [:name :city ]))

(convert(time (project people [ :city :name])))
(convert (time (project+ people [ :city :name])))


(convert (project (extend people {"backwardsName"
                                  (tr-fn [t] (clojure.string/reverse (:name t)))}) [:name "backwardsName"]))



(def p1 (tr [ {:id "S1" :name "Smith"}
      {:id "S2" :name "Jones" :city "Paris"}
      {:id "S3" :name "Blake" :status 30 }
      {:id "S4" :status 20 }
      {:id "S5"}]))

(def p2 (tr [
             {:id "S3" :name "Blake" :status 30 }
             {:id "S6" :name "Smith" :status 20 :city "London"}
      {:id "S7" :name "Jones" :status 10 :city "Paris"}
      {:id "S8" :name "Blake" :status 30 :city "Paris"}
      {:id "S9" :name "Clark" :status 20 :city "London"}
      {:id "S10" :name "Adams" :status 30 :city "Athens"}]))

(time (convert (union p1 p2)))
(union p1 p2 p3)

(intersection p1 p2)




(def p1 (tr [ {:id "S1" :name "Smith"}
      {:id "S2" :name "Jones" :city "Paris"}
      {:id "S3" :name "Blake" :status 30 }
      {:id "S4" :status 20 }
      {:id "S5"}]))

(def p2 (tr [
             {:id "S3" :name "Blake" :status 30 }
             {:id "S6" :name "Smith" :status 20 :city "London"}
      {:id "S7" :name "Jones" :status 10 :city "Paris"}
     ]))


(def p3 (tr [ {:id "S8" :name "Blake" :status 30 :city "Paris"}
      {:id "S9" :name "Clark" :status 20 :city "London"}
      {:id "S10" :name "Adams" :status 30 :city "Athens"}
              {:id "S3" :name "Blake" :status 30 }]))


(intersection p1 p2 p3)


(def p1 (tr [ {:id "S1" :name "Smith"}
      {:id "S2" :name "Jones" :city "Paris"}
      {:id "S3" :name "Blake" :status 30 }
      {:id "S4" :status 20 }
      {:id "S5"}]))

(def p2 (tr [
             {:id "S3" :name "Blake" :status 30 }
             {:id "S6" :name "Smith" :status 20 :city "London"}
      {:id "S7" :name "Jones" :status 10 :city "Paris"}
     ]))


(def p3 (tr [ {:id "S8" :name "Blake" :status 30 :city "Paris"}
      {:id "S9" :name "Clark" :status 20 :city "London"}
      {:id "S10" :name "Adams" :status 30 :city "Athens"}]))

(union p1 p2 p3)

(intersection p1 p2 p3)


(difference p1 p2 p3)


(tr-fn [t] (and (>= (:status t) 30) (= (:city t) "Paris") (= (:name t) (:city t))))

(def p (tr #{{:id "S1"  :name "Smith"    :status 20  :city "London"   :gender "male"  }
            {:id "S2"  :name "Jones"    :status 10  :city "Paris"    :gender "female"}
            {:id "S3"  :name "Blake"    :status 30  :city "Paris"    :gender "male"  }
            {:id "S4"  :name "Clark"    :status 20  :city "London"   :gender "female"}
            {:id "S5"  :name "Adams"    :status 30  :city "Athens"   :gender "male"  }
            {:id "S6"  :name "Miller"   :status 30  :city "Paris"    :gender "male"  }
            {:id "S7"  :name "Thomas"   :status 20  :city "London"   :gender "female"}
            {:id "S8"  :name "Enderson" :status 30  :city "Athens"   :gender "male"  }
            {:id "S9"  :name "Simpson"  :status 20  :city "London"   :gender "female"}
            {:id "S10" :name "Woods"    :status 30  :city "New York" :gender "male"  }}))
(tupel-in-tr p  {:id "S2"  :name "Jones"    :status 10  :city "Paris"    :gender "female"})
(tupel-in-tr p  {:id "S2"  :name "Smith"    :status 30  :city "Lomdon"    :gender "male"})

(def employees-data (take 10000 (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" )))))


(def xrel (map #(zipmap [:emp_no :birth_date :first_name :last_name :gender :hire_date] %) employees-data))

(time (def employees (tr xrel)))


(time (tupel-in-tr-rec employees {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""}))
(time (tupel-in-tr-rec employees {:emp_no 438441, :birth_date "1952-07-16", :first_name "Kiyokazu", :last_name "Mahmud", :gender "M", :hire_date "1985-12-15"}))

(time (tupel-in-tr-not-rec employees {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""}))
(time (tupel-in-tr-not-rec employees { :gender "M", :hire_date "1985-12-15", :emp_no 438441, :birth_date "1952-07-16", :first_name "Kiyokazu", :last_name "Mahmud",}))


(def p (tr #{{:id "S1"  :name "Smith"    :status 20  :city "London"   :gender "male"  }
            {:id "S2"  :name "Jones"    :status 10  :city "Paris"    :gender "female"}
            {:id "S3"  :name "Blake"    :status 30  :city "Paris"    :gender "male"  }
            {:id "S4"  :name "Clark"    :status 20  :city "London"   :gender "female"}
            {:id "S5"  :name "Adams"    :status 30  :city "Athens"   :gender "male"  }
            {:id "S6"  :name "Miller"   :status 30  :city "Paris"    :gender "male"  }
            {:id "S7"  :name "Thomas"   :status 20  :city "London"   :gender "female"}
            {:id "S8"  :name "Enderson" :status 30  :city "Athens"   :gender "male"  }
            {:id "S9"  :name "Simpson"  :status 20  :city "London"   :gender "female"}
            {:id "S10" :name "Woods"    :status 30  :city "New York" :gender "male"  }}))
(time (tupel-in-tr p  {:id "S2"  :name "Jones"    :status 10  :city "Paris"    :gender "female"}))
(time (tupel-in-tr p  {:id "S2"  :name "Smith"    :status 30  :city "London"    :gender "male"}))
(time (tupel-in-tr p  {:status 30  :city "London" }))

