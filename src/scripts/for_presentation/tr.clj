(ns example.tr
  (:use [relation.transrelational]))


;##################### Relationen


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

p

(convert p)



(def demo-data [{:name "Blake" :status 10}
                {:name "Clark" :status 20}
                {:name "Adams" :status 30}])






;#####################  Operation


(time (insert p {:id "S11" :name "Jackson"  :status 20  :city "New York" :gender "female"  }))

(defn alter-insert
  [table tupel]
  (let [converted   (convert table)
        manipulated (conj converted tupel)]
    (tr manipulated)))

(time (alter-insert p {:id "S11" :name "Jackson"  :status 20  :city "New York" :gender "female"  }))








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

;(time (insert some-tupel {:id "S11" :name "Jackson"  :status 20  :city "New York" :gender "female"  }))
;(time (alter-insert some-tupel {:id "S11" :name "Jackson"  :status 20  :city "New York" :gender "female"  }))

;(count (time (insert many-tupel {:emp_no 123123 :birth_date "1953-09-26"  :first_name "Irena" :last_name "Unno" :gender "M" :hire_date "1985-06-07"})))
;(time (alter-insert many-tupel {:emp_no 123123 :birth_date "1953-09-26"  :first_name "Irena" :last_name "Unno" :gender "M" :hire_date "1985-06-07"}))




(defn alter-project [trans-rel attrs]
  (let [converted (convert trans-rel)
        filtered (map #(select-keys % attrs)  converted)]
    (tr filtered)))


(time (project some-tupel [  :name :id ]))
(time (alter-project some-tupel [ :id :name ]))


(time (project  many-tupel [:first_name :gender]))
(time (alter-project  many-tupel [:first_name :gender]))


;##################### Restriktions-Problem

;siehe operators.clj
