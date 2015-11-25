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





;##################### Restriktions-Problem

;siehe operators.clj
