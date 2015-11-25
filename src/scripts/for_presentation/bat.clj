(ns example.bat
  (:use [relation.bat]))


;##################### BATs


(bat \a \b \c)

(bat [{:head "Foo",  :tail "Bar"}])


(def p (convertToBats #{{:id "S1"  :name "Smith"    :status 20  :city "London"   :gender "male"  }
                        {:id "S2"  :name "Jones"    :status 10  :city "Paris"    :gender "female"}
                        {:id "S3"  :name "Blake"    :status 30  :city "Paris"    :gender "male"  }
                        {:id "S4"  :name "Clark"    :status 20  :city "London"   :gender "female"}
                        {:id "S5"  :name "Adams"    :status 30  :city "Athens"   :gender "male"  }
                        {:id "S6"  :name "Miller"   :status 30  :city "Paris"    :gender "male"  }
                        {:id "S7"  :name "Thomas"   :status 20  :city "London"   :gender "female"}
                        {:id "S8"  :name "Enderson" :status 30  :city "Athens"   :gender "male"  }
                        {:id "S9"  :name "Simpson"  :status 20  :city "London"   :gender "female"}
                        {:id "S10" :name "Woods"    :status 30  :city "New York" :gender "male"  }}))

(:name p)

(:city p)

 (buns (reverse (:status p)))

;##################### Operationen

(reverse (:id p))

(mirror (reverse (:id p)))

(join (reverse (:city p)) (:gender p) =)





;##################### SQL Beispiel

; SELECT :name, :city AS "Home", :status
; WHERE :city != "London" AND :status > 10
; ORDER BY :name


(def not_London_OIDs (mirror (select (:city p) not= "London")))

(def bigger_10_OIDs (mirror (select (:status p) > 10)))

(def joined_conds (join not_London_OIDs bigger_10_OIDs =))

(def filtered_names (join joined_conds (:name p) = ))

(makeTable [:name] [:name "Home" :status] filtered_names (:city p) (:status p))
