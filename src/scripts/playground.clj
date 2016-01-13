(ns example
  ;(:use [relation.transrelational :as transRel])
  ;(:use [relation.bat :as batRel])
  ;(:use [relation.hashRel :as hashRel])
  (:require relation.transrelational)
  (:require relation.bat )
  (:require relation.hashRel))

(def p #{{:id "S1" :name "Smith" :status 20 :city "London" :gender "male" :size 10 :hair "black" :eyes "brown" }
      {:id "S2" :name "Jones" :status 10 :city "Paris" :gender "female" :size 10 :hair "blond" :eyes "brown" }
      {:id "S3" :name "Blake" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S4" :name "Clark" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S5" :name "Adams" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S6" :name "Miller" :status 30 :city "Paris" :gender "male" :size 20 :hair "black" :eyes "blue" }
      {:id "S7" :name "Thomas" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S8" :name "Enderson" :status 30 :city "Athens" :gender "male" :size 30 :hair "blond" :eyes "blue" }
      {:id "S9" :name "Simpson" :status 20 :city "London" :gender "female" :size 40 :hair "red" :eyes "green" }
      {:id "S10" :name "Woods" :status 30 :city "New York" :gender "male" :size 30 :hair "blond" :eyes "blue" }})


(clojure.set/index p #{:gender })



(get (clojure.set/index p #{:gender :eyes}) {:eyes "green", :gender "female"})



  {:a 1, :b 2, :c 3}

  (def v {:a 1, :b 2, :c 3})

  (assoc v :d 4)

  v





  [:sno :sname :status :scity]
  #{["S1" "Smith" 20 "London"]
    ["S2" "Jones" 10 "Paris" ]
    ["S3" "Blake" 30 "Paris" ]
    ["S4" "Clark" 20 "London"]
    ["S5" "Adams" 30 "Athens"]}




  #{{:sno "S1" :sname "Smith" :status 20 :scity "London"}
    {:sno "S2" :sname "Jones" :status 10 :scity "Paris" }
    {:sno "S3" :sname "Blake" :status 30 :scity "Paris" }
    {:sno "S4" :sname "Clark" :status 20 :scity "London"}
    {:sno "S5" :sname "Adams" :status 30 :scity "Athens"}}




  (def p  #{{:id "S1"  :name "Smith"    :status 20  :city "London"   :gender "male"  }
            {:id "S2"  :name "Jones"    :status 10  :city "Paris"    :gender "female"}
            {:id "S3"  :name "Blake"    :status 30  :city "Paris"    :gender "male"  }
            {:id "S4"  :name "Clark"    :status 20  :city "London"   :gender "female"}
            {:id "S5"  :name "Adams"    :status 30  :city "Athens"   :gender "male"  }
            {:id "S6"  :name "Miller"   :status 30  :city "Paris"    :gender "male"  }
            {:id "S7"  :name "Thomas"   :status 20  :city "London"   :gender "female"}
            {:id "S8"  :name "Enderson" :status 30  :city "Athens"   :gender "male"  }
            {:id "S9"  :name "Simpson"  :status 20  :city "London"   :gender "female"}
            {:id "S10" :name "Woods"    :status 30  :city "New York" :gender "male"  }})






(clojure.set/index p #{:gender})





{{:gender "male"} #{{:id "S1",  :name "Smith",    :status 20, :city "London",   :gender "male"}
                    {:id "S5",  :name "Adams",    :status 30, :city "Athens",   :gender "male"}
                    {:id "S6",  :name "Miller",   :status 30, :city "Paris",    :gender "male"}
                    {:id "S3",  :name "Blake",    :status 30, :city "Paris",    :gender "male"}
                    {:id "S8",  :name "Enderson", :status 30, :city "Athens",   :gender "male"}
                    {:id "S10", :name "Woods",    :status 30, :city "New York", :gender "male"}},

 {:gender "female"} #{{:id "S2", :name "Jones",   :status 10, :city "Paris",    :gender "female"}
                      {:id "S4", :name "Clark",   :status 20, :city "London",   :gender "female"}
                      {:id "S7", :name "Thomas",  :status 20, :city "London",   :gender "female"}
                      {:id "S9", :name "Simpson", :status 20, :city "London",   :gender "female"}}}




  #{{:head 7,  :tail "London"  }
    {:head 5,  :tail "Athens"  }
    {:head 9,  :tail "London"  }
    {:head 10, :tail "New York"}
    {:head 6,  :tail "Paris"   }
    {:head 8,  :tail "London"  }
    {:head 3,  :tail "Paris"   }
    {:head 4,  :tail "Paris"   }
    {:head 2,  :tail "Athens"  }
    {:head 1,  :tail "London"  }}





  #{{:head 2,  :tail 30}
    {:head 6,  :tail 10}
    {:head 3,  :tail 30}
    {:head 1,  :tail 20}
    {:head 10, :tail 30}
    {:head 9,  :tail 20}
    {:head 7,  :tail 20}
    {:head 4,  :tail 30}
    {:head 5,  :tail 30}
    {:head 8,  :tail 20}}





   (clojure.set/index  #{{:head 20, :tail 8}
                        {:head 10, :tail 6}
                        {:head 20, :tail 9}
                        {:head 30, :tail 4}
                        {:head 20, :tail 1}
                        {:head 30, :tail 3}
                        {:head 30, :tail 10}
                        {:head 20, :tail 7}
                        {:head 30, :tail 2}
                        {:head 30, :tail 5}}
                       #{:head})




  (clojure.set/index   #{{:head 2, :tail 30}
                          {:head 6, :tail 10}
                          {:head 3, :tail 30}
                          {:head 1, :tail 20}
                          {:head 10, :tail 30}
                          {:head 9, :tail 20}
                          {:head 7, :tail 20}
                          {:head 4, :tail 30}
                          {:head 5, :tail 30}
                          {:head 8, :tail 20}}
                        #{:tail})





                           {{:tail 30}
   #{{:head 2,  :tail 30}
     {:head 3,  :tail 30}
     {:head 10, :tail 30}
     {:head 4,  :tail 30}
     {:head 5,  :tail 30}},
                            {:tail 10}
   #{{:head 6,  :tail 10}},
                            {:tail 20}
   #{{:head 1,  :tail 20}
     {:head 9,  :tail 20}
     {:head 7,  :tail 20}
     {:head 8,  :tail 20}}}






  {{:head 20}  #{{:head 20, :tail 8}
                 {:head 20, :tail 9}
                 {:head 20, :tail 1}
                 {:head 20, :tail 7}},
   {:head 10}  #{{:head 10, :tail 6}},
   {:head 30}  #{{:head 30, :tail 4}
                 {:head 30, :tail 3}
                 {:head 30, :tail 10}
                 {:head 30, :tail 2}
                 {:head 30, :tail 5}}}





(def employees-data (take 10000 (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" )))))


(def xrel (map #(zipmap [:emp_no :birth_date :first_name :last_name :gender :hire_date] %) employees-data))

(time (def transrel-employees (transRel/tr xrel)))


(time (transRel/insert transrel-employees  {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""}))

(time (transRel/insert transrel-employees  {:emp_no 99999999999, :birth_date "zzzzzzzzzzzzzzzzzzzzzzzzzz", :first_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :last_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :gender "zzzzzzzzzzzzzzzzzzzzzzzzzz", :hire_date "zzzzzzzzzzzzzzzzzzzzzzzzzz"}))


(defn alter-insert
  [table tupel]
  (let [converted   (transRel/convert table)
        manipulated (conj converted tupel)]
    (transRel/tr manipulated)))


(time (alter-insert transrel-employees  {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""}))

(time (alter-insert transrel-employees  {:emp_no 99999999999, :birth_date "zzzzzzzzzzzzzzzzzzzzzzzzzz", :first_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :last_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :gender "zzzzzzzzzzzzzzzzzzzzzzzzzz", :hire_date "zzzzzzzzzzzzzzzzzzzzzzzzzz"}))






(def employees-data (take 10000 (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" )))))
(def xrel (map #(zipmap [:emp_no :birth_date :first_name :last_name :gender :hire_date] %) employees-data))


(time (def hashRel-employees (relation.hashRel/rel xrel)))
(time (def bat-employees (relation.bat/convertToBats xrel)))




; #################### Punktsuche


(time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (= (:emp_no t) 485652 ))))

(time (let [id (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:emp_no bat-employees) = 485652)) (:emp_no bat-employees) =))]
        (apply relation.bat/makeTable [] [:emp_no :birth_date :first_name :last_name :gender :hire_date] id (vals (dissoc bat-employees :emp_no)))))




;################# Bereichssuche


(count (time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (= (:gender t) "F" )))))

(time (let [females (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:gender bat-employees) = "F")) (:gender bat-employees) =))]
        (apply relation.bat/makeTable [] [:emp_no :birth_date :first_name :last_name :gender :hire_date] females (vals (dissoc bat-employees :gender)))))



(time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (and (= (:gender t) "F" ) (= "1952-11-09" (:birth_date t))))))
(time (let [females (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:gender bat-employees) = "F"))
                                             (:gender bat-employees) =))
            birth (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:birth_date bat-employees) = "1952-11-09"))
                                           (:birth_date bat-employees) =))]
        (apply relation.bat/makeTable [] [:emp_no :birth_date :first_name :last_name :gender :hire_date]
               females birth (vals (dissoc bat-employees :gender :birth_date)))))

(count (time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (and (and (= (:gender t) "M" )
                                                              (= "1952-11-09" (:birth_date t)))
                                                         (or  (= (:first_name t) "Genta")
                                                              (> (:emp_no t) 35000)))))))

(time (let [females (relation.bat/select (:gender bat-employees) = "M")
            birth  (relation.bat/select (:birth_date bat-employees) = "1952-11-09")
            first-name (relation.bat/select (:first_name bat-employees) = "Genta")
            id (relation.bat/select (:emp_no bat-employees) > 35000)
            all (relation.bat/intersect (relation.bat/intersect females birth) (relation.bat/union first-name id))]
        (apply relation.bat/makeTable [] [:emp_no :birth_date :first_name :last_name :gender :hire_date]
              (relation.bat/join (relation.bat/mirror all)  (:emp_no bat-employees) =)
               (vals (select-keys bat-employees [:birth_date :first_name :last_name :gender :hire_date])))))





; ########### mani



(def toInsert {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""})

(time (relation.hashRel/union hashRel-employees (relation.hashRel/rel toInsert)))

(time (relation.hashRel/difference hashRel-employees (relation.hashRel/rel {:emp_no 16574, :birth_date "1963-05-06", :first_name "Nevio", :last_name "Penz", :gender "M", :hire_date "1990-08-23"})))

(time (let[ newId (inc (apply clojure.core/max (map (fn[[_ bat]] (relation.bat/max (relation.bat/reverse bat))) bat-employees)))]
    (into {} (map (fn[[name value]] [name (relation.bat/insert (get bat-employees name) newId value)]) toInsert))))




(let[todelete (into {} (map (fn[attr] [attr [ 3655 (relation.bat/find (get bat-employees attr) 3655) ]]) (keys bat-employees))) ]
   (into {} (map (fn [[attr [h t]]] [attr (relation.bat/delete (get bat-employees attr) h t )] ) todelete)))

