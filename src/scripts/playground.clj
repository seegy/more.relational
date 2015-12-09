(ns example
  (:use [relation.transrelational :as transRel]))

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






