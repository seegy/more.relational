(ns relation.batRel
  (:require  [relation.newRel.relational :as rel])
  (:require  [relation.bat.bat]))




(def rr (rel/rel #{ {:id 1, :name "Arthur", :address "somewhere"} {:id 2, :name "Betty" :address "nowhere"} }))
(def ss (rel/rel #{{:sid 1 :description "Scrows"}, {:sid 2 :description "Hammer"}, {:sid 3, :description "Nail"}}))



(defn convertToBats
  [rel]
  (let [ splitData (reduce (fn[container attr]
            (assoc container attr (map #(attr %) (rel/body rel))))
          {} (rel/scheme rel))]
    (reduce (fn [container [k v]] (assoc container k (apply bat/bat (reverse v)))) {} splitData)))


(convertToBats rr)
(convertToBats ss)


(def employees-data (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" ))))
 (count employees-data)

 (print "read employees")
 (time (def employees (rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] employees-data))); auf MAC: 65.3 ms

(time (def employeesBat (convertToBats employees)))

(bat/select (:gender employeesBat) "M")



