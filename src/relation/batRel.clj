(ns relation.batRel
  (:require  [relation.newRel.relational :as rel])
  (:require  [relation.bat.bat :as bat]))




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

   (def employees-data (take 10000 employees-data))

 (println "read employees")
 (time (def employees (rel/rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] employees-data))); auf MAC: 65.3 ms

 (println "convert employees to bats")
(time (def employeesBat (convertToBats employees)))







 (def salaries-data (set (read-string  (str "[" (slurp  "resources/salaries.clj" ) "]" ))))
 (count salaries-data)


 (def salaries-data (take 5000 salaries-data))
 (println "read salaries")
 (time (def salaries (rel/rel [:emp_no :salary :from_date :to_date] salaries-data))) ; auf MAC: 80.5ms

 (println "convert salaries to bats")
(time (def salariesBat (convertToBats salaries)))







(def males (bat/select (:gender employeesBat) "M"))
(time (bat/join (bat/reverse (:emp_no employeesBat)) males =))



(count (bat/join (:emp_no employeesBat) (bat/reverse (:emp_no salariesBat)) =))
