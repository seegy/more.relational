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

   (def employees-data (take 100000 employees-data))

 (println "read employees")
 (time (def employees (rel/rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] employees-data))); auf MAC: 65.3 ms

 (println "convert employees to bats")
(time (def employeesBat (convertToBats employees)))







 (def salaries-data (set (read-string  (str "[" (slurp  "resources/salaries.clj" ) "]" ))))
 (count salaries-data)


 (def salaries-data (take 100000 salaries-data))
 (println "read salaries")
 (time (def salaries (rel/rel [:emp_no :salary :from_date :to_date] salaries-data)))

 (println "convert salaries to bats")
(time (def salariesBat (convertToBats salaries)))







(def males (bat/select (:gender employeesBat) = "M"))
(time (bat/join (bat/reverse (:emp_no employeesBat)) males =))


(println "join")
(time (def batbat (bat/join (:emp_no employeesBat) (bat/reverse (:emp_no salariesBat)) =)))
(count batbat)








; ############################### beispiel

; SELECT employee.last_name  AS :last_name , salaries.salary / 12  AS :monthlySalary
; FROM employee JOIN salaries
; WHERE salaries.salary BETWEEN 100000 AND 110000
; ORDER BY :monthlySalary

(defn between [x l h] (and (<= x h) (>= x  l)))


(def sal_id_nil (bat/select (:salary salariesBat) between 100000 110000))
(def sal_id_sal_salary (bat/join (bat/mirror sal_id_nil) (:salary salariesBat) =))
(def sal_emp_no_sal_salary (bat/join (bat/reverse (:emp_no salariesBat)) sal_id_sal_salary =))
(def sal_emp_no_sal_salary_mod (bat/multijoin / sal_emp_no_sal_salary 12))
(def emp_id_emp_no (bat/join (:emp_no employeesBat) (bat/mirror sal_emp_no_sal_salary_mod) =))
(def emp_emp_no_lastname (bat/join (bat/reverse  emp_id_emp_no)  (:last_name employeesBat) =))

(bat/makeTable [:monthlySalary] [:last_name :monthlySalary] emp_emp_no_lastname sal_emp_no_sal_salary_mod )
