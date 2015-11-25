(ns example.hashrel
  (:use [relation.hashRel :as hashRel])
  (:use [core.relational :as crel]))



;##################### Beispieldaten
; 10000 Employees, 5000 Salaries, 158 Übereinstimmungen

(def employees-data (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" ))))
(def employees-data (take 10000 employees-data))


(def salaries-data (set (read-string  (str "[" (slurp  "resources/salaries.clj" ) "]" ))))
(def salaries-data (take 5000 salaries-data))





;##################### core.relational


(def crel-employees (crel/rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] employees-data))

(def crel-salaries (crel/rel [:emp_no :salary :from_date :to_date] salaries-data))

(time (crel/join crel-employees crel-salaries))





;##################### hashRel

(def hashRel-employees (hashRel/rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] employees-data))

(def hashRel-salaries (hashRel/rel [:emp_no :salary :from_date :to_date] salaries-data))

(time (hashRel/join hashRel-employees hashRel-salaries))










(hashRel/restrict hashRel-employees
                  (hashRel/relfn [t] (= (first (:last_name t)) \T )))

(hashRel/rename hashRel-employees {:last_name :surname})


