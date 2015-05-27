(ns relation.testsOldRel
  (:use [core.relational]))

 ;; ######################## BIG TEST ############################


 (def employees-data (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" ))))
 (count employees-data)

 (def employees-data (take 10000 employees-data))
 (print "read employees")
 (time (def employees (rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] employees-data))); auf MAC: 65.3 ms

 (def salaries-data (set (read-string  (str "[" (slurp  "resources/salaries.clj" ) "]" ))))
 (count salaries-data)


 (def salaries-data (take 5000 salaries-data))
 (print "read salaries")
 (time (def salaries (rel [:emp_no :salary :from_date :to_date] salaries-data))) ; auf MAC: 80.5ms

(print "join")
 (time (def es (join employees salaries))) ; auf MAC 272281.8ms
(count es)






(def employees-data (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" ))))
 (count employees-data)

 (print "read employees")
 (time (def employees (rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] employees-data))); auf MAC: 65.3 ms


 (println "bereichssuche")
;;  Bereichssuchen
(time (restrict employees (relfn [r] (= (:gender r) "M"))))
(time (restrict employees (relfn [r] (and (= (:first_name r) "Arvin") (= (:gender r) "M")))))
(time (-> employees (restrict (relfn [r] (and (= (:first_name r) "Arvin") (= (:gender r) "M")))) (project [:emp_no :first_name])))
(time (restrict employees (relfn [r] (= (:birth_date r) "1964-10-29"))))



 (println "punktsuche")
;; Punktsuchen
(time (restrict employees (relfn [r] (= (:emp_no r) 429270))))
(time (restrict employees (relfn [r] (= (:birth_date r) "1964-10-29"))))
(time (restrict employees (relfn [r] (and (= (:gender r) "M") (= (:birth_date r) "1962-07-15") (= (:last_name r) "Perl") (= (:first_name r) "Arvin") (= (:gender r) "M")))))

 (println "andere operationen")
;; Andere Operationen
(time (rename employees {:emp_no :id}))
(time (union employees employees))
(time (group employees {:dates #{:birth_date :hire_date}}))
(time (summarize employees #{:emp_no} {:scount (relfn [r] (count r))}))
(time (summarize employees #{} {:quantitysum (relfn [r] (reduce + (:emp_no r)))}))
