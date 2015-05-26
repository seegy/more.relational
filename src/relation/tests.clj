(ns relation.tests
  (:use [relation.newRel.relational]))



(def r (rel #{ {:id 1, :name "Arthur", :address "somewhere"} {:id 2, :name "Betty" :address "nowhere"} }))
(def s (rel #{{:sid 1 :description "Scrows"}, {:sid 2 :description "Hammer"}, {:sid 3, :description "Nail"}}))

(def rs (rel #{{:id 1 :sid 1 :quantity 200} {:id 1 :sid 2 :quantity 2} {:id 2 :sid 3 :quantity 100} {:id 2 :sid 2 :quantity 1}}))

(seq r)


(rename r {:id :di})

(rename* r #"(.+)" "prefix-$1")
(restrict r (relfn [t] (= (:name t) "Betty")))

(project r [:id :name])
(project r {:name "Name", :address "Adresse"})

(project r {:name :name, :id (relfn [t] (* 2 (:id t)))})

(project- r [:name :address])

(project+ r {:id*2 (relfn [t] (* 2 (:id t)))})

(join r rs)

(join (join r rs) s)

(compose r rs)

(compose (compose r rs) s)

(def r2 (rel #{ {:id 1, :name "Arthur", :address "somewhere"} {:id 11, :name "xxxxArthur", :address "xxxxsomewhere"} {:id 12, :name "xxxxBetty" :address "xxxxnowhere"} }))

(union r r2)
(union r2 r)


(intersect r r2)

(difference r r2)
(difference r2 r)


(divide r (project r #{:address}))
(divide rs r)


(def relation1 r)
(def relation2 rs)


(def r1-only-attrs (diverging-attr relation1 relation2))
(def r1-only (project relation1 r1-only-attrs))

(let [r1-only-attrs (diverging-attr relation1 relation2)
          r1-only (project relation1 r1-only-attrs)]
      (difference r1-only
                  (project (difference (join r1-only relation2)
                                       relation1)
                           r1-only-attrs)))


(group (join (join r rs) s) {:NameAndHammer #{:sid :quantity :description}})
(def srgroup (group (join (join r rs) s) {:NameAndHammer #{:sid :quantity :description}}))
(ungroup srgroup #{:NameAndHammer})


  (wrap  (join (join r rs) s) {:article #{:sid :description}})

  (def wraped (wrap  (join (join r rs) s) {:article #{:sid :description}}))

(unwrap wraped #{:article})

 (def blaRelation (join (join r rs) s))
 (summarize blaRelation #{:sid :description} {:scount (relfn [r] (count r))})
 (summarize blaRelation #{ :description} {:quantitysum (relfn [r] (reduce + (:quantity r)))})
 (summarize blaRelation #{} {:quantitysum (relfn [r] (reduce + (:quantity r)))})



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
