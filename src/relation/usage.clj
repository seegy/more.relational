(ns relation.usage
  (:use core.relational)
   (:use [clojure.repl])
   (require [wikidata.connector :as wiki]))

(def people (rel [:id :name :aID] #{[1 "Arthur" 2], [2 "Betty" 1], [3 "Charlie" 1]}))

(seq people)


(def people (rel [:id :name :aID] #{[1 "Arthur" 2], [2 "Betty" 1], [3 "Charlie" 1]}))

(def addresses (rel [:aID :address] #{[1 "Alter Platz 1"], [2 "Bäckerstraße"]}))

;(save-rel  people "D:/Downloads/employees/test.clj")


(count (:addressID people))

(count (join people addresses))
(map #(print (:name %)) (join people addresses)) ; 2x nil


(map #( if (not (nil? %)) (print %)) (join people addresses))





;(doc core.relational/rel)
;(source core.relational/rel)


;---------------------------------------------------------------------------------------------------------
; ######################### LARGE DATA TEST! #############################################################
;---------------------------------------------------------------------------------------------------------


;############### SCHEMA #############

(def employees (rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] #{}))
(def salaries (rel [:emp_no :salary :from_date :to_date ] #{}))

(def employees-var (relvar employees {:key :emp_no}))
(def salaries-var (relvar salaries [{:key #{:emp_no :from_date}}
                                    ;{:foreign-key {:key :emp_no,
                                     ;              :referenced-relvar employees-var,
                                      ;             :referenced-key :emp_no}}
                                    ]))

;############### FILL EMPLOYEES #############

(def file "resources/employees.clj")

(def insertClj  (first (map #(read-string (str "[" % "]"))  (let [rdr (clojure.java.io/reader file)]
          (line-seq rdr)))))
(count insertClj)

(defn toEmplyeeMap[row]
  {:emp_no (nth row 0)
   :birth_date (nth row 1)
    :first_name (nth row 2)
    :last_name (nth row 3)
    :gender (nth row 4)
    :hire_date (nth row 5)})

(def employees-var (relvar employees))
(assign! employees-var (rel [:emp_no :birth_date :first_name :last_name :gender :hire_date] (set insertClj)))

 (map #(insert! employees-var (toEmplyeeMap %))  (take 500 insertClj))

 (deref employees-var)

 ;(print-relation (deref employees-var))

 (count (project  (deref employees-var) #{:emp_no}))
(count (restrict (deref employees-var) (relfn [t] (= "M" (:gender t)))))
 (count (restrict (deref employees-var) (relfn [t] (= "F" (:gender t)))))

(def r #{  {:a 1 :b 2} {:a 2 :b 1} {:a 2 :b 3}})




(clojure.set/index r #{:a})


;#################### FILL Salaries #################

(def file "resources/salaries.clj")
(def insertClj  (first (map #(read-string (str "[" % "]"))  (let [rdr (clojure.java.io/reader file)]
          (line-seq rdr)))))
(count insertClj)

(defn toSalatiesMap[row]
  {:emp_no (nth row 0)
   :salary (nth row 1)
   :from_date (nth row 2)
   :to_date (nth row 3)})

(map #(insert! salaries-var (toSalatiesMap %)) (take 500 insertClj))

(deref salaries-var)

;(print-relation (deref salaries-var))

(count (project (deref salaries-var) #{:emp_no :from_date}))




;################# Joins ######################

(join (deref employees-var) (deref salaries-var))

(count (join (deref employees-var) (deref salaries-var)))





;---------------------------------------------------------------------------------------------------------
; ################################ WIKI DATA #############################################################
;---------------------------------------------------------------------------------------------------------

(def persons (rel [:id :name :description :gender :birth_date ] #{}))
(def persons-var (relvar persons {:key :id}))

(defn toPersonMap[row]
  {:id (nth row 0)
   :name  (nth row 1)
    :description (nth row 2)
    :gender (nth row 3)
    :birth_date(nth row 4)})

(map #(insert! persons-var (toPersonMap %))  (wiki/searchFor "Usain Bolt" 20))
(print-relation (deref persons-var))





(let [persons2 (rel [:id :name :description :gender :birth_date ] #{})
        persons2-var (relvar persons2 {:key :id})
        toPersonMap (fn[row]
                      {:id (nth row 0)
                       :name  (nth row 1)
                       :description (nth row 2)
                       :gender (nth row 3)
                       :birth_date(nth row 4)})]
    (map #(insert! persons2-var (toPersonMap %))  (wiki/searchFor "Usain Bolt" 10))
    (print-relation (deref persons2-var)))



(defn createPersonView [name limit]
  (let [persons2 (rel [:id :name :description :gender :birth_date ] #{})
        persons2-var (relvar persons2 {:key :id})
        toPersonMap (fn[row]
                      {:id (nth row 0)
                       :name  (nth row 1)
                       :description (nth row 2)
                       :gender (nth row 3)
                       :birth_date(nth row 4)})]
    (map #(insert! persons2-var (toPersonMap %))  (wiki/searchFor name limit))
    persons2-var))


(createPersonView "" 20)
(print-relation (deref (createPersonView "Usain Bolt" 20)))





;---------------------------------------------------------------------------------------------------------
; ################################  BIG TEST #############################################################
;---------------------------------------------------------------------------------------------------------

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
