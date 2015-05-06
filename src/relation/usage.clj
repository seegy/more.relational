(ns relation.usage
  (:use core.relational)
   (:use [clojure.repl])
   (require [wikidata.connector :as wiki]))


(def people (rel [:id :name :aID] #{[1 "Arthur" 2], [2 "Betty" 1], [3 "Charlie" 1]}))

(def addresses (rel [:aID :address] #{[1 "Alter Platz 1"], [2 "Bäckerstraße"]}))

(save-rel  people "D:/Downloads/employees/test.clj")


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

 (map #(insert! employees-var (toEmplyeeMap %))  (take 500 insertClj))

 (deref employees-var)

 ;(print-relation (deref employees-var))

 (count (project  (deref employees-var) #{:emp_no}))
(count (restrict (deref employees-var) (relfn [t] (= "M" (:gender t)))))
 (count (restrict (deref employees-var) (relfn [t] (= "F" (:gender t)))))






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
(def person-var (relvar persons {:key :id}))

(defn toPersonMap[row]
  {:id (nth row 0)
   :name  (nth row 1)
    :description (nth row 2)
    :gender (nth row 3)
    :birth_date(nth row 4)})

(map #(insert! person-var (toPersonMap %))  (wiki/searchFor "Angela Merkel" 20))

(print-relation (deref person-var))
