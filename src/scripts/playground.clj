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



;#######


(def employees-data (take 10000 (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" )))))


(def xrel (map #(zipmap [:emp_no :birth_date :first_name :last_name :gender :hire_date] %) employees-data))

(time (def transrel-employees (relation.transrelational/tr xrel)))


(time (relation.transrelational/insert transrel-employees  {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""}))

(time (relation.transrelational/insert transrel-employees  {:emp_no 99999999999, :birth_date "zzzzzzzzzzzzzzzzzzzzzzzzzz", :first_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :last_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :gender "zzzzzzzzzzzzzzzzzzzzzzzzzz", :hire_date "zzzzzzzzzzzzzzzzzzzzzzzzzz"}))


(defn alter-insert
  [table tupel]
  (let [converted   (relation.transrelational/convert table)
        manipulated (conj converted tupel)]
    (relation.transrelational/tr manipulated)))


(time (alter-insert transrel-employees  {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""}))

(time (alter-insert transrel-employees  {:emp_no 99999999999, :birth_date "zzzzzzzzzzzzzzzzzzzzzzzzzz", :first_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :last_name "zzzzzzzzzzzzzzzzzzzzzzzzzz", :gender "zzzzzzzzzzzzzzzzzzzzzzzzzz", :hire_date "zzzzzzzzzzzzzzzzzzzzzzzzzz"}))





