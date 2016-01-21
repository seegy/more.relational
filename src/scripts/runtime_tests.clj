(ns example
  (:require relation.transrelational)
  (:require relation.bat )
  (:require relation.hashRel))



(def employees-data (take 10000 (set (read-string  (str "[" (slurp  "resources/employees.clj" ) "]" )))))
(def xrel (map #(zipmap [:emp_no :birth_date :first_name :last_name :gender :hire_date] %) employees-data))


(println "\nCreating relations")
(time (def hashRel-employees (relation.hashRel/rel xrel)))
(time (def bat-employees (relation.bat/convertToBats xrel)))
(time (def tr-employees (relation.transrelational/tr xrel)))



; #################### Punktsuche

(println "\nPunktsuche")

(time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (= (:emp_no t) 485652 ))))

(time (let [id (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:emp_no bat-employees) = 485652)) (:emp_no bat-employees) =))]
       ))


(time (relation.transrelational/restriction tr-employees (relation.transrelational/restrict-fn [t] (= (:emp_no t) 485652))))




;################# Bereichssuche


(println "\nBereichssuche 1")

(count (time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (= (:gender t) "F" )))))

(time (let [females (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:gender bat-employees) = "F")) (:gender bat-employees) =))]
        ))


(time (relation.transrelational/restriction tr-employees (relation.transrelational/restrict-fn [t] (= (:gender t) "F"))))



(println "\nBereichssuche 2")

(time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (and (= (:gender t) "F" ) (= "1952-11-09" (:birth_date t))))))
(time (let [females (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:gender bat-employees) = "F"))
                                             (:gender bat-employees) =))
            birth (time (relation.bat/join (relation.bat/mirror (relation.bat/select (:birth_date bat-employees) = "1952-11-09"))
                                           (:birth_date bat-employees) =))]
        ))


  (time (relation.transrelational/restriction tr-employees (relation.transrelational/restrict-fn [t] (and (= (:gender t) "F" ) (= "1952-11-09" (:birth_date t))))))




(println "\nBereichssuche 3")

(count (time (relation.hashRel/restrict hashRel-employees (relation.hashRel/relfn [t] (and (and (= (:gender t) "M" )
                                                              (= "1952-11-09" (:birth_date t)))
                                                         (or  (= (:first_name t) "Genta")
                                                              (> (:emp_no t) 35000)))))))

(time (let [females (relation.bat/select (:gender bat-employees) = "M")
            birth  (relation.bat/select (:birth_date bat-employees) = "1952-11-09")
            first-name (relation.bat/select (:first_name bat-employees) = "Genta")
            id (relation.bat/select (:emp_no bat-employees) > 35000)
            all (relation.bat/intersect (relation.bat/intersect females birth) (relation.bat/union first-name id))]
              (relation.bat/join (relation.bat/mirror all)  (:emp_no bat-employees) =)))


(time (relation.transrelational/restriction tr-employees (relation.transrelational/restrict-fn [t] (and (and (= (:gender t) "M" )
                                                              (= "1952-11-09" (:birth_date t)))
                                                         (or (= (:first_name t) "Genta")
                                                              (> (:emp_no t) 35000))))))


; ########### mani

(def toInsert {:emp_no 0, :birth_date "", :first_name "", :last_name "", :gender "", :hire_date ""})


(println "\nInsert")

(time (relation.hashRel/union hashRel-employees (relation.hashRel/rel toInsert)))

(time (let[ newId (inc (apply clojure.core/max (map (fn[[_ bat]] (relation.bat/max (relation.bat/reverse bat))) bat-employees)))]
    (into {} (map (fn[[name value]] [name (relation.bat/insert (get bat-employees name) newId value)]) toInsert))))

(time (relation.transrelational/insert tr-employees toInsert))



(println "\nDelete")

(time (relation.hashRel/difference hashRel-employees (relation.hashRel/rel {:emp_no 16574, :birth_date "1963-05-06", :first_name "Nevio", :last_name "Penz", :gender "M", :hire_date "1990-08-23"})))

(time (let[todelete (into {} (map (fn[attr] [attr [ 3655 (relation.bat/find (get bat-employees attr) 3655) ]]) (keys bat-employees))) ]
   (into {} (map (fn [[attr [h t]]] [attr (relation.bat/delete (get bat-employees attr) h t )] ) todelete))))

(time (relation.transrelational/delete tr-employees 0 0))

