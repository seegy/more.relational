(ns relation.incanter
 (:use [incanter core stats charts io])
   (:use [clojure.repl])
  (require [wikidata.connector :as wiki]))


;------- Examples of incanter

;(view (histogram (sample-normal 1000)))

(dataset ["x1" "x2" "x3"]
 [[1 2 3]
 [4 5 6]
 [7 8 9]])

(to-dataset [[1 2 3]
 [4 5 6]
 [7 8 9]])



;---- Examples of bader

(dataset ["sno" "sname" "status" "scity"]
         [["S1" "Smith" 20 "London"]
          ["S2" "Jones" 10 "Paris"]])

(def s (dataset [:sno :sname :status :scity]
         [["S1" "Smith" 20 "London"]
          ["S2" "Jones" 10 "Paris"]
          ["S3" "Blake" 30 "Paris"]
          ["S4" "Clark" 10 "London"]
          ["S5" "Adams" 30 "Athen"]]))
s



(def sp (dataset [:sno :pno :qty]
                 [["S1" "P1" 300]
                  ["S2" "P1" 200]
                  ["S1" "P3" 200]
                  ["S2" "P2" 200]]))
sp

(def p (dataset [:pno :pname :color :weight :pcity]
                [["P1" "Nut" "Red" 12 "London"]
                 ["P2" "Bolt" "Green" 17 "Paris"]]))
p




;rename

(col-names s)
(col-names s [:s1_sno :s1_sname :s1_status :s1_scity])



;projection
;; SELECT sname, status, scity FROM s
(sel s :cols [:sname :status :scity])
;;TODO
;; select sno, sname, status, scity, (status / 10) as new_status From s
($map (fn [s] (/ s 10)) :status s)
(replace-column :status ($map (fn [s] (/ s 10)) :status s) s)
(col-names (replace-column :status ($map (fn [x] (/ x 10)) :status s) s) [:sno :sname :new_status :scity])



;; select * From s where scity='Paris`
($where {:scity {:fn  #(= "Paris" %)}} s)



;;SELECT * FROM s WHERE status > 20
($where {:status {:fn  #(> % 20)}} s)



;;SELECT * FROM s WHERE scity = Paris AND status > 20
($where {:scity {:fn  #(= "Paris" %)}
         :status {:fn  #(> % 20)}} s)


;;SELECT sname FROM s WHERE scity = London;
(sel
 ($where {:scity {:fn  #(= "London" %)}} s)
     :cols [:sname])




;;SELECT * FROM s NATURAL JOIN sp;
;;or
;;SELECT * FROM s JOIN sp USING (pno);
($join [:sno :sno] s sp)




;;SELECT * FROM s NATURAL JOIN sp NATURAL JOIN p WHERE pname = Screw;
($where {:pname {:fn  #(= % "Screw")}} ($join [:pno :pno] sp p))




;;SELECT scity, status, sname, qty, pcity, weight, color, pname FROM s NATURAL JOIN sp NATURAL JOIN p;
(sel ($join [:sno :sno] ($join [:pno :pno] sp p) s)
     :cols [:scity :status :sname :qty :pcity :weight :color :pname])



;;SELECT s.sname, s1.sname FROM s, s AS s1 WHERE s.scity = s1.scity AND s.sno > s1.sno;
;no cartesian product supported.




;;SELECT SUM(qty) FROM sp;
(sel ($rollup :sum :qty [:sno :pno]  sp) :cols [:qty] )



;;SELECT sno, SUM(qty) AS parts_total FROM sp GROUP BY sno;
(sel ($rollup :sum :qty [:sno ]  sp) :cols [:sno :qty] )

;;SELECT sno, SUM(qty) AS parts_total FROM sp GROUP BY sno ORDER BY parts_total ASC;
($order :parts_total :asc
        (col-names
 (sel ($rollup :sum :qty [:sno]  sp)
      :cols [:sno :qty] )
 [:sno :parts_total] ))



;;SELECT * FROM s
;;UNION
;;SELECT '56', 'MEYER', 10, 'Berlin';
(conj-rows s ["54" "MEYER" 10 "Berlin"])



;;SELECT * FROM s
;;EXCEPT
;;SELECT 'S2', 'Jones', 10, 'Paris';

;just by filter



;;SELECT sno, sname FROM s NATURAL JOIN sp NATURAL JOIN p WHERE p.pno = 'P2'
;;INTERSECT
;;SELECT sno, sname FROM s NATURAL JOIN sp NATURAL JOIN p WHERE p.pno = 'P4'
;nope

;;--------------------------------------------------------------------------




;---------------------------------------------------------------------------------------------------------
; ######################### LARGE DATA TEST! #############################################################
;---------------------------------------------------------------------------------------------------------

#_(
(def file "resources/employees.clj")

(def insertCljEmployees  (first (map #(read-string (str "[" % "]"))  (let [rdr (clojure.java.io/reader file)]
          (line-seq rdr)))))
(count insertCljEmployees)

(def employees (dataset [:emp_no :birth_date :first_name :last_name :gender :hire_date] insertCljEmployees))

(length  employees)

(sel employees :cols [:emp_no])




(def file "resources/salaries.clj")
(def insertCljSalaries  (first (map #(read-string (str "[" % "]"))  (let [rdr (clojure.java.io/reader file)]
          (line-seq rdr)))))
(count insertCljSalaries)

(def salaries (dataset [:emp_no :salary :from_date :to_date ] insertCljSalaries))
salaries


(sel
 ($where {:gender {:fn  #(= % "M")}}
         ($join [:emp_no :emp_no] employees salaries))
 :cols [:first_name])
)

;---------------------------------------------------------------------------------------------------------
; ################################ WIKI DATA #############################################################
;---------------------------------------------------------------------------------------------------------

#_(
(def persons (dataset [:id :name :description :gender :birth_date ] (wiki/searchFor "Usain Bolt" 20)))

 (print (dataset [:id :name :description :gender :birth_date ] (wiki/searchFor "Usain Bolt" 20)))

 (defn createPersonView [name limit]
   (let [data (wiki/searchFor name limit)
         persons (dataset [:id :name :description :gender :birth_date ] data)]
     (print persons)
     persons))

 (createPersonView "Michael Jackson" 10)
)
