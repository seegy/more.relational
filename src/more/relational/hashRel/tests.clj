(ns wayneHashRel
  (:use [more.relational.hashRel]))






(rel [:sno :sname :status :scity]
         ['("S1" "Smith" 20 "London")
          '("S2" "Jones" 10 "Paris")
          '("S3" "Blake" 30 "Paris")
          '("S4" "Clark" 10 "London")
          '("S5" "Adams" 30 "Athen")])


(rel [{:sno "S1" :sname "Smith" :status 20 :scity "London"}
      {:sno "S2" :sname "Jones" :status 10 :scity "Paris"}
      {:sno "S3" :sname "Blake" :status 30 :scity "Paris"}
      {:sno "S4" :sname "Clark" :status 10 :scity "London"}
      {:sno "S5" :sname "Adams" :status 30 :scity "Athen"}])


(rel [:sno :sname :status :scity]
     [{:sno "S1" :sname "Smith" :status 20 :scity "London"}
      {:sno "S2" :sname "Jones" :status 10 :scity "Paris"}
      {:sno "S3" :sname "Blake" :status 30 :scity "Paris"}
      {:sno "S4" :sname "Clark" :status 10 :scity "London"}
      {:sno "S5" :sname "Adams" :status 30 :scity "Athen"}])

(def r (rel #{ {:id 1, :name "Arthur", :address "somewhere"} {:id 2, :name "Betty" :address "nowhere"} }))
(def s (rel #{{:sid 1 :description "Scrows"}, {:sid 2 :description "Hammer"}, {:sid 3, :description "Nail"}}))
r
(def rs (rel #{{:id 1 :sid 1 :quantity 200} {:id 1 :sid 2 :quantity 2} {:id 2 :sid 3 :quantity 100} {:id 2 :sid 2 :quantity 1}}))

(group (join (join r rs) s) {:NameAndHammer #{:sid :quantity :description}})



(seq r)

(count r)
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



 (def something (relvar blaRelation))

 (save-relvar something "/home/seegy/Desktop/Test-RelVar.file")
 (load-relvar "/home/seegy/Desktop/Test-RelVar.file")


 (def bla (relvar blaRelation))
 (def blubb (relvar blaRelation))

 (def database {:some something :bla bla :blubb blubb})

 (save-db database "/home/seegy/Desktop/Test-RelVar-database-map.file")

 (def database2 #{ something  bla  blubb})

 (save-db database2 "/home/seegy/Desktop/Test-RelVar-database-set.file")

