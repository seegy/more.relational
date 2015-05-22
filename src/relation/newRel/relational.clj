(ns relation.newRel
  (:require [clojure.repl   :refer :all]
            [clojure.string :as str]
            [clojure.edn    :as edn]))

; entry point for core.relational; loads other classes

(declare same-type?)
(declare same-attr-order?)
(declare sort-vec)
(declare index-of)

(load "newRel/relation")
(load "newRel/tools")
(load "newRel/operators")



(def r (rel #{ {:id 1, :name "Arthur", :address "somewhere"} {:id 2, :name "Betty" :address "nowhere"} }))
(def s (rel #{{:sid 1 :description "Scrows"}, {:sid 2 :description "Hammer"}, {:sid 3, :description "Nail"}}))

(def rs (rel #{{:id 1 :sid 1 :donation 200} {:id 1 :sid 2 :donation 2} {:id 2 :sid 3 :donation 100} {:id 2 :sid 2 :donation 1}}))

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

(def r2 (rel #{ {:id 11, :name "xxxxArthur", :address "xxxxsomewhere"} {:id 12, :name "xxxxBetty" :address "xxxxnowhere"} }))

(union r r2)

