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
(rename r {:id :di})

(rename* r #"(.+)" "prefix-$1")
(restrict r (relfn [t] (= (:name t) "Betty")))

(project r [:id :name])
(project r {:name "Name", :address "Adresse"})

(project r {:name :name, :id (relfn [t] (* 2 (:id t)))})

(project- r [:address])

(project+ r {:id*2 (relfn [t] (* 2 (:id t)))})
