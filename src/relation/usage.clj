(ns relation.usage
  (:use core.relational)
   (:use [clojure.repl]))


(def people (rel [:id :name :aID] #{[1 "Arthur" 2], [2 "Betty" 1], [3 "Charlie" 1]}))

(def addresses (rel [:aID :address] #{[1 "Alter Platz 1"], [2 "Bäckerstraße"]}))


(count (:addressID people))

(count (join people addresses))
(map #(print (:name %)) (join people addresses)) ; 2x nil


(map #( if (not (nil? %)) (print  %)) (join people addresses))





(doc core.relational/rel)
(source core.relational/rel)
