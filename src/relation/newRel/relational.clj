(ns relation.newRel.relational
  (:require [clojure.repl   :refer :all]
            [clojure.string :as str]
            [clojure.edn    :as edn]))

; entry point for core.relational; loads other classes

(declare same-type?)
(declare same-attr-order?)
(declare sort-vec)
(declare index-of)

(load "relation")
(load "tools")
(load "operators")





(def r (rel #{ {:id 1, :name "Arthur", :address "somewhere"} {:id 2, :name "Betty" :address "nowhere"} }))
(def s (rel #{{:sid 1 :description "Scrows"}, {:sid 2 :description "Hammer"}, {:sid 3, :description "Nail"}}))

(def rs (rel #{{:id 1 :sid 1 :quantity 200} {:id 1 :sid 2 :quantity 2} {:id 2 :sid 3 :quantity 100} {:id 2 :sid 2 :quantity 1}}))

(def relation1 r)
(def relation2 rs)

(def ks (clojure.set/intersection (set (.head relation1)) (set (.head relation2))))

(def r relation1)
(def s relation2)

(def idx (clojure.set/index r ks))

(def x (first s))
(def ret #{})

(def xform
  (comp
   #(select-keys % ks)
   #(idx %)
   ))

(reduce (fn [ret x] (transduce xform #(conj %1 (merge %2 x)) ret x)) #{} s)


;(transduce  (comp (filter #(= (:id (key %) 1))) (map #(assoc (key %) :id 3 ))) conj {} idx)


(def xf (comp (filter odd?) (map inc)))
(transduce xf + (range 1000))

(into [] xf (range 1000))
(sequence xf (range 1000))


(def xform (comp #(select-keys %) #(idx %) #([x %])))

(transduce (fn [ret [x found]]
          (if found
              (reduce #(conj %1 (merge %2 x)) ret found)
                  ret)) xform #{} s)




  #_(join [relation1 relation2]
        (if (and (seq relation1) (seq relation2))
          (let [ks (clojure.set/intersection (set (.head relation1)) (set (.head relation2)))
                [r s] (if (<= (count relation1) (count relation2))
                        [relation1 relation2]
                        [relation2 relation1])
                idx (clojure.set/index r ks)]
            (rel (reduce (fn [ret x]
                      (let [found (idx (select-keys x ks))]
                        (if found
                          (reduce #(conj %1 (merge %2 x)) ret found)
                          ret)))
                    #{} s)))
          (rel [] #{}))) ;TODO head


(join r rs)

#_(
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

)
