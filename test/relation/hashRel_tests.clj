(ns relation.hashRel-tests
   (:use clojure.test)
   (:require [relation.hashRel :refer :all]))





(def r (rel #{ {:id 1, :name "Arthur", :address "somewhere"}
               {:id 2, :name "Betty" :address "nowhere"} }))

(def s (rel #{ {:sid 1 :description "Scrows"},
               {:sid 2 :description "Hammer"},
               {:sid 3, :description "Nail"}}))

(def rs (rel #{{:id 1 :sid 1 :quantity 200}
               {:id 1 :sid 2 :quantity 2}
               {:id 2 :sid 3 :quantity 100}
               {:id 2 :sid 2 :quantity 1}}))

(def people (rel [{:sno "S1" :sname "Smith" :status 20 :scity "London"}
      {:sno "S2" :sname "Jones" :status 10 :scity "Paris"}
      {:sno "S3" :sname "Blake" :status 30 :scity "Paris"}
      {:sno "S4" :sname "Clark" :status 10 :scity "London"}
      {:sno "S5" :sname "Adams" :status 30 :scity "Athen"}]))




(deftest convert-test
  (testing "set"
    (let [xrel #{{:id 1, :name "Arthur", :address "somewhere"} {:id 2, :name "Betty" :address "nowhere"}}
          r (rel xrel)]
      (is (= xrel (set r))))))




(deftest rename-test
  (testing "rename"
    (let [renamed (rename people {:sno :id, :scity :town})]
        (is (not-any? #(contains? % :sno) (set renamed)))
        (is (not-any? #(contains? % :scity) (set renamed)))
        (is (every? #(contains? % :id) (set renamed)))
        (is (every? #(contains? % :town) (set renamed)))
        (is (= (count renamed) (count people)))))

  (testing "rename*"
    (let [renamed (rename* r #"(.+)" "prefix-$1")]
        (is (every? (fn[attrs] (.startsWith (name attrs) "prefix-")) (scheme renamed)))
        (is (every? (fn[tuple] (every? #(.startsWith (name (first %)) "prefix-") tuple)) renamed))
        (is (= (count renamed) (count r))))))







(deftest restrict-test
  (testing "restrict"
    (let [pred #(and (= 10 (:status %)) (= "Paris" (:scity %)))
          restricted (restrict people pred)]
        (is (every? pred (set restricted) ))
        (is (not-any? #(contains? (set restricted) %)
                      (set (restrict people #(not (pred %)))))))))



(deftest project-test
  (testing  "project"
    (let [projected (project people [:sno :status])]
        (is (= (set (scheme projected))))
        (is (not-any? #(contains? % :sname) (set projected)))
        (is (not-any? #(contains? % :scity) (set projected)))
        (is (every? #(contains? % :sno) (set projected)))
        (is (every? #(contains? % :status) (set projected)))
        (is (= (count projected) (count people)))))

  (testing "project+"
    (let [projected (project+ r {:id*2 (relfn [t] (* 2 (:id t)))})]
        (is (contains? (set (scheme projected)) :id*2))
        (is (every? #(= (:id*2 %) (* 2 (:id %))) (set projected)))
        (is (= (count projected) (count r))))
    (let [projected (project+ r {:new "new"})]
        (is (contains? (set (scheme projected)) :new))
        (is (every? #(= "new" (:new %)) (set projected)))
        (is (= (count projected) (count r))))))



(deftest join-test
  (testing "join"
    (let [r-rs (join r rs)]
        (is (= #{:id :name :address :sid :quantity} (set (scheme r-rs))))
        (is (every? #(= #{:id :name :address :sid :quantity} (set (keys %))) (set r-rs)))
        (is (every? (fn [r-tuple]
                      (some (fn[r-rs-tuple]
                               (every?
                                #(contains? (set (vals r-rs-tuple)) %) (vals r-tuple))) (seq r-rs)))  (seq r))))
    (let [r-rs-s (join r (join rs s))]
        (is (= #{:id :name :address :sid :quantity :description} (set (scheme r-rs-s))))
        (is (every? #(= #{:id :name :address :sid :quantity :description} (set (keys %))) (set r-rs-s)))
        (is (every? (fn [r-s-tuple]
                      (some (fn[r-rs-s-tuple]
                               (every?
                                #(contains? (set (vals r-rs-s-tuple)) %) (vals r-s-tuple))) (seq r-rs-s)))  (apply conj (seq r) (seq s))))))

  (testing "compose"
    (let [composed (compose r rs)]
        (is (= #{ :name :address :sid :quantity } (set (scheme composed))))
        (is (every? #(= #{ :name :address :sid :quantity } (set (keys %))) (set composed)))
        (is (= (set composed ) (set (project- (join r rs) #{:id})))))))



(deftest set-operators-test
  (testing "union"
    (let [ xrel  #{ {:id 1, :name "Arthur", :address "somewhere"}
                    {:id 11, :name "xxxxArthur", :address "xxxxsomewhere"}
                    {:id 12, :name "xxxxBetty" :address "xxxxnowhere"} }
          unioned (union r (rel xrel))]
        (is (every? #(contains? (set unioned) %) xrel))
        (is (every? #(contains? (set unioned) %) (set r)))
      (is (every? #(or (contains? xrel %) (contains? (set r) %)) unioned))))

  (testing "intersect"
    (let [xrel  #{ {:id 1, :name "Arthur", :address "somewhere"}
                    {:id 11, :name "xxxxArthur", :address "xxxxsomewhere"}
                    {:id 12, :name "xxxxBetty" :address "xxxxnowhere"} }
          intersected (intersect r (rel xrel))]
      (is (every? #(and (contains? xrel %) (contains? (set r) %)) intersected))))

  (testing "difference"
    (let [xrel  #{ {:id 1, :name "Arthur", :address "somewhere"}
                    {:id 11, :name "xxxxArthur", :address "xxxxsomewhere"}
                    {:id 12, :name "xxxxBetty" :address "xxxxnowhere"} }
          differenced1 (difference r (rel xrel))
          differenced2 (difference (rel xrel) r)]
      (is (every? #(and (contains? xrel %) (not (contains? (set r) %))) differenced2))
      (is (every? #(and (not (contains? xrel %)) (contains? (set r) %)) differenced1)))))



(deftest devide-test
  (testing "devide"
    (let [])))

#_(




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

 (save-db database2 "/home/seegy/Desktop/Test-RelVar-database-set.file"))

