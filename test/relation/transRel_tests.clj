(ns relation.transRel-tests
   (:use clojure.test)
   (:require [relation.transrelational :refer :all])
   (:refer-clojure :exclude [extend update]))




(def people [{:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}])


(deftest search-test
  (testing "point-search")
  (testing "area-search")
  (testing "restriction"
    (let [relation (tr people)]
      (let [pred (restrict-fn [t] (<= 30 (:status t)))
            result (restriction relation pred)
            converted-result (convert result)]
        (is (= 2 (count result)))
        (is (every? pred converted-result)))

      (let [pred (restrict-fn [t] (and (= "Paris" (:city t)) (<= 30 (:status t))))
            result (restriction relation pred)
            converted-result (convert result)]
        (is (= 1 (count result)))
        (is (every? pred converted-result))))))



#_(
(meta (restrict-fn [t] (and (>= 30 (:status t)) (= (:city t) "Paris"))))

;(meta (restrict-fn [t] (and (>= (:status t) 30) (= (:city t) "Paris") (#(= (last %1 ) (last %2)) (:name t) "Paris"))))





(restrict-fn-analytic [t] (and (= "Paris" (:city t))  (<= 30 (:status t))))


(pred-search people
             (restrict-fn [t] (and (= "Paris" (:city t)) (<= 30 (:status t)))))

(restriction people
             (restrict-fn [t] (and (= "Paris" (:city t)) (<= 30 (:status t)))))


(clojure.set/intersection (point-search people :city "Paris") (area-search people :status >= 30))


(restriction people
             (restrict-fn [t] (and (>= 30 (:status t)) (= (:city t) "Paris"))))

(restriction people
             (restrict-fn [t] (and (= (:city t) "Paris")
                                   (#(= (last %1 ) (last %2)) (:name t) (:city t)))))

(restriction people
             (restrict-fn [t] (and (>= (:status t) 30) (= (:city t) "Paris")
                            (#(= (last %1 ) \s) (:name t)))))


(area-search people :status >=  30)
(area-search people :status < 30)
(area-search people :city not= "London")
(point-search people :city "London")
(inner-compare people  #(= (last %1 ) (last %2)) :name :city)
)
