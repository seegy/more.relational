(ns relation.transRel-tests
   (:use clojure.test)
   (:require [relation.transrelational :refer :all])
   (:refer-clojure :exclude [extend update max min]))




(def people #{{:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}})




(deftest convert-test
  (testing "create relation"
    (let [relation (tr people)]
      (is (-> relation keyorder empty? not))
      (is (-> relation fieldValues empty? not))
      (is (-> relation recordReconst empty? not))))
  (testing "convert relation"
    (let [relation (tr people)
          converted-result (convert relation)]
      (is (= (set converted-result) (set people))))))



(deftest insert-test
  (testing "new tuple"
     (let [relation (tr people)
          tuple {:id "S6" :name "Jeff" :status 10 :city "Berlin"}
          inserted (insert relation tuple)]
       (is (= (count relation) (dec (count inserted))))
       (is (= (set (convert inserted)) (conj people tuple)))))
  (testing "duplicate"
    (let [relation (tr people)
          tuple {:id "S5" :name "Adams" :status 30 :city "Athens"}
          inserted (insert relation tuple)]
      (is (= relation inserted)))))




(deftest search-test
  (testing "point-search")
  (testing "area-search")
  (testing "restriction"
    (let [relation (tr people)]
      (let [pred (restrict-fn [t] (<= 30 (:status t)))
            result (restriction relation pred)
            converted-result (convert result)]
        (is (= 2 (count result)))
        (is (= 2 (count converted-result)))
        (is (every? pred converted-result)))

      (let [pred (restrict-fn [t] (and (= "Paris" (:city t)) (<= 30 (:status t))))
            result (restriction relation pred)
            converted-result (convert result)]
        (is (= 1 (count result)))
        (is (= 1 (count converted-result)))
        (is (every? pred converted-result)))

      (let [pred (restrict-fn [t] (< 30 (:status t)))
            result (restriction relation pred)]
        (is (= 0 (count result)))
        (is (= (keyorder relation) (keyorder result)))))))




(deftest aggregat-test
  (testing "max"
    (let [relation (tr people)]
      (is (= 30 (max relation :status)))))
  (testing "min"
    (let [relation (tr people)]
      (is (= 10 (min relation :status)))))
  (testing "sum"
    (let [relation (tr people)]
      (is (= 110 (sum relation :status))))))


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
