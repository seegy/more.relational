(ns relation.batRel-tests
   (:use clojure.test)
   (:require [relation.bat :refer :all])
   (:refer-clojure :exclude [find reverse slice min max update load]))




(def names ["Smith" "Jones" "Blake" "Clark" "Adams"])

(def names-with-heads [{:head 1 :tail "Smith"}
                        {:head 2 :tail "Jones"}
                        {:head 3 :tail "Blake"}
                        {:head 4 :tail "Clark"}
                        {:head 5 :tail "Adams"}])


(def people [{:sno "S1" :sname "Smith" :status 20 :scity "London"}
              {:sno "S2" :sname "Jones" :status 10 :scity "Paris"}
              {:sno "S3" :sname "Blake" :status 30 :scity "Paris"}
              {:sno "S4" :sname "Clark" :status 20 :scity "London"}
              {:sno "S5" :sname "Adams" :status 30 :scity "Athen"}])






(deftest create-test
  (testing "creating bats"
    (let [name-bat (apply bat names)]
      (is (= (count name-bat) 5))
      (is (= #{"Smith" "Jones" "Blake" "Clark" "Adams"} (into #{} (map #(:tail %) (seq name-bat))))))
    (let [name-bat (bat names-with-heads)]
      (is (= (count name-bat) 5))
      (is (= #{"Smith" "Jones" "Blake" "Clark" "Adams"} (into #{} (map #(:tail %) (seq name-bat)))))))
  (testing "convert to bats"
    (let [converted (convertToBats people)]
      (is (= (count converted) 4))
      (is (= (set (keys converted)) #{:sno :sname :status :scity})))))





(deftest getting-test
  (testing "find"
    (let [name-bat (bat names-with-heads)]
      (is (= "Smith" (find name-bat 1)))
      (is (= "Adams" (find name-bat 5)))))
  (testing "select"
    (let [converted (convertToBats people)]
      (is (= 1 (count (select (:status converted) = 10))))
      (is (= 3 (count (select (:status converted) <= 20)))))))
