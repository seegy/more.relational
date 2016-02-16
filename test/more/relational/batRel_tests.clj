(ns more.relational.batRel-tests
   (:use clojure.test)
   (:require [more.relational.bat :refer :all])
   (:refer-clojure :exclude [find reverse slice min max update load]))




(def names ["Smith" "Jones" "Blake" "Clark" "Adams"])

(def names-with-heads [{:head 1 :tail "Smith"}
                        {:head 2 :tail "Jones"}
                        {:head 3 :tail "Blake"}
                        {:head 4 :tail "Clark"}
                        {:head 5 :tail "Adams"}])


(def people [{:id "S1" :name "Smith" :status 20 :city "London"}
              {:id "S2" :name "Jones" :status 10 :city "Paris"}
              {:id "S3" :name "Blake" :status 30 :city "Paris"}
              {:id "S4" :name "Clark" :status 20 :city "London"}
              {:id "S5" :name "Adams" :status 30 :city "Athen"}])

(def sp #{{:id "S1" :pno "P1" :qty 300}
          {:id "S2" :pno "P1" :qty 200}
          {:id "S1" :pno "P3" :qty 200}
          {:id "S2" :pno "P2" :qty 200}})




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
      (is (= (set (keys converted)) #{:id :name :status :city})))))



(deftest getting-test
  (testing "find"
    (let [name-bat (bat names-with-heads)]
      (is (= "Smith" (find name-bat 1)))
      (is (= "Adams" (find name-bat 5)))))
  (testing "select"
    (let [converted (convertToBats people)]
      (is (= 1 (count (select (:status converted) = 10))))
      (is (= 3 (count (select (:status converted) <= 20)))))))






(deftest createRef
  (testing "creating bvar"
    (let [ relation (convertToBats people)
           bvar (batvar relation)]
      (is (= @bvar relation)))
    (let [ relation (convertToBats people)
           constraints #{{:key :id}
                         (fn [rel] (and (<= 10 (min (:status rel))) (>= 30 (max (:status rel)))))}
           bvar (batvar relation constraints)]
      (is (= @bvar relation))
      (is (thrown? IllegalArgumentException (batvar (assoc @bvar :status (insert (:status relation) 234 0)) constraints)))
      (is (thrown? IllegalArgumentException (batvar (assoc @bvar :status (insert (:status relation) 234 31)) constraints))))))



(deftest assignRef
  (testing "assign!"
    (let [relation (convertToBats people)
          bvar (batvar relation)
          rel2 (convertToBats #{{:id "S1" :name "Smith" :status 20 :city "London"}
                                  {:id "S2" :name "Jones" :status 10 :city "Paris"}})]
      (assign! bvar rel2)
      (is (not (= @bvar relation)))
      (is (= @bvar rel2))
      (is (thrown? IllegalArgumentException (assign! bvar (convertToBats #{{:id "S1" :name "Smith"}
                                                                           {:id "S2" :name "Jones"}})))))))



 (deftest insertRef
  (testing "insert!"
    (let [relation (convertToBats people)
          bvar (batvar relation)]
      (insert! bvar :name 99 "Testname")
      (is (contains? (buns (:name @bvar)) {:head 99 :tail "Testname"})))
    (let [relation (convertToBats people)
          bvar (batvar relation)
          tuple {:id "S10" :name "bla" :status 31 :city "blubb"}]
      (insert! bvar tuple)
      (is (contains? (set (makeTable! bvar)) tuple)))))



(deftest deleteRef
  (testing "delete!"
    (let [relation (convertToBats people)
          bvar (batvar relation)]
      (delete! bvar :name 1 "Jones")
      (is (= 4 (count (:name @bvar))))
      (is (not (contains? (buns (:name @bvar)) {:head 1 :tail "Jones"}))))
    (let [relation (convertToBats people)
          bvar (batvar relation)
          tuple {:id "S1" :name "Smith" :status 20 :city "London"}]
      (delete! bvar tuple)
      (is (every? #(= (count (second %)) 4) (seq @bvar)))
      (is (not (contains? (set (makeTable! bvar)) tuple)))
      (is (= (set (makeTable! bvar)) #{{:id "S2" :name "Jones" :status 10 :city "Paris"}
                                        {:id "S3" :name "Blake" :status 30 :city "Paris"}
                                        {:id "S4" :name "Clark" :status 20 :city "London"}
                                        {:id "S5" :name "Adams" :status 30 :city "Athen"}})))))

(deftest updateRef
  (testing "update!"
    (let [relation (convertToBats people)
          bvar (batvar relation)]
      (update! bvar :name 1 "Jones" "Jim")
      (is (contains? (set (:name @bvar)) {:head 1 :tail "Jim"}))
      (is (not (contains? (set (:name @bvar)) {:head 1 :tail "Jones"}))))
    (let [relation (convertToBats people)
          bvar (batvar relation)]
      (update! bvar :city "London" "Manchester")
      (is (= (set (makeTable! bvar)) #{{:id "S1" :name "Smith" :status 20 :city "Manchester"}
                                       {:id "S2" :name "Jones" :status 10 :city "Paris"}
                                        {:id "S3" :name "Blake" :status 30 :city "Paris"}
                                        {:id "S4" :name "Clark" :status 20 :city "Manchester"}
                                        {:id "S5" :name "Adams" :status 30 :city "Athen"}})))))



(deftest resetConst
  (testing "reset constraints"
    (let [relation (convertToBats people)
          constraints #{{:key :id}
                         (fn [rel] (and (<= 10 (min (:status rel))) (>= 30 (max (:status rel)))))}
          bvar (batvar relation constraints)]
      (constraint-reset! bvar nil)
      (insert! bvar {:id "S1" :name nil :status 0 :city nil})
      (insert! bvar {:id "S10" :name nil :status 31 :city nil})
      (is (thrown? IllegalArgumentException (constraint-reset! bvar constraints))))))


(deftest addConst
  (testing "add constraint"
    (let [relation (convertToBats people)
          bvar (batvar relation)]
      (insert! bvar {:id "S1" :name nil :status 0 :city nil})
      (is (thrown? Exception (add-constraint! bvar {:key :id}))))
    (let [relation (convertToBats people)
          bvar (batvar relation {:key :id})]
      (add-constraint! bvar (fn [rel] (and (<= 10 (min (:status rel))) (>= 30 (max (:status rel))))))
      (is (thrown? Exception (insert! bvar {:id "S10" :name "bla" :status 31 :city "blubb"})))
      (is (thrown? Exception (insert! bvar {:id "S1" :name nil :status 0 :city nil}))))))



(deftest foreigntest
  (testing "foreign-keys"
    (let [people-bvar (batvar (convertToBats people))
          foreign-key {:foreign-key {:key :id, :referenced-relvar people-bvar, :referenced-key :id}}
          fail-sp #{{:id "S1" :pno "P1" :qty 300}
                    {:id "S100" :pno "P1" :qty 200}
                    {:id "S1" :pno "P3" :qty 200}
                    {:id "S2" :pno "P2" :qty 200}}]
      (batvar (convertToBats sp) foreign-key)
      (is (thrown? Exception (batvar (convertToBats fail-sp) foreign-key))))))

