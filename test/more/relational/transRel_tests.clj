(ns more.relational.transRel-tests
   (:use clojure.test)
   (:require [more.relational.transrelational :refer :all])
   (:refer-clojure :exclude [extend update max min]))




(def people #{{:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}})

(def sp #{{:id "S1" :pno "P1" :qty 300}
          {:id "S2" :pno "P1" :qty 200}
          {:id "S1" :pno "P3" :qty 200}
          {:id "S2" :pno "P2" :qty 200}})


(deftest convert-test
  (testing "create relation"
    (let [relation (tr people)]
      (is (-> relation keyorder empty? not))
      (is (-> relation fieldValues empty? not))
      (is (-> relation recordReconst empty? not)))

    (let [relation (tr people)
          relation2 (tr  [:id :name :status :city]
                         [["S1" "Smith" 20 "London"]
                          {:id "S2" :name "Jones" :status 10 :city "Paris"}
                          ["S3" "Blake" 30 "Paris"]
                          ["S4" "Clark" 20 "London"]
                          {:status 30 :name "Adams" :city "Athens" :id "S5"}])]

      (is (= (set (convert relation)) (set (convert relation2)) (set people)))
      (is (= (set relation) (set  relation2) (set people)))))

    (let [xrel #{{:id "S1" :name "" :status 0 :city nil}
                  {:id "S2" :name nil :status nil }
                  {:id "S3" :name nil :status 30 :city "Paris"}}
          relation (tr xrel)]
      (is (= (count (convert relation)) (count xrel) 3)))


  (testing "convert relation"
    (let [relation (tr people)
          converted-result (convert relation)]
      (is (= (set converted-result) (set people))))))



(deftest insert-test
  (testing "new tuple"
     (let [relation (tr people)
          tuple {:id "S6" :name "Jeff" :status 10 :city "Berlin"}
          inserted (insert relation tuple)]
       (is (= (inc (count relation)) (count inserted)))
       (is (= (set (convert inserted)) (conj people tuple)))))
  (testing "duplicate"
    (let [relation (tr people)
          tuple {:id "S5" :name "Adams" :status 30 :city "Athens"}
          inserted (insert relation tuple)]
      (is (= relation inserted)))))



(deftest delete-test
  (testing "one tuple"
    (let [relation (tr people)
          deleted (delete relation  0 0 )]
      (is (= (dec (count relation)) (count deleted)))
      (is (= (set (convert deleted)) (set (remove #(= {:id "S1" :name "Smith" :status 20 :city "London"} %) people))))))
  (testing "delete all"
    (let [relation (tr people)
          deleted (reduce (fn [x _ ](delete x 0 0)) relation (range (count relation)))]
      (is (= (count deleted) 0))
      (is (= (keyorder deleted) (keyorder relation))))))




(deftest search-test
  (testing "restriction"
    (let [relation (tr people)]
      (let [pred (tr-fn [t] (<= 30 (:status t)))
            result (restriction relation pred)
            converted-result (convert result)]
        (is (= 2 (count result)))
        (is (= 2 (count converted-result)))
        (is (every? pred converted-result)))

      (let [pred (tr-fn [t] (and (= "Paris" (:city t)) (<= 30 (:status t))))
            result (restriction relation pred)
            converted-result (convert result)]
        (is (= 1 (count result)))
        (is (= 1 (count converted-result)))
        (is (every? pred converted-result)))

      (let [pred (tr-fn [t] (< 30 (:status t)))
            result (restriction relation pred)]
        (is (= 0 (count result)))
        (is (= (keyorder relation) (keyorder result))))
      (let [pred (tr-fn [t] (#(= (last %1 ) (last %2)) (:name t) (:city t)))
            result (restriction relation pred)]
        (is (= 2 (count result)))
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



(deftest join-test
  (testing "join"
    (let [s (tr people)
          sp (tr sp)
          s-sp (join s sp)]
      (is (= (set (convert s-sp)) #{{:id "S1" :name "Smith" :status 20 :city "London" :pno "P1" :qty 300}
                                    {:id "S2" :name "Jones" :status 10 :city "Paris" :pno "P1" :qty 200}
                                    {:id "S1" :name "Smith" :status 20 :city "London" :pno "P3" :qty 200}
                                    {:id "S2" :name "Jones" :status 10 :city "Paris" :pno "P2" :qty 200}})))))


(deftest projection-test
  (testing "project"
    (let [relation (tr people)
          order [:name :status]
          projected (project relation order)]
      (is (= order (keyorder projected)))
      (is (= (set (convert projected)) #{{:name "Smith" :status 20}
                                         {:name "Jones" :status 10}
                                         {:name "Blake" :status 30}
                                         {:name "Clark" :status 20}
                                         {:name "Adams" :status 30}})))
    (let [relation (tr people)
          order [:city :status]
          projected (project relation order)]
      (is (= order (keyorder projected)))
      (is (= (set (convert projected)) #{{:status 20 :city "London"}
                                         {:status 10 :city "Paris"}
                                         {:status 30 :city "Paris"}
                                         {:status 30 :city "Athens"}}))))
  (testing "project+"
      (let [relation (tr people)
          order [:name :status]
          projected (project relation order)]
      (is (= order (keyorder projected)))
      (is (= (set (convert projected)) #{{:name "Smith" :status 20}
                                         {:name "Jones" :status 10}
                                         {:name "Blake" :status 30}
                                         {:name "Clark" :status 20}
                                           {:name "Adams" :status 30}})))
      (let [relation (tr people)
            order [:city :status]
            projected (project relation order)]
        (is (= order (keyorder projected)))
        (is (= (set (convert projected)) #{{:status 20 :city "London"}
                                         {:status 10 :city "Paris"}
                                         {:status 30 :city "Paris"}
                                         {:status 30 :city "Athens"}}))))
  (testing "extend"
    (let [relation (tr people)
          extention {:one 1}
          extended (extend relation extention)]
      (is (= (keyorder extended) [:id :name :status :city :one]))
      (is (every? #(= 1 (:one %)) (convert extended))))

    (let [relation (tr people)
          extention {"reverse" (tr-fn [t] (clojure.string/reverse (:name t)))}
          extended (extend relation extention)]
      (is (= (keyorder extended) [:id :name :status :city "reverse"]))
      (is (every? #(= (clojure.string/reverse (:name %)) (get % "reverse")) (convert extended))))))







(deftest set-operators
  (let [p1 (tr [{:id "S1" :name "Smith"}
                {:id "S2" :name "Jones" :city "Paris"}
                {:id "S3" :name "Blake" :status 30 }
                {:id "S4" :status 20 }
                {:id "S5"}])

         p2 (tr [{:id "S3" :name "Blake" :status 30 }
                 {:id "S6" :name "Smith" :status 20 :city "London"}
                 {:id "S7" :name "Jones" :status 10 :city "Paris"}])

         p3 (tr [{:id "S8" :name "Blake" :status 30 :city "Paris"}
                  {:id "S9" :name "Clark" :status 20 :city "London"}
                  {:id "S10" :name "Adams" :status 30 :city "Athens"}
                  {:id "S3" :name "Blake" :status 30 }])]

    (testing "union"
      (let [united (union p1 p2)]
        (is (= (keyorder p1) (keyorder united)))
        (is (= 7 (count united)))
        (is (= (set (convert united)) #{{:id "S1" :name "Smith" :status nil :city nil}
                                        {:id "S2" :name "Jones" :status nil :city "Paris"}
                                        {:id "S3" :name "Blake" :status 30 :city nil}
                                        {:id "S4" :name nil     :status 20  :city nil}
                                        {:id "S5" :name nil     :status nil :city nil}
                                        {:id "S6" :name "Smith" :status 20 :city "London"}
                                        {:id "S7" :name "Jones" :status 10 :city "Paris"}})))

      (let [united (union p1 p2 p3)]
        (is (= (keyorder p1) (keyorder united)))
        (is (= 10 (count united)))
        (is (= (set (convert united)) #{{:id "S1" :name "Smith" :status nil :city nil}
                                        {:id "S2" :name "Jones" :status nil :city "Paris"}
                                        {:id "S3" :name "Blake" :status 30 :city nil}
                                        {:id "S4" :name nil     :status 20  :city nil}
                                        {:id "S5" :name nil     :status nil :city nil}
                                        {:id "S6" :name "Smith" :status 20 :city "London"}
                                        {:id "S7" :name "Jones" :status 10 :city "Paris"}
                                        {:id "S8" :name "Blake" :status 30 :city "Paris"}
                                        {:id "S9" :name "Clark" :status 20 :city "London"}
                                        {:id "S10" :name "Adams" :status 30 :city "Athens"}}))))

    (testing "intersection"
      (let [intersected (intersection p1 p2)]
        (is (= (keyorder p1) (keyorder intersected)))
        (is (= 1 (count intersected)))
        (is (= (set  intersected) #{{:id "S3" :name "Blake" :status 30 :city nil}})))

      (let [intersected (intersection p1 p2 p3)]
        (is (= (keyorder p1) (keyorder intersected)))
        (is (= 1 (count intersected)))
        (is (= (set intersected) #{{:id "S3" :name "Blake" :status 30 :city nil}}))))

    (testing "difference"
      (let [diff (difference p1 p2)]
        (is (= (keyorder p1) (keyorder diff)))
        (is (= (set diff)   #{{:id "S1" :name "Smith" :status nil :city nil}
                                        {:id "S2" :name "Jones" :status nil :city "Paris"}
                                        {:id "S4" :name nil     :status 20  :city nil}
                                        {:id "S5" :name nil     :status nil :city nil}})))

      (let [diff (difference p1 p2 p3)]
        (is (= (keyorder p1) (keyorder diff)))
        (is (= (set  diff)   #{{:id "S1" :name "Smith" :status nil :city nil}
                                        {:id "S2" :name "Jones" :status nil :city "Paris"}
                                        {:id "S4" :name nil     :status 20  :city nil}
                                        {:id "S5" :name nil     :status nil :city nil}}))))))




(deftest reftest
  (testing "creating tvar"
    (let [ relation (tr people)
           tvar (transvar relation)]
      (is (= @tvar relation)))
    (let [ relation (tr people)
           constraints #{{:key :id}
                         (tr-fn [rel] (and (<= 10 (min rel :status)) (>= 30 (max rel :status))))}
           tvar (transvar relation constraints)]
      (is (= @tvar relation))
      (is (thrown? IllegalArgumentException (transvar (insert relation {:id "S1" :name nil :status nil :city nil}) constraints)))
      (is (thrown? IllegalArgumentException (transvar (insert relation {:id "S10" :name nil :status 31 :city nil}) constraints))))))

(deftest bla
  (testing "assign!"
    (let [relation (tr people)
          tvar (transvar relation)
          rel2 (restriction relation (tr-fn [t] (< 10 (:status t))))]
      (assign! tvar rel2)
      (is (not (= @tvar relation)))
      (is (= @tvar rel2))
      (is (thrown? IllegalArgumentException (assign! tvar (tr [:id :name] {})))))))

 (deftest blubb
  (testing "insert!"
    (let [relation (tr people)
          tvar (transvar relation)
          tuple {:id "S10" :name "bla" :status 31 :city "blubb"}]
      (insert! tvar tuple)
      (is (contains? (set @tvar) tuple))
      (is (= @tvar (insert @tvar tuple))))
    (let [relation (tr people)
          tvar (transvar relation)
          tuples [{:id "S8" :name "Blake" :status 30 :city "Paris"}
                                        {:id "S9" :name "Clark" :status 20 :city "London"}
                                        {:id "S10" :name "Adams" :status 30 :city "Athens"}]]
      (insert! tvar tuples)
      (is (every? #(contains? (set @tvar) %) tuples))
      (is (every? #(contains? (set @tvar) %) people)))))

(deftest raaa
  (testing "delete!"
    (let [relation (tr people)
          tvar (transvar relation)]
      (delete! tvar (tr-fn [t] (= "London" (:city t))))
      (is (= (count @tvar) 3))
      (is (every? #(not (= "London" (:city %))) (seq @tvar))))))

(deftest lululu
  (testing "update!"
    (let [relation (tr people)
          tvar (transvar relation)]
      (update! tvar (tr-fn [t] (= "London" (:city t))) :city "Manchester")
      (is (= (count @tvar) (count relation)))
      (is (every? #(not (= "London" (:city %))) (set @tvar)))
      (is (= 2 (count (restriction @tvar (tr-fn [t] (= "Manchester" (:city t))))))))))

(deftest hhihi
  (testing "reset constraints"
    (let [relation (tr people)
          constraints #{{:key :id}
                         (tr-fn [rel] (and (<= 10 (min rel :status)) (>= 30 (max rel :status))))}
          tvar (transvar relation constraints)]
      (constraint-reset! tvar nil)
      (insert! tvar {:id "S1" :name nil :status nil :city nil})
      (insert! tvar {:id "S10" :name nil :status 31 :city nil})
      (is (thrown? IllegalArgumentException (constraint-reset! tvar constraints))))))

(deftest nanana
  (testing "add constraint"
    (let [relation (insert (tr people) {:id "S1" :name nil :status nil :city nil})
          tvar (transvar relation)]
      (is (thrown? IllegalArgumentException (add-constraint! tvar {:key :id}))))
    (let [relation (tr people)
          tvar (transvar relation {:key :id})]
      (add-constraint! tvar (tr-fn [rel] (and (<= 10 (min rel :status)) (>= 30 (max rel :status)))))
      (is (thrown? IllegalArgumentException (insert! tvar {:id "S10" :name "bla" :status 31 :city "blubb"}))))))







