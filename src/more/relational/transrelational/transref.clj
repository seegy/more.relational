(ns more.relational.transrelational.transref
  (:use  [more.relational.transrelational.table]
         [more.relational.transrelational.operators])
   (:refer-clojure :exclude [extend update max min ]))


(defn- check-constraints
  ""
  [tvar]
  (doseq [c (:constraints (meta tvar))]
    (if (map? c)
      (let [[ctype attr] (first c)
            attrs (if (set? attr) attr #{attr})]
        (case ctype
              :key (when-not (= (count @tvar) (count (project @tvar attrs)))
                     (throw (IllegalArgumentException. (str "The key attribute " attrs " is not unique in " @tvar)))))))))



(defn transvar
  ([trans-table]
    (ref trans-table :meta {:constraints nil, :referenced-by #{}}))
  ([]))


#_(

;; ##################################################################
(def people #{{:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}})

(def relation (tr people))

(def tvar (ref relation :meta {:constraints [ {:key #{:status :city} }
                                      ], :referenced-by #{}}))

(check-constraints tvar)

   )
