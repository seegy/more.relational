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
                     (throw (IllegalArgumentException. (str "The key attribute " attrs " is not unique in " @tvar))))))
      (when-not (c @tvar)
         (throw (IllegalArgumentException. (str  "The new value does not satisfy the constraint " (:body (meta c)))))))))



(defn transvar
  ([trans-table]
    (ref trans-table :meta {:constraints nil}))
  ([trans-table constraints]
   (let [ constraints (if (and (coll? constraints) (not (map? constraints))) (set constraints) #{constraints})
          tvar (ref trans-table :meta {:constraints constraints})]
     (check-constraints tvar)
     tvar)))




(defn assign!
  "Assigns a new relation value to a relation variable, but only if it has the
  same type and if all constraints are satisfied. Otherwise, an
  IllegalArgumentException is thrown and the relvar remains unchanged."
  [tvar new-relation]
  (dosync
    (when-not (= (keyorder @tvar) (keyorder new-relation))
      (throw (IllegalArgumentException. "The new value has a different type.")))

    (ref-set tvar new-relation)
    (check-constraints tvar)
    @tvar))




#_(

;; ##################################################################
(def people #{{:id "S1" :name "Smith" :status 20 :city "London"}
      {:id "S2" :name "Jones" :status 10 :city "Paris"}
      {:id "S3" :name "Blake" :status 30 :city "Paris"}
      {:id "S4" :name "Clark" :status 20 :city "London"}
      {:id "S5" :name "Adams" :status 30 :city "Athens"}})

(def relation (tr people))


(def tvar (transvar relation  {:key :id}))
(transvar relation  {:key #{:status :city}})

(transvar relation [{:key :id}
                    (tr-fn [rel] (<= 10 (max rel :status) 30))])

(transvar relation [(tr-fn [rel] (<= 40 (max rel :status) ))])

(restriction relation (tr-fn [t] (< 10 (:status t))))

(assign! tvar (restriction relation (tr-fn [t] (< 10 (:status t)))))


tvar

)
