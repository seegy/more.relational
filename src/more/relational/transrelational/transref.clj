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


(defn insert!
  "Inserts the tuple (or set of tuples) into relvar."
  [tvar tuple]
  (let [new-rel (tr tuple)]
    (assign! tvar (union @tvar new-rel))))




(defn delete!
  "Deletes tuples from relvar, for which the tuple predicate returns true. Use
  relfn to produce the predicate to enable optimization. It takes a single tuple
  as its argument."
  [tvar pred?]
  (let [dif-rel (restriction @tvar pred?)]
    (assign! tvar (difference @tvar dif-rel))))



#_(defn update!
  "Updates tuples in relvar, for which the tuple predicate is true. The
  value at attribute is then changed to new-value. This can be a fixed value or
  a tuple function. Use relfn for predicate."
  [tvar pred? attribute new-value]
  (assign! tvar (rel (set (map (fn [t]
                                 (if (pred? t)
                                   (assoc t attribute (if (fn? new-value) (new-value t) new-value))
                                   t))
                            (seq @rvar))))))

#_(defn constraint-reset!
  "If the new constraints are valid for relvar, it sets them permanently for it."
  [rvar constraints]
  (dosync
    (let [constraints (if (or (map? constraints) (fn? constraints))
                        [constraints]
                        constraints)
          old-constraints (:constraints (meta rvar))]
      (alter-meta! rvar assoc :constraints constraints)
      (try
        (check-constraints rvar)
        (catch Exception e
          (do (alter-meta! rvar assoc :constraints old-constraints)
              (throw e))))

      ; constraints ok, take care of references
      (let [find-references (fn [cs] (set (remove nil? (map #(when (and (map? %)
                                                                   (= :foreign-key (first (keys %))))
                                                          (-> % vals first :referenced-relvar))
                                                       cs))))
            old-refs (find-references old-constraints)
            new-refs (find-references constraints)]
        (doseq [r (clj-set/difference old-refs new-refs)]
          (remove-reference! rvar r))
        (doseq [r (clj-set/difference new-refs old-refs)]
          (add-reference! rvar r))))))

#_(defn add-constraint!
  "Adds the constraint (see relvar) to a relvar. If the new constraint cannot
  be satisfied by the relvar's value, an exception is thrown and the contraint
  is not added."
  [rvar new-constraint]
  (let [old-cons (:constraints (meta rvar))]
    (constraint-reset! rvar (conj old-cons new-constraint))))

#_(defn save-relvar
  "Saves the relvar in the specified file."
  [relv file]
  (spit file (str "#relvar #rel " (prn-str (set @relv)))))

#_(defn load-relvar
  "Loads a relvar from the specified file."
  [file]
  (edn/read-string {:readers {'relvar core.relational/relvar
                              'rel    core.relational/rel}}
                   (slurp file)))

#_(defn save-db
  "Saves the database in the specified file. A database is an arbitrary Clojure
  collection, preferrably a hash map."
  [db file]
  (spit file (prn-str (if (map? db)
                        (apply merge (map (fn [[k v]]
                                            {k (list 'relvar (list 'rel (set @v)))})
                                       db))
                        (vec (map (fn [rv]
                                    (list 'relvar (list 'rel (set @rv))))
                               db))))))

#_(defn load-db
  "Loads a database from the specified file."
  [file]
  (eval (edn/read-string {:readers {'relvar core.relational/relvar
                                    'rel    core.relational/rel}}
          (slurp file))))



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
