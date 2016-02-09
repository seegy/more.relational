(ns more.relational.transrelational.transref
  (:use  [more.relational.transrelational.table]
         [more.relational.transrelational.operators])
   (:require [clojure.edn    :as edn])
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



(defn update!
  "Updates tuples in relvar, for which the tuple predicate is true. The
  value at attribute is then changed to new-value. This can be a fixed value or
  a tuple function. Use relfn for predicate."
  [tvar pred? attribute new-value]
  (let [to-update (set (restriction @tvar pred?))
        changed (set (map (fn[t] (assoc t attribute (if (fn? new-value) (new-value t) new-value))) to-update))]
    (assign! tvar (union (difference @tvar to-update) changed))))



(defn constraint-reset!
  "If the new constraints are valid for relvar, it sets them permanently for it."
  [tvar constraints]
  (dosync
    (let [constraints (cond
                       (or (map? constraints) (fn? constraints)) #{constraints}
                       coll? (set constraints)
                       :else nil)
          old-constraints (:constraints (meta tvar))]
      (alter-meta! tvar assoc :constraints constraints)
      (try
        (check-constraints tvar)
        (catch Exception e
          (do (alter-meta! tvar assoc :constraints old-constraints)
              (throw e)))))))




(defn add-constraint!
  "Adds the constraint (see relvar) to a relvar. If the new constraint cannot
  be satisfied by the relvar's value, an exception is thrown and the contraint
  is not added."
  [tvar new-constraint]
  (let [old-cons (:constraints (meta tvar))]
    (constraint-reset! tvar (conj old-cons new-constraint))))



;TODO saving constraints?
(defn save-transvar
  "Saves the relvar in the specified file."
  [tvar file]
  (spit file (str "#transvar #tr " (prn-str (set @tvar)))))



(defn load-transvar
  "Loads a relvar from the specified file."
  [file]
  (edn/read-string {:readers {'transvar more.relational.transrelational.transref/transvar
                              'tr    more.relational.transrelational.table/tr}}
                   (slurp file)))



(defn save-db
  "Saves the database in the specified file. A database is an arbitrary Clojure
  collection, preferrably a hash map."
  [db file]
  (spit file (prn-str (if (map? db)
                        (apply merge (map (fn [[k v]]
                                            {k (list 'transvar (list 'tr (set @v)))})
                                       db))
                        (vec (map (fn [rv]
                                    (list 'transvar (list 'tr (set @rv))))
                               db))))))



(defn load-db
  "Loads a database from the specified file."
  [file]
  (eval (edn/read-string {:readers {'transvar more.relational.transrelational.transref/transvar
                                    'tr    more.relational.transrelational.table/tr}}
          (slurp file))))


