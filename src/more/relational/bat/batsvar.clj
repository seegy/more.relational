(ns more.relational.bat.batsvar
   (:use [ more.relational.bat.batOperators :as OP ])
   (:use [ more.relational.bat.table :as TAB ])
   (:require [clojure.edn    :as edn])
  (:refer-clojure :exclude [find reverse slice min max update load]))





(defn- check-constraints
  ""
  [bvar]
  (doseq [c (:constraints (meta bvar))]
    (if (map? c)
      (let [[ctype attr] (first c)]
        (case ctype
              :key (when-not   (let [attrs (if (set? attr) attr #{attr})
                                   columns (select-keys @bvar attrs)
                                   with-oid (assoc columns (keyword (gensym "G_")) (mirror (get columns (first attrs))))
                                   table-with-oid (set (makeTable [] (keys with-oid) (vals with-oid)))
                                          table (into #{} (map #(select-keys  % attrs) table-with-oid))]
                               (= (count table) (count table-with-oid)))
                     (throw (IllegalArgumentException. (str "The key attribute " attr " is not unique in " @bvar))))

              :foreign-key (when-not  (let [self-key-bat-mirror (OP/mirror (OP/reverse (get @bvar (:key attr))))
                                           origin-key-bat-mirror (OP/mirror (OP/reverse (get @(:referenced-relvar attr) (:referenced-key attr))))]
                                       (= (count self-key-bat-mirror) (count (OP/join self-key-bat-mirror origin-key-bat-mirror =))))
                             (throw (IllegalArgumentException.
                                     (str "The key given for "
                                        (:key attr)
                                        " does not appear in the referenced relvar at "
                                        (:referenced-key attr)))))))
      (when-not (c @bvar)
         (throw (IllegalArgumentException. (str  "The new value does not satisfy the constraint " (:body (meta c)))))))))




(defn- add-reference!
  "Tell tvar it is referenced by referencer."
  [bvar referencer]
  (alter-meta! bvar assoc :referenced-by (conj (:referenced-by (meta bvar)) referencer)))

(defn- remove-reference!
  "Tell rvar it is no longer referenced by referencer."
  [bvar referencer]
  (alter-meta! bvar assoc :referenced-by (disj (:referenced-by (meta bvar)) referencer)))



(defn batvar
  ""
  ([batMap]
    (ref batMap :meta  {:constraints nil, :referenced-by #{}}))
  ([batMap constraints]
    (let [ constraints (if (and (coll? constraints) (not (map? constraints))) (set constraints) #{constraints})
          references (remove nil? (map #(when (and (map? %) (= :foreign-key (first (keys %))))
                                         (-> % vals first :referenced-relvar))
                                       constraints))
           bvar (ref batMap :meta {:constraints constraints, :referenced-by #{}})]
      (check-constraints bvar)
     ; every relvar this one references to, is "notified"
      (doseq [r references]
        (add-reference! r bvar))
      bvar)))




(defn assign!
  ""
  [batVar batMap]
  (dosync
    (when-not (= (set (keys @batVar)) (set (keys batMap)))
      (throw (IllegalArgumentException. "Schema of map of BATs are not equal.")))
    (ref-set batVar batMap)
   (check-constraints batVar)
    @batVar))



(defn insert!
  ""
  ([batVar attr head tail]
    (let [temp (OP/insert (get @batVar attr) head tail)
          newBatMap (assoc @batVar attr temp)]
     (assign! batVar newBatMap)))
  ([batVar tuple]
   (when-not (= (set (keys @batVar)) (set (keys tuple)))
      (throw (IllegalArgumentException. "Schema of tuple and schema of BATs are not equal.")))
   (let[ newId (inc (apply clojure.core/max(map (fn[[_ bat]] (max (reverse bat))) @batVar)))
         newBatMap (into {} (map (fn[[attr value]] [attr (OP/insert (get @batVar attr) newId value)]) tuple))]
      (assign! batVar newBatMap))))


(defn update!
  ""
  ([batVar attr head oldTail newTail]
   (let [ temp (OP/update (get @batVar attr) head oldTail newTail)
          newBatMap (assoc @batVar attr temp)]
    (assign! batVar newBatMap)))
  ([batVar attr old new]
   (let [toDelete (select (get @batVar attr) #(= % old))
         newBat  (reduce (fn[bat d] (insert (delete bat (:head d) old) (:head d) new)) (get @batVar attr) toDelete)
         newBatMap (assoc @batVar attr newBat)]
    (assign! batVar newBatMap))))


(defn delete!
  ""
  ([batVar attr head tail]
   (let [ temp (OP/delete (get @batVar attr) head tail)
          newBatMap (assoc  @batVar attr temp)]
    (assign! batVar newBatMap)))
  ([batVar tuple]
   (let [ bunsSeq (map (fn[attr] (select (get @batVar attr) #( = % (get tuple attr)))) (keys tuple))
          joined (reduce (fn[a b] (mirror (join a b =))) (mirror (first bunsSeq)) (rest bunsSeq))
          newBatMap (reduce (fn[m [attr bat]](assoc m attr (reduce (fn[bat bun](delete bat (:head bun) (get tuple attr))) bat joined))) @batVar @batVar)]
    (assign! batVar newBatMap))))


(defn constraint-reset!
  "If the new constraints are valid for relvar, it sets them permanently for it."
  [bvar constraints]
  (dosync
    (let [constraints (cond
                       (or (map? constraints) (fn? constraints)) #{constraints}
                       coll? (set constraints)
                       :else nil)
          old-constraints (:constraints (meta bvar))]
      (alter-meta! bvar assoc :constraints constraints)
      (try
        (check-constraints bvar)
        (catch Exception e
          (do (alter-meta! bvar assoc :constraints old-constraints)
              (throw e)))))))




(defn add-constraint!
  "Adds the constraint (see relvar) to a relvar. If the new constraint cannot
  be satisfied by the relvar's value, an exception is thrown and the contraint
  is not added."
  [bvar new-constraint]
  (let [old-cons (:constraints (meta bvar))]
    (constraint-reset! bvar (conj old-cons new-constraint))))


(defn makeTable!
  ([orderseq bvar]
   (let [keySeq (vec (keys @bvar))
         batSeq (mapv (fn[k] (get @bvar k)) keySeq)]
     (TAB/makeTable orderseq keySeq batSeq)))
  ([bvar]
   (makeTable! [] bvar)))


(defn save-batvar
  ""
  [batVar file]
  (spit file (str "#batvar "(prn-str @batVar))))


(defn load-batvar
  ""
  [file]
   (edn/read-string {:readers {'batvar more.relational.bat.batsvar/batvar
                               'BAT   more.relational.bat.table/bat}}
                    (slurp file)))


(defn save-db
  "Saves the database in the specified file. A database is an arbitrary Clojure
  collection, preferrably a hash map."
  [db file]
  (spit file (prn-str (if (map? db)
                        (apply merge (map (fn [[k v]]
                                            {k (list 'batvar (list 'bat (set @v)))})
                                       db))
                        (vec (map (fn [rv]
                                    (list 'batvar (list 'bat (set @rv))))
                               db))))))

(defn load-db
  "Loads a database from the specified file."
  [file]
  (eval (edn/read-string {:readers {'batvar more.relational.bat.batsvar/batvar
                               'BAT   more.relational.bat.table/bat}}
                    (slurp file))))
