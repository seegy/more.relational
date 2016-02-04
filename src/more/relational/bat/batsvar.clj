(ns more.relational.bat.batsvar
   (:use [ more.relational.bat.batOperators :as OP ])
   (:use [ more.relational.bat.table :as TAB ])
   (:require [clojure.edn    :as edn])
  (:refer-clojure :exclude [find reverse slice min max update load]))


(defn batvar
  ""
  ([batMap metaMap]
    (ref batMap :meta metaMap))
  ([batMap]
    (batvar batMap {})))




(defn assign!
  ""
  [batVar batMap]
  (dosync
    (when-not (= (set (keys @batVar)) (set (keys batMap)))
      (throw (IllegalArgumentException. "Schema of map of BATs are not equal.")))
    (ref-set batVar batMap)
    batVar))



(defn insert!
  ""
  ([batVar attr head tail]
    (dosync
     (def temp (OP/insert (get @batVar attr) head tail))
     (def newBatMap (assoc @batVar attr temp))
     (ref-set batVar newBatMap)
     batVar))
  ([batVar tuple]
   (when-not (= (set (keys tuple)) (set (keys @batVar)))
       (throw (IllegalArgumentException. "Schemas of map of BATs are not equal.")))
   (let[ newId (inc (apply clojure.core/max(map (fn[[_ bat]] (max (reverse bat))) @batVar)))]
     (map (fn[[name value]] (insert! batVar name newId value)) tuple))))


(defn update!
  ""
  ([batVar attr head oldTail newTail]
   (dosync
    (def temp (OP/update (get @batVar attr) head oldTail newTail))
    (def newBatMap (assoc @batVar attr temp))
    (ref-set batVar newBatMap)
    batVar))
  ([batVar attr old new]
   (dosync
    (def toDelete (select (get @batVar attr) #(= % old)))
    (println toDelete)
    (def newBat  (reduce (fn[bat d] (insert (delete bat (:head d) old) (:head d) new)) (get @batVar attr) toDelete))
    (def newBatMap (assoc @batVar attr newBat))
    (ref-set batVar newBatMap)
    batVar)))


(defn delete!
  ""
  ([batVar attr head tail]
   (dosync
    (def temp (OP/delete (get @batVar attr) head tail))
    (def newBatMap (assoc  @batVar attr temp))
    (ref-set batVar newBatMap)
    batVar))
  ([batVar tuple]
   (dosync
    (def bunsSeq (map (fn[attr] (select (get @batVar attr) #( = % (get tuple attr)))) (keys tuple)))
    (def joined (reduce (fn[a b] (mirror (join a b =))) (mirror (first bunsSeq)) (rest bunsSeq)))
    (def newBatMap (reduce (fn[m [attr bat]](assoc m attr (reduce (fn[bat bun](delete bat (:head bun) (get tuple attr))) bat joined))) @batVar @batVar))
    (ref-set batVar newBatMap)
    batVar)))



(defn makeTable!
  ([orderseq batVar]
   (let [keySeq (vec (keys @batVar))
         batSeq (vec (map (fn[k] (get @batVar k)) keySeq))]
     (TAB/makeTable orderseq keySeq batSeq)))
  ([batVar]
   (makeTable! [] batVar)))


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
