(ns relation.bat.batsvar
   (:use [ relation.bat.batOperators :as OP ])
   (:use [ relation.bat.table :as TAB ])
   (:require [clojure.edn    :as edn]))


(defn batvar
  ""
  ([batMap metaMap]
    (ref batMap :meta metaMap))
  ([batMap]
    (batvar batMap {})))




(defn assign!
  ""
  [batRef batMap]
  (dosync
    (when-not (= (set (keys @batRef)) (set (keys batMap)))
      (throw (IllegalArgumentException. "Schema of map of BATs are not equal.")))
    (ref-set batRef batMap)
    batRef))



(defn insert!
  ""
  ([batRef attr head tail]
    (dosync
     (def temp (OP/insert (get @batRef attr) head tail))
     (def newBatMap (assoc @batRef attr temp))
     (ref-set batRef newBatMap)
     batRef))
  ([batRef tuple]
   (when-not (= (set (keys tuple)) (set (keys @batRef)))
       (throw (IllegalArgumentException. "Schemas of map of BATs are not equal.")))
   (let[ newId (inc (apply clojure.core/max(map (fn[[_ bat]] (max (reverse bat))) @batRef)))]
     (map (fn[[name value]] (insert! batRef name newId value)) tuple))))


(defn update!
  ""
  ([batRef attr head oldTail newTail]
   (dosync
    (def temp (OP/update (get @batRef attr) head oldTail newTail))
    (def newBatMap (assoc @batRef attr temp))
    (ref-set batRef newBatMap)
    batRef))
  ([batRef attr old new]
   (dosync
    (def toDelete (select (get @batRef attr) #(= % old)))
    (println toDelete)
    (def newBat  (reduce (fn[bat d] (insert (delete bat (:head d) old) (:head d) new)) (get @batRef attr) toDelete))
    (def newBatMap (assoc @batRef attr newBat))
    (ref-set batRef newBatMap)
    batRef)))


(defn delete!
  ""
  ([batRef attr head tail]
   (dosync
    (def temp (OP/delete (get @batRef attr) head tail))
    (def newBatMap (assoc  @batRef attr temp))
    (ref-set batRef newBatMap)
    batRef))
  ([batRef tuple]
   (dosync
    (def bunsSeq (map (fn[attr] (select (get @batRef attr) #( = % (get tuple attr)))) (keys tuple)))
    (def joined (reduce (fn[a b] (mirror (join a b =))) (mirror (first bunsSeq)) (rest bunsSeq)))
    (def newBatMap (reduce (fn[m [attr bat]](assoc m attr (reduce (fn[bat bun](delete bat (:head bun) (get tuple attr))) bat joined))) @batRef @batRef))
    (ref-set batRef newBatMap)
    batRef)))



(defn makeTable!
  ([orderseq batRef]
   (let [keySeq (vec (keys @batRef))
         batSeq (vec (map (fn[k] (get @batRef k)) keySeq))]
     (TAB/makeTable orderseq keySeq batSeq)))
  ([batRef]
   (makeTable! [] batRef)))


(defn save-batvar
  ""
  [batRef file]
  (spit file (str "#batvar "(prn-str @batRef))))


(defn load-batvar
  ""
  [file]
   (edn/read-string {:readers {'batvar relation.bat.batsvar/batvar
                               'BAT   relation.bat.table/bat}}
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
  (eval (edn/read-string {:readers {'batvar relation.bat.batsvar/batvar
                               'BAT   relation.bat.table/bat}}
                    (slurp file))))
