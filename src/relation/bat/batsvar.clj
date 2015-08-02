(ns relation.bat.batsvar
   (:use [relation.bat.batOperators])
   (:require  [relation.bat.table]))

(defn temp[])


(defn batvar
  ""
  ([batMap metaMap]
    (ref batMap :meta metaMap)
  ([batMap]
    (batvar batMap {})))




(defn assign!
  ""
  [batRef batMap]
  (dosync
    (when-not (= (keys @batRef) (keys batMap))
      (throw (IllegalArgumentException. "Schema of map of BATs are not equal.")))
    (ref-set batRef batMap)
    batRef))



(defn insert!
  ""
  ([batRef attr head tail]
  (dosync
   (def temp (insert (get @batRef attr) head tail))
   (def newBatMap (assoc @batRef attr temp))
   (ref-set batRef newBatMap)
   batRef)))

(def bestellungen [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}
                   {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}
                   {:id 4 :name "Max" :artikel "Hammer" :menge 1 :datum "1.2.01"}
                   {:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])

(keys {:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"})

(def bestellungenBat (convertToBats bestellungen))

(def batRef (batvar bestellungenBat))

(insert! batRef :id 5 5)
(insert! batRef :name 5 "Alan")
(insert! batRef :artikel 5 "Säge")
(insert! batRef :menge 5 1)
(insert! batRef :datum 5 "Gestern")


(defn makeTable!
  ([orderseq batRef]
   (let [keySeq (vec (keys @batRef))
         batSeq (vec (map (fn[k] (get @batRef k)) keySeq))]
     (makeTable orderseq keySeq batSeq)))
  ([batRef]
   (makeTable! [] batRef)))

(makeTable! batRef)
