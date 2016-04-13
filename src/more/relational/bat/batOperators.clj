(ns more.relational.bat.batOperators
   (:use [more.relational.bat.table])
   (:require [clojure.edn    :as edn])
  (:refer-clojure :exclude [find reverse slice min max update load]))

 (defn find
   "Returns the tail value of first bun for head, or nil if not exists."
   [batObj head]
   (:tail (first (filter #(= head (:head %)) (buns batObj)))))



(defn select
  "Returns a sub-BAT for batObj with entries giving true to f. Function f for matching entries can have additional parameters (f tail_of_bat & more)."
  [batObj f & more]
  (let [newBat (into [] (comp
                    (filter #(let [b (:tail %)]
                               (apply f b more)))
                         (map #(assoc  % :tail nil)))
                     (buns batObj))]
    (bat newBat)))




(defn join
  "Returns a BAT of [tailAB tailCD] by connecting heads of batAB and batCD. Function f for matching pairs can have additinal parameters (f tail_of_AB tail_of_CD & more)."
  [batAB batCD f & params]
     (let [ AB (map (fn[bun] (clojure.set/rename-keys bun {:tail :key})) (buns batAB))
           CD (map (fn[bun] (clojure.set/rename-keys bun {:head :key})) (buns batCD))
           ks #{:key}
           idx (clojure.set/index AB ks)]
      (if (= f =)
        (if (and (>= (count params) 2) (not (apply = params)))
          (bat [])
          (bat (reduce (fn [ret x]
               (let [filtered-idx  (if (empty? params)
                                     idx
                                     (select-keys idx [{:key (first params)}]))
                     found       (filtered-idx (select-keys x ks))]
                 (if found
                   (reduce #(conj %1 (dissoc  (merge %2 x) :key)) ret found)
                   ret)))
             [] CD)))

       (bat (reduce (fn [ret x]
             (let [found  (reduce (fn [m [k v]]
                                   (if (apply f (:key k) (:key (first x)) params)
                                     (apply conj m v)
                                     m)) #{} idx)] (println x  idx found )
               (if (empty? found)
                 ret
                 (reduce #(apply conj %1
                                 (map (fn[y] (dissoc (merge %2 y) :key))
                                      (second x))) ret found))))
           [] (clojure.set/index CD ks))))))




(defn reverse
  "Returns a BAT with switched head and tail for each entry in batObj."
  [batObj]
  (bat (map (fn [bun] (clojure.set/rename-keys bun {:head :tail, :tail :head})) (buns batObj))))



(defn mirror
  "Returns a BAT with same tail as head for each entry in batObj."
  [batObj]
  (bat (map (fn [bun] (assoc bun
                         :tail (:head bun))) (buns batObj))))


(defn mark
  "Returns a BAT with same tails as batObj and new, growing ids in heads starting with o."
  [batObj o]
  (let [os (drop o (range (+ o (count batObj))))
        recurMark (fn[table buns os]
                    (if (empty? buns)
                      table
                      (let [fbun (first buns)
                            fo (first os)
                            newTable (conj table {:head (:head fbun) :tail fo})]
                        (recur newTable (drop 1 buns) (drop 1 os)))))]
    (recurMark [] (buns batObj) os)))





(defn slice
  [batObj lo hi]
  (bat (into [] (comp (take (+ lo hi))
                      (drop lo ))(buns batObj))))


(defn sum
  "Returns the sum of all tails in batObj."
  [batObj]
  (reduce #(+ %1 (:tail %2)) 0 (buns batObj)))


(defn max
  "Returns the highest numeric value of all tails in batObj."
  [batObj]
  (apply clojure.core/max (map #(:tail %) (buns batObj))))



(defn min
  "Returns the lowest numeric value of all tails in batObj."
  [batObj]
  (apply clojure.core/min (map #(:tail %) (buns batObj))))


(defn diff
  "Returns the set difference of batObjAB and batObjCD."
  [batObjAB batObjCD]
  (bat (clojure.set/difference (set (buns batObjAB)) (set (buns batObjCD)))))


(defn union
  "Returns the union of batObjAB and batObjCD."
  [batObjAB batObjCD]
  (bat (clojure.set/union (buns batObjAB) (buns batObjCD))))

(defn intersect
  "Returns the intersection of batObjAB and batObjCD."
  [batObjAB batObjCD]
  (bat (clojure.set/intersection (set (buns batObjAB)) (set (buns batObjCD)))))


(defn group
  "Returns a recursive BAT with sub-BATS grouped by the tail values of batObj. Optionally the returning group BAT by a group BAT AB  and the tails of CD."
  ([batObj]
   (let [tails (distinct (map (fn[tuple](:tail tuple)) batObj))
         tails' (map (fn[tail] (bat (filter #(= (:tail %) tail) batObj))) tails)]
     (bat (map (fn[tuple] {:head (:head (first tuple)) :tail tuple}) tails'))))
  ([AB CD]
   (let [B (distinct (map (fn[tuple] (:tail tuple)) AB))
         groups (vals (reduce (fn[m bun] (let [k (:head bun)
                                         v (:tail bun)]
                                     (if (contains? m v)
                                         (assoc m v (conj (get m v) k))
                                         (assoc m v #{k})))) {} (buns CD)))
         tails'  (reduce (fn[m x](apply conj m x)) []
                         (map (fn[subBat] (filter (fn[x] (not (empty? x)))
                                                  (map (fn[group]
                                                         (filter #(contains? group (:head %))
                                                                 subBat))
                                                       groups)))
                              B))]
    (bat (map (fn[tuple] {:head (:head (first tuple)) :tail (bat tuple)}) tails')))))





(defn fragment
  "Returns a recursive BAT with fragments of batAB. Upper and downer borders of fragments are tails and heads of batCD."
  [batAB batCD]
  (let [AA (mirror batAB)]
    (bat (map (fn[bun]{:head (:tail bun)
                      :tail (select AA (fn [x l h] (and (>= x l) (<= x h))) (:head bun) (:tail bun))}) batCD))))




(defn split
  "Returns a BAT with oid borders of batObj in nearly n-sized fragments."
  [batObj n]
    (let [m (count batObj)
          boundarySize (int (Math/ceil (/ m n)))
          heads (sort (map :head batObj))
          from-to (loop [h heads
                         result []]
                    (if (<=  (count h) boundarySize)
                      (conj result {:head (first h)
                                    :tail (last h)})
                      (recur (drop boundarySize h) (conj result {:head (nth h 0)
                                                                 :tail (nth h (dec boundarySize))}))))]
      (bat from-to)))






(defn multijoin
  "Joins AB and more BATS by its head and applies f on each permutation of joined tails. Returns a BAT with heads and results of f operation."
  [f AB & more] ;; ---> TODO Original hatte zwei Bats vorgegeben, aufgrund vom group ist es hier nur noch 1
  (let [moreList more
       betterAB (map #(assoc % :tail [(:tail %)]) AB)
       recurMultijoin (fn [table thingList]
                      (if (empty? thingList)
                        table
                        (let [firstThing (first thingList)
                               newTable    (if (bat? firstThing)
                                             (let [ks #{:head}
                                                   heads (clojure.set/index table ks)]
                                                (reduce (fn [ret x]
                                                             (let [found (heads (select-keys x ks))]
                                                               (if found
                                                                 (reduce #(conj %1 {:head (:head %2) :tail (conj (:tail %2) (:tail x))}) ret found)
                                                                 ;(println found x)
                                                                 ret)))
                                                           [] firstThing))

                                             (map (fn[bun](assoc bun :tail (conj (:tail bun) firstThing))) table))]
                           (recur newTable (next thingList)))))
        paramMap (recurMultijoin betterAB moreList)]
    (bat (map (fn[bun](assoc bun :tail (apply f (:tail bun)))) paramMap))))



(defn pump
  "Groups AB by heads of CD and applies f on each group BAT. Returns a BAT with heads and results of f operation."
  [f AB CD]
  (let [joined (join (mirror CD) AB =)
        groups (reduce (fn [m bun]
                         (let [entry (get m (:head bun))]
                           (if (nil? entry)
                             (assoc m (:head bun) [{:head (:tail bun) :tail (:tail bun)}])
                             (assoc m (:head bun) (conj entry {:head (:tail bun) :tail (:tail bun)})))))
                       {} joined)
        groupsBat  (map (fn [[k v]] {:head k :tail (f (bat v))}) groups)]
   (bat groupsBat)))





(defn delete
  " delete {:head a, :tail b} from AB "
  [AB a b]
  (diff AB (bat [{:head a :tail b}])))



(defn insert
  " add {:head a, :tail b} to AB "
  [AB a b]
  (union AB (bat [{:head a :tail b}])))


(defn update
  "--> (delete AB a b) & (insert AB a c)"
  [AB a b c]
  (insert (delete AB a b) a c))



(defn save
  "Saves BAT AB as file. "
  [AB file]
  (spit file (str "#BAT "  (buns AB))))

(defn load
  "Reads file and returns value as BAT"
  [file]
   (edn/read-string {:readers {'BAT   more.relational.bat.table/bat}}
                    (slurp file)))

