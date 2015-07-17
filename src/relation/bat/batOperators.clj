(ns relation.bat.batOperators
   (:use [relation.bat.table]))

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
  ([batAB batCD f & more]
  (let [ AB (map (fn[bun] (clojure.set/rename-keys bun {:tail :key})) (buns batAB))
         CD (map (fn[bun] (clojure.set/rename-keys bun {:head :key})) (buns batCD))
         ks #{:key}
         idx (clojure.set/index AB ks)]
    (bat (reduce (fn [ret x]
           (let [found (first (filter not-empty (map (fn [[k v]]
                                                        (if (apply f (:key k) (:key x) more)
                                         v
                                         nil)) idx)))]
             (if found
              (reduce #(conj %1 (dissoc  (merge %2 x) :key)) ret found)
               ret)))
         [] CD))))
   ([batAB batCD f]
     (let [ AB (map (fn[bun] (clojure.set/rename-keys bun {:tail :key})) (buns batAB))
           CD (map (fn[bun] (clojure.set/rename-keys bun {:head :key})) (buns batCD))
           ks #{:key}
           idx (clojure.set/index AB ks)]
      (if (= f =)

        (bat (reduce (fn [ret x]
             (let [found (idx (select-keys x ks))]
               (if found
                 (reduce #(conj %1 (dissoc  (merge %2 x) :key)) ret found)
                 ret)))
           [] CD))

       (bat (reduce (fn [ret x]
             (let [found (first (filter not-empty (map (fn [[k v]]
                                                          (if  (f (:key k) (:key x))
                                           v
                                           nil)) idx)))]
               (if found
                (reduce #(conj %1 (dissoc  (merge %2 x) :key)) ret found)
                 ret)))
           [] CD))))))




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
  ""
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



;TODO gibts im orig. Script garnicht!
(defn project
  ""
  [batObj c]
  (bat (map (fn [bun](assoc bun :tail c) ) (buns batObj))))



(defn slice
  ""
  [batObj lo hi]
  (bat (into [] (comp (take (+ lo hi))
                      (drop lo ))(buns batObj))))


(defn sum
  ""
  [batObj]
  (reduce #(+ %1 (:tail %2)) 0 (buns batObj)))


(defn max
  ""
  [batObj]
  (apply clojure.core/max (map #(:tail %) (buns batObj))))



(defn min
  ""
  [batObj]
  (apply clojure.core/min (map #(:tail %) (buns batObj))))


(defn diff
  ""
  [batObjAB batObjCD]
  (bat (clojure.set/difference (set (buns batObjAB)) (set (buns batObjCD)))))


(defn union
  ""
  [batObjAB batObjCD]
  (bat (clojure.set/union (buns batObjAB) (buns batObjCD))))

(defn intersect
  ""
  [batObjAB batObjCD]
  (bat (clojure.set/intersection (set (buns batObjAB)) (set (buns batObjCD)))))

(defn group
  ""
  ([batObj]
   (let [id (fn [tail] (:head (first (filter #(= ( :tail %) tail) batObj))))]
     (bat (map (fn[bun](assoc bun :tail (id (:tail bun)))) batObj))))
  ([batObjAB batObjCD]
   (let [joined (map (fn [bunAB]
                       (first (filter not-empty (map (fn [bunCD]
                                        (if (= (:head bunAB) (:head bunCD))
                                          {:a (:head bunAB)
                                           :b (:tail bunAB)
                                           :d (:tail bunCD)}
                                          ()
                                          )) batObjCD)))) batObjAB)
         id (fn [b d] (:a (first (filter #(and (= (:b %) b) (= (:d %) d)) joined))))]
     (bat (map (fn[tuple]{:head (:a tuple) :tail (id (:b tuple) (:d tuple))}) joined)))))



(defn fragment
  ""
  [batAB batCD]
  (let [AA (mirror batAB)]
    (bat (map (fn[bun]{:head (:tail bun)
                      :tail (select AA (fn [x l h] (and (>= x l) (<= x h))) (:head bun) (:tail bun))}) batCD))))

; Brauchbar? Wofür n?
(defn split
  ""
  [batObj n]
    (let [allPairs  (flatten (map (fn [bunA](map (fn [bunB]{:head (:head bunA) :tail (:head bunB) }) batObj )) batObj))]
      (into [] (comp
                (filter #(<= (:head %) (:tail %))))
            allPairs)))






(defn multijoin
  ""
  [f AB CD & more]
  (let [moreList (cons CD more)
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
  ""
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

