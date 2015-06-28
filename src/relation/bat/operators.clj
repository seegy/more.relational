(ns relation.bat.bat)

 (defn find
   [batObj head]
   (:tail (first (filter #(= head (:head %)) (buns batObj)))))



(defn select
  [batObj f & more]
  (let [< (fn[a b] (neg? (compare a b)))
        > (fn[a b] (pos? (compare a b)))
        newBat (into [] (comp
                    (filter #(let [b (:tail %)]
                               (apply f b more)))
                         (map #(assoc  % :tail nil)))
                     (buns batObj))]
    (bat newBat)))


(def NameRelationBAT (bat [{:head 1 :tail 2}
                           {:head 1 :tail 3}
                           {:head 2 :tail 3}
                           {:head 3 :tail 2} ]))

(select NameRelationBAT (fn [x h l] (and (<= x h) (>= x l))) 3 3)


(defn join
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
  [batObj]
  (bat (map (fn [bun] (clojure.set/rename-keys bun {:head :tail, :tail :head})) (buns batObj))))


(defn mirror
  [batObj]
  (bat (map (fn [bun] (assoc bun
                         :tail (:head bun))) (buns batObj))))



(defn mark
  [batObj & {:keys [o] :or {o 0}}]
  (bat (map (fn [bun] (assoc bun
                         :tail (dec (+ o (:head bun))))) (buns batObj))))



(defn project
  [batObj c]
  (bat (map (fn [bun](assoc bun :tail c) ) (buns batObj))))


(defn slice
  [batObj lo hi]
  (bat (into [] (comp (take (+ lo hi))
                      (drop lo ))(buns batObj))))


(defn sum
  [batObj]
  (reduce #(+ %1 (:tail %2)) 0 (buns batObj)))


(defn max
  [batObj]
  (apply clojure.core/max (map #(:tail %) (buns batObj))))



(defn min
  [batObj]
  (apply clojure.core/min (map #(:tail %) (buns batObj))))


(defn unique
  [batObj]
  (bat (set (buns batObj))))


(defn diff
  [batObjAB batObjCD]
  (bat (clojure.set/difference (set (buns batObjAB)) (set (buns batObjCD)))))


(defn union
  [batObjAB batObjCD]
  (bat (clojure.set/union (buns batObjAB) (buns batObjCD))))

(defn intersect
  [batObjAB batObjCD]
  (bat (clojure.set/intersection (set (buns batObjAB)) (set (buns batObjCD)))))

(defn group
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
  (bat (map (fn[bun]{:head (:tail bun)
                :tail (select batAB (fn [x l h] (and (>= x l) (<= x h))) (:head bun) (:tail bun))}) batCD)))


