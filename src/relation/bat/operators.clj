(ns relation.bat.bat)

 (defn find
   [batObj head]
   (:tail (first (filter #(= head (:head %)) (buns batObj)))))


 (defn select
   [batObj v1 & {:keys [v2 r1 r2] :or {v2 v1, r1 true, r2 r1}}]
  (let [< (fn[a b] (neg? (compare a b)))
        > (fn[a b] (pos? (compare a b)))
        newBat (into []
             (filter #(let [b (:tail %)]
                       (or
                         (and (or (nil? v1) (< v1 b))
                              (or (nil? v2) (< b v2))
                              (not (and (nil? v1) (nil? v2))))
                         (and r1 (= v1 b))
                         (and r2 (= v2 b))
                         (and (nil? v1) r1 (nil? v2) r2 (nil? b))))
                     (buns batObj)))]
    (bat newBat)))



(defn join
"(bat[H1,T1] AB, bat[T1,T2] CD,str f, ···pi···)"
  ([batObjAB batObjCD f]
   (bat (into [] (map (fn [tupleAB]
                   (first (filter not-empty (map (fn [tupleCD]
                         (if (f (:tail tupleAB) (:head tupleCD))
                           {:head (:head tupleAB)
                            :tail (:tail tupleCD)}))
                       (buns batObjCD)))))
                  (buns batObjAB)))))
  ([batObjAB batObjCD f & more]
   (bat (into [] (map (fn [tupleAB]
                   (first (filter not-empty (map (fn [tupleCD]
                         (if (apply f (conj more (:tail tupleAB) (:head tupleCD)))
                           {:head (:head tupleAB)
                            :tail (:tail tupleCD)}))
                       (buns batObjCD)))))
                  (buns batObjAB))))))


(def nameBAT (reverse (bat [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}])))
(def NameRelationBAT (bat [{:head 1 :tail 2}
                           {:head 1 :tail 3}
                           {:head 2 :tail 3}
                           {:head 3 :tail 2} ]))


(defn join [batAB batCD]
  (let [ AB (map (fn[bun] (clojure.set/rename-keys bun {:tail :key})) (buns batAB))
         CB (map (fn[bun] (clojure.set/rename-keys bun {:head :key})) (buns batCD))
         ks #{:key}
         idx (clojure.set/index AB ks)]
    (bat (reduce (fn [ret x]
           (let [found (idx (select-keys x ks))]
             (if found
               (reduce #(conj %1 (dissoc  (merge %2 x) :key)) ret found)
               ret)))
         [] CB))))

(join nameBAT NameRelationBAT)



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


