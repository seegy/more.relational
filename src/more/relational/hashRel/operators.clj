(ns more.relational.hashRel.operators
  (:use [more.relational.hashRel.relation ])
  (:require [more.relational.hashRel.tools :as tools]
            [clojure.string :as str]))


  (defn rename [relation smap]
    (rel
     (replace smap (.head relation))
     (set (map #(clojure.set/rename-keys % smap) (.body relation)))))


  (defn rename* [relation match-exp replace-str]
           (let [attrs (map (fn [a]
                                 (-> a name (str/replace match-exp replace-str) keyword))
                               (.head relation))
                 smap (zipmap (.head relation) attrs)]
    (rel (vec attrs) (map #(clojure.set/rename-keys % smap) (.body relation)))))


  (defn restrict [relation predicate]
            (rel (set (filter predicate (.body relation)))))



  (defn project [relation attributes]
    (if (map? attributes)
      ; attributes is a hash map
      (let [head (vec (keys attributes))
            ; seq with functions that return the correct value for the position
            new-rel (set
                     (map (fn [t]
                            (apply merge
                                   (map (fn [[k v]] {k (if (or
                                                            (keyword? v) (fn? v))
                                                         (v t) v)}) attributes))) (.body relation)))]
        (rel (keys attributes) new-rel))
      ; attributes is a set/vector/list
      (let [value-tuples (set (map #(vec (map (fn [p] (% p)) attributes))
                                (.body relation)))]
        (rel attributes value-tuples))))


  (defn project- [relation attributes]
    (let [attrs (if (set? attributes) attributes (set attributes))
          pos (remove nil? (map #(if (contains? attrs %)
                                    nil
                                    %)
                                (.head relation)))]
      (project relation pos)))


  (defn project+ [relation extend-map]
    (rel (set (map (fn [t]
                        (apply merge t (map (fn [[k v]] {k (if (or (keyword? v) (fn? v))
                                                             (v t)
                                                             v)}) extend-map)))
                      (seq relation)))))


  (defn join [relation1 relation2]
        (if (and (seq relation1) (seq relation2)) ; Both args are resolveable to seq
          (let [ks (clojure.set/intersection (set (.head relation1)) (set (.head relation2)))
                [r s] (if (<= (count relation1) (count relation2))
                        [relation1 relation2]
                        [relation2 relation1])
                idx (clojure.set/index r ks)]
            (rel (reduce (fn [ret x]
                      (let [found (idx (select-keys x ks))]
                        (if found
                          (reduce #(conj %1 (merge %2 x)) ret found)
                          ret)))
                    #{} s)))
          (if (seq relation1) ; cases for not relations
            relation1
            (if (seq relation2)
              relation2
               (rel [] #{})))))


  (defn compose [relation1 relation2]
    (project- (join relation1 relation2) (tools/common-attr relation1 relation2)))



  (defn union [relation1 relation2]
    (when-not (tools/same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
      (rel (.head relation1) (clojure.set/union (.body relation1) (.body relation2))))



  (defn intersect [relation1 relation2]
    (when-not (tools/same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
      (rel (.head relation1) (clojure.set/intersection (.body relation1) (.body relation2))))



  (defn difference [relation1 relation2]
      (rel (.head relation1) (clojure.set/difference (.body relation1) (.body relation2))))


    ;### TODO maybe divide give not correct results

  (defn divide [relation1 relation2]
    (let [r1-only-attrs (tools/diverging-attr relation1 relation2)
          r1-only (project relation1 r1-only-attrs)]
      (difference r1-only
                  (project (difference (join r1-only relation2)
                                       relation1)
                           r1-only-attrs))))


    ;### TODO
  (defn tclose [relation]
    (let [temp (keyword (gensym))
          [a1 a2] (.head relation)]
      (loop [r relation]
        (let [new-rel (union r (rename (compose r (rename r {a2 temp, a1 a2})) {temp a2}))]
          (if (= r new-rel)
              r
              (recur new-rel))))))



  (defn group [relation group-map]
    (loop [r relation, gmap group-map]
      (if (nil? gmap)
        r
        (let [[alias attributes] (first gmap)
              remaining (remove attributes (.head r))
              new-header (conj (vec remaining) alias)
              tuples-rel (apply merge-with union (map (fn [tuple]
                               (let [xf (comp (map #(get tuple %)))]
                               {(into [] xf remaining)
                                (rel (vec attributes)
                                     #{ (into [] xf attributes) })})) (.body r)))
              new-body (set (map (fn [[k v]] (conj k v)) tuples-rel))]
          (recur (rel new-header new-body) (next gmap))))))




   (defn ungroup [relation attributes]
    (loop [r relation, attrs attributes]
      (if (nil? attrs)
          r
          (let [target (first attrs)
                _ (when-not (empty? (clojure.set/intersection
                                      (set (.head r))
                                      (set (.head (get (first (.body r)) target)))))
                    (throw (IllegalStateException.
                             "There are attributes in the inner relation that already are in the outer one.")))
                rem-attrs (remove #{target} (.head r))
                new-head (vec (concat rem-attrs
                                 (.head (get (first (.body r)) target))))
                 new-body  (set (apply concat (map (fn [tuple]
                                   (let [beginning (reduce (fn [map attr] (assoc map attr (get tuple attr))) {}
                                                                   rem-attrs)]
                                     (map (fn [inner-tuple]
                                           (merge beginning inner-tuple))
                                          (-> tuple (get target) .body))
                                     ))
                                 (.body r))))]
            (recur (rel new-head new-body)
                   (next attrs))))))

  (defn wrap [relation wrap-map]
    (loop [r relation, wrapper wrap-map]
      (if (nil? wrapper)
          r
          (let [[new-attr old-attrs] (first wrapper)
                rem-pos (remove old-attrs (.head r))
                new-head (concat rem-pos [new-attr])
                new-body (map (fn [tuple]
                               (let [make-map (fn [key-list](reduce (fn [m k] (assoc m k (get tuple k))) {} key-list))
                                     rem-tuple-stuff (make-map rem-pos)
                                     inner-tuple (make-map old-attrs)]
                                 (assoc rem-tuple-stuff new-attr inner-tuple))) (.body r))]
            (recur (rel new-head new-body)
                   (next wrapper))))))



  (defn unwrap [relation attributes]
    (loop [r relation, attrs attributes]
            (if (nil? attrs)
          r
          (let [attr (first attrs)
                _ (when-not (empty? (clojure.set/intersection
                                      (set (.head r))
                                      (set (keys (get (first (.body r)) attr)))))
                    (throw (IllegalStateException.
                             "There are attributes in the inner relation that already are in the outer one.")))
                new-attrs (-> r .body first (get attr) keys)
                rem-attrs (remove #{attr} (.head r))
                new-head (concat (vec rem-attrs) new-attrs)
                new-body (map (fn [tuple]
                                  (reduce (fn [m k] (assoc m k (get (get tuple attr) k)) )
                                          (reduce (fn [rem-m rem-k] (assoc rem-m rem-k (get tuple rem-k))) {} rem-attrs)
                                          new-attrs))
                                  (.body r))]
            (recur (rel new-head new-body)
                   (next attrs))))))


  (defn summarize [relation group-by sum-map]
    (let [group? (not (empty? group-by))
          gsym (keyword (gensym "G_"))
          r (if group? (group relation {gsym (set(tools/attr-complement relation group-by))}) relation)]
      (if group?
          ; with group by
          (loop [new-rel r
                 summap sum-map]
            (if (nil? summap)
                (project- new-rel [gsym])
                (let [[name fun] (first summap)
                      new-head (conj (.head new-rel) name)
                      new-body (set (map (fn [tuple]
                                      (let [new-val (fun (get tuple gsym))]
                                        (assoc tuple name new-val)))
                                    (.body new-rel)))]
                  (recur (rel new-head new-body)
                         (next summap)))))

          ; no group by attributes
          (loop [new-rel (rel [] #{[]})
                 summap sum-map]
            (if (nil? summap)
                new-rel
                (let [[name fun] (first summap)]
                  (recur (project+ new-rel {name (fun r)})
                         (next summap))))))))
