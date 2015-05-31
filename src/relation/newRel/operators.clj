(ns relation.newRel.relational)


(defprotocol RelationalOperators
  "Protocol for relational operators. If an attribute is given that does not
  exist in the relation, it shall be ignored and not lead to an error.

  If a set of attributes shall be given, it might also be another collection,
  like a vector or a list."
  (rename [relation replace-map]
    "Given a substitution map, it renames the attributes.

    Example:
      (rename r {:pname :product-name})")
  (rename* [relation match-regexp replace-str]
    "Renames all attributes that match match-regexp with replace-str. Semantics
    are the same as clojure.string/replace.

    Example:
      (rename* r #\"(.+)\" \"prefix-$1\"")
  (restrict [relation predicate?]
    "Returns a relation with only the tuples that satisfy the predicate pred?.
    That is a usual function, but fn shall be replaced with relfn, so that
    optimization can be done.

    Examples:
      (restrict r (relfn [t] (= (:sno t) \"S12\")))")
  (project [relation project-map]
    "Returns the relation with only the attributes specified in pmap. That is a
    hash map where the key is the final name and the value is what shall be
    projected. This can be an attribute (can be renamed) or a list, representing
    a function. Attributes must be written as a keyword and every keyword will
    be interpreted as an attribute, if it appears in the original relation. If
    it is a function, it takes a single argument representing a tuple.

    For convenience, project-map can also be a set of attributes, that shall be
    contained in the resulting relation, but now functions or new names can be
    given.

    Examples:
      (project r {:sno :sno, :supplier-city :city})
      (project r #{:sno :city})
      (project r {:sno :sno, :new-status (relfn [t] (* 2 (:status t)))})")
  (project- [relation attributes]
    "Projects the relation with all original attributes, but the one specified.
    Think of it as \"remove\".

    Example:
      (project- r #{:sno})  ; relation r without :sno")
  (project+ [relation extend-map]
    "Extends the relation with the attributes specified in extend-map. In this,
    a key is a new attribute and the value a tuple function. The same effect can
    be achieved with project.

    Examples:
      (project+ r {:new-price (relfn [t] (* 1.05 (:price t)))})
      ; same as
      (project r {:a1 :a1, :a2 :a2, ..., :an :an,
                  :new-price (relfn [t] (* 1.05 (:price t))")
  (join [relation1 relation2]
    "Produces the natural join of both relations.")
  (compose [relation1 relation2]
    "Like join, but the attribute(s) over which is joined are not in the
    resulting relation.")
  (union [relation1 relation2]
    "Combines both relations, provided they have the same type.")
  (intersect [relation1 relation2]
    "Tuples of the returned relation appear in both relations. They must be of
    the same type.")
  (difference [relation1 relation2]
    "If both relations have the same type, it returns relation1 without the
    tuples of relation2.")
  (divide [relation1 relation2]
    "Divide relation1 by relation2.")
  (tclose [binary-relation]
    "Builds the transitive closure on a binary relation.")
  (group [relation group-map]
    "Groups the attributes (group-map values) in a new relation that appears in
    the original relation as the key.

    NOTICE: Name the attributes that shall appear in the group, not by which
            it shall be grouped (as done in SQL).

    Example: Given the relation 'orders' like
    +--------+-----------+-----+
    | BillId | ProductId | Qty |
    +--------+-----------+-----+
    | 7      | 42        | 5   |
    | 5      | 21        | 7   |
    | 5      | 42        | 3   |
    +--------+-----------+-----+

    the statement (group orders {:Positions #{:ProductId :Qty}}) would produce:
    +--------+---------------------+
    | BillId | Positions           |
    +--------+---------------------+
    | 7      | +-----------+-----+ |
    |        | | ProductId | Qty | |
    |        | +-----------+-----+ |
    |        | | 42        | 5   | |
    |        | +-----------+-----+ |
    |        |                     |
    | 5      | +-----------+-----+ |
    |        | | ProductId | Qty | |
    |        | +-----------+-----+ |
    |        | | 21        | 7   | |
    |        | | 42        | 3   | |
    |        | +-----------+-----+ |
    +--------+---------------------+

    In SQL you would say \"GROUP BY BillId\".")
  (ungroup [relation attributes]
    "Inverts group: extracts the attributes (that must be relations) to
    be standard rows again (like you have never done a group).

    Example:
      (ungroup orders #{:Positions})  ; see group")
  (wrap [relation wrap-map]
    "Makes one attribute from several others as specified in wrap-map. The value
    is a set of attributes; the key under which they shall appear.

    Example:
      (wrap r {:address #{:street :zip :city})")
  (unwrap [relation attributes]
    "Takes every attribute from the set attributes and unwraps it, so former
    wrapped attributes are single attributes again like they have never been
    wrapped.

    Example:
      (unwrap r #{:address})  ; see wrap")
  (summarize [relation group-by sum-map]
    "Apply aggregate functions to the relation. group-by is a set of attributes
    by which the result shall be grouped. sum-map's values are functions that
    take a relation body (not a tuple!) as their only parameter. The return
    value appears in the resulting relation under the key.

    Examples:
      (summarize r #{:sno} {:pcount (relfn [r] (count r))})
      ; like in SQL: \"select sno, count(*) as pcount from r group by sno;\"

      (summarize r #{:pno} {:psum (relfn [r] (reduce + (:price r)))})
      ; like in SQL: \"select pno, sum(distinct price) as psum from r
                       group by pno;\""))



; implementation for Relation (clojure data structures, row-oriented)
(extend-protocol RelationalOperators Relation
  (rename [relation smap]
    (Relation.
     (replace smap (.head relation))
     (map #(clojure.set/rename-keys % smap) (.body relation))))


  (rename* [relation match-exp replace-str]
           (let [attrs (map (fn [a]
                                 (-> a name (str/replace match-exp replace-str) keyword))
                               (.head relation))
                 smap (zipmap (.head relation) attrs)]
    (rel (vec attrs) (map #(clojure.set/rename-keys % smap) (.body relation)))))


  #_(restrict [relation predicate]
    (rel (set (filter predicate (seq relation))))) ;; TODO set relevant?

  (restrict [relation predicate]
            (rel (into {} (filter predicate) (.body relation))))



  (project [relation attributes]
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


  (project- [relation attributes]
    (let [attrs (if (set? attributes) attributes (set attributes))
          pos (remove nil? (map #(if (contains? attrs %)
                                    nil
                                    %)
                                (.head relation)))]
      (project relation pos)))


  (project+ [relation extend-map]
    (rel (set (map (fn [t]
                        (apply merge t (map (fn [[k v]] {k (if (or (keyword? v) (fn? v))
                                                             (v t)
                                                             v)}) extend-map)))
                      (seq relation)))))

  #_(join [relation1 relation2]
   (let [common (common-attr relation1 relation2)
          div-r2 (diverging-attr relation2 relation1)
          new-head (vec (concat (.head relation1) div-r2))]
      ; add relation 2 value tuples to that of relation 1
      (rel new-head
        ; tuple join
        (set (apply concat (map (fn [tuple-r1]    ; alternative zu concat : into
                          (remove nil? (map (fn [tuple-r2]
                                                  ; check equality of common attributes
                                                  (if (every? true? (map (fn [attr]
                                                                           (= (get tuple-r1 attr)
                                                                              (get tuple-r2 attr)))
                                                                      common))
                                                    ; join tuples
                                                    (reduce merge tuple-r1 (map (fn [attr]
                                                                                 {attr (get tuple-r2 attr)})
                                                                            div-r2))))
                                                    (.body relation2))))
                                 (.body relation1)))))))



  #_(join [relation1 relation2]
        (if (and (seq relation1) (seq relation2))
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
          (rel [] #{}))) ;TODO head

    (join [relation1 relation2]
        (if (and (seq relation1) (seq relation2))
          (let [ks (clojure.set/intersection (set (.head relation1)) (set (.head relation2)))
                [r s] (if (<= (count relation1) (count relation2))
                        [relation1 relation2]
                        [relation2 relation1])
                idx (clojure.set/index r ks)]
            (rel (reduce (fn [ret x]
                      (let [found (idx (select-keys x ks))]
                        (if found
                          (transduce (comp (merge x)) conj ret found)
                          ret)))
                    #{} s)))
          (rel [] #{}))) ;TODO head




  (compose [relation1 relation2]
    (project- (join relation1 relation2) (common-attr relation1 relation2)))



  (union [relation1 relation2]
    (when-not (same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
      (rel (.head relation1) (clojure.set/union (.body relation1) (.body relation2))))



  (intersect [relation1 relation2]
    (when-not (same-type? relation1 relation2)
      (throw (IllegalArgumentException. "The two relations have different types.")))
      (rel (.head relation1) (clojure.set/intersection (.body relation1) (.body relation2))))



  (difference [relation1 relation2]
      (rel (.head relation1) (clojure.set/difference (.body relation1) (.body relation2))))


    ;### TODO

  (divide [relation1 relation2]
    (let [r1-only-attrs (diverging-attr relation1 relation2)
          r1-only (project relation1 r1-only-attrs)]
      (difference r1-only
                  (project (difference (join r1-only relation2)
                                       relation1)
                           r1-only-attrs))))


    ;### TODO
  (tclose [relation]
    (let [temp (keyword (gensym))
          [a1 a2] (.head relation)]
      (loop [r relation]
        (let [new-rel (union r (rename (compose r (rename r {a2 temp, a1 a2})) {temp a2}))]
          (if (= r new-rel)
              r
              (recur new-rel))))))



  (group [relation group-map]
    (loop [r relation, gmap group-map]
      (if (nil? gmap)
        r
        (let [[alias attributes] (first gmap)
              remaining (remove attributes (.head r))
              new-header (conj (vec remaining) alias)
              tuples-rel (apply merge-with union (map (fn [tuple]
                                                   {(vec (map #(get tuple %) remaining))
                                                    (rel (vec attributes) #{(vec (map #(get tuple %) attributes)) })})
                                                 (.body r)))
              new-body (set (map (fn [[k v]] (conj k v)) tuples-rel))]
          (recur (rel new-header new-body) (next gmap))))))




   (ungroup [relation attributes]
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

  (wrap [relation wrap-map]
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



  (unwrap [relation attributes]
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
;### TODO IAM HERE

  (summarize [relation group-by sum-map]
    (let [group? (not (empty? group-by))
          gsym (keyword (gensym "G_"))
          r (if group? (group relation {gsym (set(attr-complement relation group-by))}) relation)]
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
                         (next summap)))))))))
