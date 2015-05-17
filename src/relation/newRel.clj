(ns relation.newRel
  (:require[clojure.edn :as edn]))


 ;############################################################################################
 ;########################                   TOOLS                          ##################
 ;############################################################################################

(defn same-type?
  "Checks if two relations have the same type, i.e. have the same header."
  [relation1 relation2]
  (=
    (sort (.head relation1))
    (sort (.head relation2))))

(defn same-attr-order?
  "Checks if the attributes of the relations are in the same order. If not or
  if the headers are not equal, it returns false."
  [relation1 relation2]
  (if (and (same-type? relation1 relation2)
        (= (.head relation1) (.head relation2)))
    true
    false))

(defn index-of
  "Finds the position of the item in the collection. Nil if not in there."
  [coll item]
  (let [index (.indexOf coll item)]
    (if (>= index 0)
      index
      nil)))
  #_(let [res (count (take-while (partial not= item) coll))]
     (if (>= res (count coll))
       nil
       res))

(defn sort-vec
  "Creates a vector showing the positions of the attributes in the second
  relation in the order of the first relation header. So the second relation
  can be ordered like the first one. Example:

  header of rel1 = [id name phone]
  header of rel2 = [name phone id]
  (sort-vec rel1 rel2) => [2 0 1]

  Reads like: The new first attribute of rel2 is currently on position 2, etc.

  If the predicate same-attr-order? is true, it will always return [0 1 2 ...]"
  [relation1 relation2]
  (when-not (same-type? relation1 relation2)
    (throw (IllegalArgumentException. "The two relations have different types.")))

  (let [h1 (.head relation1)
        h2 (.head relation2)]
    (vec (map #(index-of h2 %) h1))))

 ;############################################################################################
 ;########################                   NEW REL                        ##################
 ;############################################################################################

(declare sort-rel)

(deftype Relation [head body]
  Object
  (equals [this obj]
    (cond
      (identical? this obj)
      true

      (not (instance? Relation obj))
      false

      (not= (count (.head this)) (count (.head obj)))
      false

      (not= (count (.body this)) (count (.body obj)))
      false

      (and (= (.hashCode this) (.hashCode obj))
           (same-type? this obj)
           (or (and (nil? (.body this)))
               (= (.body this) (.body (sort-rel this obj)))))
      true

      :else false))
  (hashCode [this]
    (let [shead (sort (.head this))
          sbody (sort (map hash (.body (sort-rel (Relation. shead #{}) this))))]
      (+ (* 31 (+ (* 17 31)
                  (hash shead)))
         (hash sbody))))

  clojure.lang.Seqable
  (seq [this]
    (if (= #{[]} (.body this))
      ; table dee
      (seq #{{}})

      ; just make a sequence
      (map (fn [tuple]
           (apply merge (map (fn [attr val]
                               {attr val})
                             (.head this)
                             tuple)))
       (.body this))))

  clojure.lang.Counted
  (count [this]
    (count (.body this)))

  clojure.lang.IKeywordLookup
  (getLookupThunk [this key]
    (reify clojure.lang.ILookupThunk
      (get [_ target]
           (set (map (fn [t] (get t key) (.body target))))))))


(defn scheme
  "Returns the scheme (= set of attributes) of the relation."
  [relation]
  (set (.head relation)))

(defn body
  "Returns the set of value tuples of the relation. Each tuple is a vector with
  its order being the same as in (scheme relation)."
  [relation]
  (.body relation))

(defn degree
  "Returns the degree of a relation, i.e. the number of its attributes."
  [relation]
  (count (.head relation)))

(defn sort-rel
  "If both relations have the same type, a relation equal two rel2 is returned
  with the same attribute order as rel1. If they have different type, it just
  returns rel2."
  [rel1 rel2]
  (if-not (same-type? rel1 rel2)
    rel2
    (if (same-attr-order? rel1 rel2)
      rel2
      (let [sorter (sort-vec rel1 rel2)]
        (Relation.
          (vec (map (fn [a] (get (.head rel2) a)) sorter))
          (.body rel2)))
      )))


(defmethod print-method Relation
  [rel writer]
  ;(.write writer (str "#rel " (pr-str (set (seq rel))))))
  (.write writer (str "#rel " (pr-str  (scheme rel) (body rel)) )))



(defn rel
  "
  Returns a map of type incanter.core.dataset constructed from the given column-names and
  data. The data is either a sequence of sequences or a sequence of hash-maps.
  "
  ([column-names & data]
    (let [dat (cond
                (or (map? (ffirst data)) (coll? (ffirst data)))
                  (first data)
                (map? (first data))
                  data
                :else
                  (map vector (first data)))
          rows (cond
                 (map? dat)
                   [dat]
                 (map? (first dat))
                   dat
                 :else
                   (map #(apply assoc {} (interleave column-names %)) dat))]
      (Relation. (into [] column-names) (set rows))))

  ([tuple-set]
    (let [tuples (if (or (empty? tuple-set) (nil? tuple-set))
                   #{}
                   (if (map? tuple-set) #{tuple-set} tuple-set))]
      (let [head (vec (keys (first tuples)))]
        (print head)(print tuples)
       (Relation.
         head
         tuples)))))

(rel #{ {:id 1, :name "Arthur"} {:id 2, :name "Betty"} })
(rel [:id :name] #{ [1 "Arthur"] [2 "Betty"]})


;  (1) Given as a set of hash maps: #{ {:id 1, :name \"Arthur\"} {:id 2, :name \"Betty\"} }
;  (2) Given as a single hash map: {:id 1, :name \"Arthur\"}
;  (3) Given as head and body: [:id :name] #{ [1 \"Arthur\"] [2 \"Betty\"} }


; ####### I AM HERE

(defn in?
  "Checks if the tuple is containing in the relation"
  [rel tuple]
  (some #(= % tuple) (seq rel)))

(def dee "Represents true." (rel [] #{[]}))
(def dum "Represents false." (rel [] #{}))

(defn save-rel
  "Saves the relation in the specified file."
  [rel file]
  (spit file (str "#rel " (prn-str (set rel)))))

(defn load-rel
  "Loads a relation from the specified file."
  [file]
  (edn/read-string {:readers {'rel core.relational/rel}} (slurp file)))

(defn order
  "Returns the relation as a sorted set. The sorting is defined by the hash
  map, with the key as the attribute to be sorted by and its value either :asc
  or :desc. Example: (sort r {:id :asc})

  Sorting by multiple attributes can be done. sort-map is a vector of maps then,
  in the form shown above. The map defines the primary sorting, the second one
  the secondary sorting, etc. Example:
  (sort r [{:surname :asc} {:prename :desc}])"
  [rel sort-map]
  (apply sorted-set-by
         (fn [t1 t2]
           (loop [smap (if (map? sort-map) [sort-map] sort-map)]
             (let [attr (-> smap first keys first)
                   order (-> smap first vals first)]
               (if (= (t1 attr) (t2 attr))
                 (if (next smap)
                   (recur (next smap))
                   0)
                 (if (= :asc order)
                   (compare (t1 attr) (t2 attr))
                   (compare (t2 attr) (t1 attr)))))))
         (seq rel)))
