(ns more.relational.hashRel.relation
  (:use [more.relational.hashRel.tools])
  (:require [clojure.edn    :as edn]
            [more.relational.hashRel.tools]))

(declare sort-rel)

(deftype HashRelation [head body]
  Object
  (equals [this obj]
          (cond
           (identical? this obj)
           true

           (not (instance? HashRelation obj))
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
                  sbody (sort (map hash (.body (sort-rel (HashRelation. shead #{}) this))))]
              (+ (* 31 (+ (* 17 31)
                          (hash shead)))
                 (hash sbody))))

  clojure.lang.Seqable
  (seq [this]
       (if (= #{[]} (.body this))
         ; table dee
         (seq #{{}})

         ; just make a sequence
         (seq (.body this))))

  clojure.lang.Counted
  (count [this]
         (count (.body this)))

  clojure.lang.IKeywordLookup ; in Masterarbeit nachgucken: evtl. projektion auf ein attribute
  (getLookupThunk [this key]
                  (reify clojure.lang.ILookupThunk
                    (get [_ target]
                         (set (map (fn [t] (get t key)) (.body target)))))))


(defn scheme
  "Returns the scheme (= vector of attributes) of the relation."
  [relation]
  (.head relation))

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
        (HashRelation.
         (vec (map (fn [a] (get (.head rel2) a)) sorter))
         (.body rel2)))
      )))


(defmethod print-method HashRelation
  [rel writer]
  (.write writer (str "#rel "  (.head rel)
                      "\n" (body rel)) ))


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

         dat  (if (map? dat)
                #{dat}
                dat)

         rows (into #{} (comp
                         (map (fn [t]
                                (if (map? t)
                                  (select-keys t column-names)
                                  (zipmap column-names t))))
                         (filter #(not (empty? %)))
                         (filter coll?)
                         )
                    dat)]
     (HashRelation. (into [] column-names)  rows)))

  ([tuple-set]
   (let [tuples (if (or (empty? tuple-set) (nil? tuple-set))
                  #{}
                  (if (map? tuple-set) #{tuple-set} (set tuple-set)))]
     (let [head (vec (keys (first tuples)))]
       (HashRelation.
        head
        tuples)))))





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
  (edn/read-string {:readers {'rel more.relational.hashRel.relation/rel}} (slurp file)))


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


