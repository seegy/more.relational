(ns wikidata.connector
  (:require [clojure.data.json :as json]))

;; https://www.mediawiki.org/wiki/Wikibase/API
; http://www.wikidata.org/wiki/Special:ListProperties
;
(def baseURL "https://wikidata.org/w/api.php?")

(def defaultParams [["sites" "enwiki"],
                    ["props" "descriptions|labels|claims"],
                    ["language" "en"],
                    ["format" "json"]])

(defn replaceSpace [string]
  (clojure.string/replace string #" " "%20"))


(defn buildParamString [paramList]
  (let [replaceSpace (fn[string] (clojure.string/replace string #" " "%20"))]
  (reduce #(str %1 "&" %2) (map #(str (first %) "=" (replaceSpace (second %))) paramList))))


(defn getEntities[params]
      (json/read-str (slurp (str baseURL (buildParamString params)))))


(defn entitiesByTitle[title]
  (let [params (conj defaultParams ["titles" title] ["action" "wbgetentities"])]
    (getEntities params)))

(defn searchEntities[searchWord]
  (let [params (conj defaultParams ["search" searchWord] ["action" "wbsearchentities"] ["limit" 20])]
    (getEntities params)))


(defn entitiesByIds[id]
  (let [params  (conj defaultParams  ["ids" id] ["action" "wbgetentities"])]
    (getEntities params)))

(def getGender (fn[entity]
                    (let [genders {6581097 "male",
                                   6581072 "female",
                                   1052281 "transgender female",
                                   2449503 "transgender male",
                                   44148 "male animal",
                                   43445 "female animal"}]
                           (get genders
                                (-> entity
                                    (get  "claims")
                                    (get "P21")
                                    (first )
                                    (get "mainsnak")
                                    (get "datavalue")
                                    (get "value")
                                    (get "numeric-id"))))))

(def getBirthDate (fn[entity]
                            (-> entity
                                    (get  "claims")
                                    (get "P569")
                                    (first )
                                    (get "mainsnak")
                                    (get "datavalue")
                                    (get "value")
                                    (get "time"))))

(defn searchFor
  ([title limit]
    (let [
          searchEntities (fn [searchWord]
                           (let [params (conj defaultParams ["search" searchWord] ["action" "wbsearchentities"] ["limit" limit])]
                             (getEntities params)))
          ids (reduce #(conj  %1 (get %2 "id")) [] (get (searchEntities title) "search"))
          connectedIDs (if (empty? ids ) "" (reduce #(str %1 "|" %2)  ids ))
          result (get (entitiesByIds connectedIDs) "entities")
          getValue (fn[id attr]
                    (-> result
                         (get  id)
                         (get attr)
                         (get "en")
                         (get "value")))]
    (reduce #(conj %1 [ %2
                        (getValue %2 "labels")
                        (getValue %2 "descriptions")
                        (getGender (get result %2))
                        (getBirthDate (get result %2))]) [] (keys result))))
  ([title]
   (searchFor title 20)))


(searchFor "Michael Jackson" 20)
