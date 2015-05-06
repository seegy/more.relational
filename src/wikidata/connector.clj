(ns wikidata.connector
  (:require [clojure.data.json :as json]))

;; https://www.mediawiki.org/wiki/Wikibase/API
;
(def baseURL "https://wikidata.org/w/api.php?")

(def defaultParams [["sites" "enwiki"],
                    ["props" "descriptions|labels"],
                    ["language" "en"],
                    ["format" "json"]])

(defn replaceSpace [string]
  (clojure.string/replace string #" " "_"))


(defn buildParamString [paramList]
  (let [replaceSpace (fn[string] (clojure.string/replace string #" " "_"))]
  (reduce #(str %1 "&" %2) (map #(str (first %) "=" (replaceSpace (second %))) paramList))))


(defn getEntities[params]
      (json/read-str (slurp (str baseURL (buildParamString params)))))


(defn entitiesByTitle[title]
  (let [params (conj defaultParams ["titles" title] ["action" "wbgetentities"])]
    (getEntities params)))

(defn searchEntities[searchWord]
  (let [params (conj defaultParams ["search" searchWord] ["action" "wbsearchentities"])]
    (getEntities params)))


(defn entitiesByIds[id]
  (let [params  (conj defaultParams  ["ids" id] ["action" "wbgetentities"])]
    (getEntities params)))

(defn searchFor [title]
  (let [ids (reduce #(conj  %1 (get %2 "id")) [] (get (searchEntities title) "search"))
        connectedIDs (reduce #(str %1 "|" %2) ids)
        result (get (entitiesByIds connectedIDs) "entities")
        getValue (fn[id attr]
                   (get (get (get (get result id) attr) "en") "value"))]
  (reduce #(conj %1 [ %2
                      (getValue %2 "labels")
                      (getValue %2 "descriptions")]) [] (keys result))))

(searchFor "jackson")
