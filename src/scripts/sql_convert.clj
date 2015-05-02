(ns scripts.sqlconvert
  (:use [clojure.string]))

(def file "D:/Downloads/employees/load_employees.dump")
(def targetFolder "D:/Downloads/employees/")

(let [rdr (clojure.java.io/reader file)]
  (let [clj_values {}
        convert (fn[insert]
                  (let [ tablename (second (split (first (split insert #"VALUES ")) #"`"))
                         dataset (replace (replace (replace (replace (second (split insert #"VALUES ")) #"\(" "[")#"\)" "]") #"'" "\"") #";|," " ")]
                    [tablename dataset]))
        addData (fn[[tablename dataset]] (assoc clj_values tablename (conj (get clj_values tablename) dataset)))
        toFile (fn[[tablename dataset]] (spit (str targetFolder tablename ".clj") (str dataset "\n") :append true))]
 (map #(toFile (convert %)) (line-seq rdr))))
