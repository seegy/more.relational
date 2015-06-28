(ns relation.bat.bat
  (:require [clojure.repl   :refer :all]
            [clojure.string :as str]
            [clojure.edn    :as edn]))


(load "table")
(load "operators")







(def names [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}])

(def postal-codes [{:head 1 :tail 123}
                 {:head 2 :tail 456}
                 {:head 3 :tail 789} ])

(def date-of-birth [{:head 1 :tail "01-01-2001"}
                    {:head 2 :tail "02-02-2001"}
                    {:head 3 :tail "03-03-2001"}])





(def named-tables {:name (bat names)
                   :postal-code (bat postal-codes)
                   :date-of-birth (bat date-of-birth)})
 (bat names)
 (bat postal-codes)
 (bat date-of-birth)
(bat "li" "la" "lu")

 (def nameBAT (bat [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}]))
 (def NameRelationBAT (bat [{:head 1 :tail 2}
                           {:head 1 :tail 3}
                           {:head 2 :tail 3}
                           {:head 3 :tail 2} ]))




(find nameBAT 2)


(select nameBAT #(= % "Roland"))
(select (bat postal-codes)  #(and (> % 123) (<= % 789)))

(join (reverse nameBAT)  (bat postal-codes) =)
(join NameRelationBAT nameBAT =)
(join NameRelationBAT nameBAT = 2)
(join NameRelationBAT nameBAT <)

(reverse nameBAT)

(mirror nameBAT)
(mirror (reverse nameBAT))

(mark nameBAT :o 10)
(mark nameBAT)

(project nameBAT "Test")

(slice nameBAT 1 1)
(slice nameBAT 0 0)
(slice nameBAT 2 1)
(slice nameBAT 0 2)

(count nameBAT)

(sum NameRelationBAT)

(max NameRelationBAT)

(min NameRelationBAT)

(unique NameRelationBAT)

(def namesBAT2 (bat [{:head 4 :tail "Jake"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}]))

(diff nameBAT namesBAT2)
(diff namesBAT2 nameBAT)

(union nameBAT namesBAT2)

(intersect nameBAT namesBAT2)
(intersect namesBAT2 nameBAT)


 (def AB (bat [{:head 1 :tail 1}
                           {:head 2 :tail 1}
                           {:head 3 :tail 2}
                           {:head 4 :tail 2} ]))

 (def CD (bat [{:head 1 :tail "A"}
                           {:head 2 :tail "A"}
                           {:head 3 :tail "B"}
                           {:head 4 :tail "C"} ]))

 (group AB)
 (group (group AB))
 (group (group AB) CD)


 (def AB  (bat [{:head 1 :tail 1231}
            {:head 2 :tail 323}
            {:head 3 :tail 1}]))

(def CD (bat [{:head 1 :tail 1} {:head 2 :tail 3}]))

(fragment  AB CD)
