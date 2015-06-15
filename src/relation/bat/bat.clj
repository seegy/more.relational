(ns relation.bat.bat
  (:require [clojure.repl   :refer :all]
            [clojure.string :as str]
            [clojure.edn    :as edn]))


(load "table")
(load "operators")







(def names [{:head 1 :tail "John"}
            {:head 2 :tail "Jane"}
            {:head 3 :tail "Bob"}])

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

 (def nameBAT (bat names))
 (def NameRelationBAT (bat [{:head 1 :tail 2}
                           {:head 1 :tail 3}
                           {:head 2 :tail 3}
                           {:head 3 :tail 2} ]))




 (find nameBAT 2)



; (select nameBAT :bla :r1 false :r2 true)
(select nameBAT "Jane" :v2 "John" :r1 false)
(select (bat postal-codes) 123 :v2 789 :r1 false :r2 true)


(join  NameRelationBAT nameBAT)

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
            {:head 2 :tail "Jane"}
            {:head 3 :tail "Bob"}]))

(diff nameBAT namesBAT2)
(diff namesBAT2 nameBAT)

(union nameBAT namesBAT2)

