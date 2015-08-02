(ns relation.bat
  (:use [potemkin])
  (:require [clojure.repl   :refer :all]
            [clojure.string :as str]
            [clojure.edn    :as edn]
            [relation.bat.batsvar]))



(import-vars
 [relation.bat.table
   bat
   buns
   bat?
   makeTable
   convertToBats]
 [relation.bat.batOperators
   find
   select
   join
   reverse
   mirror
   mark
   project
   slice
   sum
   max
   min
   diff
   union
   intersect
   group
   groupV2
   fragment
   split
   multijoin
   pump
   insert
   delete
   update]
 [relation.bat.batsvar
  temp
  batvar
  assign!])
