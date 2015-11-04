(ns relation.bat
  (:use [potemkin])
  (:require [clojure.repl   :refer :all]
            [clojure.string :as str]
            [clojure.edn    :as edn]
            [relation.bat.batsvar]
            [relation.bat.table]
            [relation.bat.batOperators]))



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
   update
   save
   load]
 [relation.bat.batsvar
  batvar
  assign!
  insert!
  delete!
  update!
  makeTable!
  save-batvar
  load-batvar
  save-db
  load-db])
