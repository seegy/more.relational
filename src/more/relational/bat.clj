(ns more.relational.bat
  (:use [potemkin])
  (:require [clojure.repl   :refer :all]
            [clojure.string :as str]
            [clojure.edn    :as edn]
            [more.relational.bat.batsvar]
            [more.relational.bat.table]
            [more.relational.bat.batOperators])

  (:refer-clojure :exclude [find reverse slice min max update load]))



(import-vars
 [more.relational.bat.table
   bat
   buns
   bat?
   makeTable
   convertToBats]
 [more.relational.bat.batOperators
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
   fragment
   split
   multijoin
   pump
   insert
   delete
   update
   save
   load]
 [more.relational.bat.batsvar
  batvar
  assign!
  insert!
  delete!
  update!
  makeTable!
  constraint-reset!
  add-constraint!
  save-batvar
  load-batvar
  save-db
  load-db])
