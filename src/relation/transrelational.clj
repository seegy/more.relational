(ns relation.transrelational
  (:use [potemkin])
  (:require  [relation.transrelational.table]
             [relation.transrelational.operators]
             [relation.transrelational.transref])
   (:refer-clojure :exclude [extend update max min ]))


(import-vars
 [relation.transrelational.table
  tr
  keyorder
  fieldValues
  recordReconst
  fieldValueOf]
 [relation.transrelational.operators
  tr-fn
  retrieve
  convert
  insert
  delete
  update
  project
  project+
  extend
  restrict-fn
  intersection
  union
  difference
  restriction
  inner-compare
  area-search
  not=-scan
  up-to-down-scan
  down-to-up-scan
  point-search
  max
  min
  sum
  join]
 [relation.transrelational.transref])
