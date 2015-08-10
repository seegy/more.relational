(ns relation.transrelational
  (:use [potemkin])
  (:require  [relation.transrelational.table]
             [relation.transrelational.operations]
             [relation.transrelational.transref]))


(import-vars
 [relation.transrelational.table
  tr]
 [relation.transrelational.operations]
 [relation.transrelational.transref])
