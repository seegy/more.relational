(ns relation.transrelational
  (:use [potemkin])
  (:use [relation.transrelational.transref]))


(import-vars
 [relation.transrelational.table
  tr]
 [relation.transrelational.operation]
 [relation.transrelational.transref])
