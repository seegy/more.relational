(ns relation.hashRel
  (:use [potemkin])
  (:require [relation.hashRel.relation]))


(import-vars
 [relation.hashRel.relation
   scheme
   body
   degree
   sort-rel
   rel
   in?
   dee
   dum
   save-rel
   load-rel
   order]
 [relation.hashRel.operators
   rename
   rename*
   restrict
   project
   project+
   project-
   compose
   union
   intersect
   difference
   divide
   tclose
   group
   ungroup
   wrap
   unwrap
   summarize
   join]
 [relation.hashRel.tools
    relfn]
 [relation.hashRel.relvar
    relvar
    assign!
    insert!
    delete!
    update!
    constraint-reset!
    add-constraint!
    save-relvar
    load-relvar
    save-db
    load-db])
