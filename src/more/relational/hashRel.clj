(ns more.relational.hashRel
  (:use [potemkin])
  (:require [more.relational.hashRel.relation]
            [more.relational.hashRel.tools]
            [more.relational.hashRel.relvar]
            [more.relational.hashRel.operators]))


(import-vars
 [more.relational.hashRel.relation
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
 [more.relational.hashRel.operators
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
 [more.relational.hashRel.tools
    relfn]
 [more.relational.hashRel.relvar
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
