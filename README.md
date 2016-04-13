# more.relational

A Clojure library to collect concepts of relational algebra. At the momemt, there are following implementations:

* **HashRel**
  * a *line-based* implementation based on xrel-like relations
* **BatRel**
  * a *column-based* implementation of the binary associated algebra
* **TransRel**
  * a *column-based* implementation of the TransRelational&trade; algebra


## Usage

Clone this repository on your local machine and run `lein install` in directory. Now insert into your `project.clj`-dependencies:

```clojure
[more.relational "0.1.0-SNAPSHOT"]
```


### HashRel

```clojure
(require '[more.relational.hashRel :as hashrel])

(def relation (hashrel/rel [:id "name" :city]
                           [{:id 1, "name" "Adam", :city "London"}  ; takes maps
                            [2 "Blake" "Paris"]                     ; vectors
                            '(3 "Clark" "Athens")]))                ; and lists

; or
(def relation (hashrel/rel #{{:id 1, "name" "Adam", :city "London"}
                             {:id 2, "name" "Blake", :city "Paris"}
                             {:id 3, "name" "Clark", :city "Athens"}}))

(hashrel/restrict relation (hashrel/relfn [t] (= "London" (:city t)))) ; relfn <~> fn
; -> rel #{{:id 1, "name" "Adam", :city "London"}}

(hashrel/project relation [:id :city])
; -> rel #{{:id 1,  :city "London"} {:id 2, :city "Paris"} {:id 3,  :city "Athens"}}

(hashrel/project+ relation {:ytic (hashrel/relfn [t] (clojure.string/reverse (:city t)))})
; -> rel  #{{:id 1, "name" "Adam", :city "London", :ytic "nodnoL"} {:id 2, "name" "Blake", :city "Paris", :ytic "siraP"} {:id 3, "name" "Clark", :city "Athens", :ytic "snehtA"}}

;TODO insert

;TODO relvar

;TODO join?

;TODO delete
```


### BatRel
```clojure
(require '[more.relational.bat :as batrel])

;TODO Create

;TODO Restriction

;TODO insert

;TODO relvar

;TODO join?

;TODO delete
```


### TransRel
```clojure
(require '[more.relational.transrelational :as trel])

;TODO Create

;TODO Restriction

;TODO Projection

;TODO insert

;TODO relvar

;TODO join?

;TODO delete
```



## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.


