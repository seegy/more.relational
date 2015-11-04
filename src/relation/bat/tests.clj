(ns wayneBat
  (:use [relation.bat]))




(bat [{:head 1 :tail "Roland"}{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}])


(def names (bat [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}]) )

(def postal-codes (bat[{:head 1 :tail 123}
                 {:head 2 :tail 456}
                 {:head 3 :tail 789} ]))

(def date-of-birth (bat[{:head 1 :tail "01-01-2001"}
                    {:head 2 :tail "02-02-2001"}
                    {:head 3 :tail "03-03-2001"}]))





(def named-tables {:name  names
                   :postal-code  postal-codes
                   :date-of-birth date-of-birth})

(bat "li" "la" "lu")

 (def nameBAT (bat [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}]))


 (def NameRelationBAT (bat [{:head 1 :tail 2}
                           {:head 1 :tail 3}
                           {:head 2 :tail 3}
                           {:head 3 :tail 2} ]))




(find nameBAT 2)


(select nameBAT #(= % "Roland"))
(select postal-codes  #(and (> % 123) (<= % 789)))

(def NameRelationBAT (bat [{:head 1 :tail 2}
                           {:head 1 :tail 3}
                           {:head 2 :tail 3}
                           {:head 3 :tail 2} ]))

(select NameRelationBAT (fn [x h l] (and (<= x h) (>= x l))) 3 3)

(join  (bat [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}])  (bat []) =)


(join (reverse nameBAT)  postal-codes =)
(join NameRelationBAT nameBAT =)
(join NameRelationBAT nameBAT = 2)
(join NameRelationBAT nameBAT = 2 3)
(join NameRelationBAT nameBAT = 2 2)
(join NameRelationBAT nameBAT <)
(join NameRelationBAT nameBAT < 4 5)


(reverse nameBAT)

(mirror nameBAT)
(mirror (reverse nameBAT))

(mark nameBAT  10)
(mark nameBAT 0)

;:(project nameBAT "Test")

(slice nameBAT 1 1)
(slice nameBAT 0 0)
(slice nameBAT 2 1)
(slice nameBAT 0 2)

(count nameBAT)

(sum NameRelationBAT)

(max NameRelationBAT)

(min NameRelationBAT)


(def namesBAT2 (bat [{:head 4 :tail "Jake"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}]))

(diff nameBAT namesBAT2)
(diff namesBAT2 nameBAT)

(union nameBAT namesBAT2)

(intersect nameBAT namesBAT2)
(intersect namesBAT2 nameBAT)


 (def AB (bat [{:head 1 :tail 1}
                           {:head 2 :tail 1}
                           {:head 3 :tail 2}
                           {:head 4 :tail 2} ]))

 (def CD (bat [{:head 1 :tail "A"}
                           {:head 2 :tail "A"}
                           {:head 3 :tail "B"}
                           {:head 4 :tail "C"} ]))

 (group AB)
 (group (group AB))
 (group (group AB) CD)


 (def AB  (bat [{:head 1 :tail 1231}
            {:head 2 :tail 323}
            {:head 3 :tail 1}]))

(def CD (bat [{:head 1 :tail 1} {:head 2 :tail 3}]))

(fragment  AB CD)




  (def AB (bat [{:head 1 :tail 1}
                           {:head 2 :tail 1}
                           {:head 3 :tail 2}
                           {:head 4 :tail 2} ]))


  (split AB 4)
  (def AB (bat [{:head 1 :tail \a}
                {:head 2 :tail \b}
                {:head 3 :tail \c}
                {:head 4 :tail \d}
                {:head 5 :tail \e}
                {:head 6 :tail \f}
                {:head 7 :tail \g}
                {:head 8 :tail \h} ]))


  (split AB 3)
  (split AB 2)
 (split AB 1)
  (split AB 9)

   (def CD (bat [{:head 1 :tail \a}
                {:head 4 :tail \b}
                {:head 5 :tail \c}
                {:head 76 :tail \d}
                {:head 34 :tail \e}
                {:head 52 :tail \f}
                {:head 23 :tail \g}
                {:head 1 :tail \h}
                {:head 34 :tail \e}
                {:head 1 :tail \f}
                {:head 23 :tail \g}
                {:head 8 :tail \h}
                {:head 1232 :tail \e}
                {:head 42352 :tail \f}
                {:head 233 :tail \g}
                {:head 234 :tail \h}
                {:head 34 :tail \e}
                {:head 4 :tail \f}
                {:head 2321 :tail \g}
                {:head 86 :tail \h}]))


(split CD 6)


(def names (bat [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}]))

(def postal-codes (bat [{:head 3 :tail 123}
                 {:head 2 :tail 456}
                 {:head 1 :tail 789} ]))

(def date-of-birth  (bat [{:head 1 :tail "01-01-2001"}
                    {:head 2 :tail "02-02-2001"}
                    {:head 3 :tail "03-03-2001"}]))


(multijoin str names 0.07 postal-codes date-of-birth)
(multijoin = postal-codes postal-codes)






(def names (bat [{:head 1 :tail "Roland"}
            {:head 2 :tail "Eddie"}
            {:head 3 :tail "Susanna"}]))

(def postal-codes (bat [{:head 3 :tail 123}
                 {:head 2 :tail 456}
                 {:head 1 :tail 789} ]))

(def date-of-birth  (bat [{:head 1 :tail "01-01-2001"}
                    {:head 2 :tail "02-02-2001"}
                    {:head 3 :tail "03-03-2001"}]))

(makeTable [:zip] [:name :zip :birth] names postal-codes date-of-birth)
(makeTable [:zip] [:name :zip :birth] [names postal-codes date-of-birth])
(makeTable [] [:name :zip :birth] [names postal-codes date-of-birth])





 (def testAB (bat [{:head 1 :tail 1}
                   {:head 1 :tail 2}
                   {:head 1 :tail 3}
                   {:head 1 :tail 4}
                   {:head 2 :tail 666}
                   {:head 3 :tail 100}
                   {:head 3 :tail 100}
                   {:head 3 :tail 100}
                   {:head 3 :tail 100}]))


 (def testCD (bat [{:head 1 :tail 2}
                   {:head 3 :tail 2} ]))

 (pump sum testAB testCD)
 (pump count testAB testCD)
 (pump min testAB testCD)
 (pump max testAB testCD)



(convertToBats #{ {:id 1, :name "Arthur"} {:id 2, :name "Betty" :address "nowhere"} {:id 3, :address "anywhere"}})
(def bla (convertToBats #{ {:id 1, :name "Arthur"} {:id 2, :name "Betty" :address "nowhere"} {:id 3, :address "anywhere"}}))
(:address bla)






(def bestellungen [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}
                   {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}
                   {:id 4 :name "Max" :artikel "Hammer" :menge 1 :datum "1.2.01"}
                   {:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])

(def bestellungenBat (convertToBats bestellungen))

;Select name, sum(Menge) As Menge FROM bestellungen Where artikel = "Nagel" GROUP BY name

(def oid_nil_wherenagel (select (:artikel bestellungenBat) #( = "Nagel" %)))
oid_nil_wherenagel

(def oid_name_wherenagel (join (mirror oid_nil_wherenagel) (:name bestellungenBat) =))
oid_name_wherenagel

(def groupoid_oid_namegroup_wherenagel (groupV2 oid_name_wherenagel ))
groupoid_oid_namegroup_wherenagel

(def groupid_menge_namegroup_wherenagel (multijoin #(join (mirror %) (:menge bestellungenBat) =) groupoid_oid_namegroup_wherenagel))
groupid_menge_namegroup_wherenagel

(def oid_menge (multijoin sum groupid_menge_namegroup_wherenagel ))
oid_menge

(makeTable [:name] [:name "Menge"] (:name bestellungenBat) oid_menge)



;Select name, artikel, sum(Menge) As Menge FROM bestellungen GROUP BY name, artikel
(def groupuid_name (groupV2 (:name bestellungenBat)))
groupuid_name

(def groupuid_name_and_article (groupV2 groupuid_name (:artikel bestellungenBat)))
groupuid_name_and_article

(def groupid_menge (multijoin #(join (mirror %) (:menge bestellungenBat) =) groupuid_name_and_article))
groupid_menge

(def oid_menge (multijoin sum groupid_menge ))
oid_menge

(makeTable [:artikel] [:name :artikel :menge] (:name bestellungenBat) (:artikel bestellungenBat) oid_menge)



(def testAB (bat [{:head 1 :tail 1}
                   {:head 1 :tail 1}
                   {:head 2 :tail 2}]))


 (def testCD (bat [{:head 1 :tail 1}
                   {:head 2 :tail 2} ]))

(join testAB testCD =)
(join testCD testAB =)







(groupV2 (bat [{:head 1 :tail 22}
               {:head 1 :tail 33}
               {:head 2 :tail 22}
               {:head 3 :tail 33} ]))

(multijoin sum (groupV2 (bat [{:head 1 :tail 22}
               {:head 1 :tail 33}
               {:head 2 :tail 22}
               {:head 3 :tail 33} ])))



(groupV2 (groupV2 (bat [ {:head 1 :tail 22}
                         {:head 2 :tail 22}
                         {:head 2 :tail 33}
                         {:head 3 :tail 33}
                         {:head 2 :tail 44}
                         {:head 3 :tail 44} ]) )

         (bat [{:head 1 :tail "AA"}
               {:head 2 :tail "AA"}
               {:head 3 :tail "BB"} ]))






  (insert  (bat [{:head 1 :tail "AA"}
               {:head 2 :tail "AA"}
               {:head 3 :tail "BB"}])
            4  "CCC" )


   (insert  (bat [{:head 1 :tail "AA"}
               {:head 2 :tail "AA"}
               {:head 3 :tail "BB"}])
            3  "BB" )




  (update  (bat [{:head 1 :tail "AA"}
               {:head 2 :tail "AA"}
               {:head 3 :tail "BB"}])
           2 "AA" "BB")


  (update  (bat [{:head 1 :tail "AA"}
               {:head 2 :tail "AA"}
               {:head 2 :tail "BB"}])
           2 "AA" "BB")


  (delete (bat [{:head 1 :tail "AA"}
               {:head 2 :tail "AA"}
               {:head 3 :tail "BB"}])
          2 "AA")



(save (bat [{:head 1 :tail "AA"}
               {:head 2 :tail "AA"}
               {:head 3 :tail "BB"}])
      "/home/seegy/Desktop/Test-singleBat.file")



  (load  "/home/seegy/Desktop/Test-singleBat.file")





(def bestellungen [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}
                   {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}
                   {:id 4 :name "Max" :artikel "Hammer" :menge 1 :datum "1.2.01"}
                   {:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])

(def bestellungenBat (convertToBats bestellungen))

(def batRef (batvar bestellungenBat))

batRef
@batRef


(assign! batRef (convertToBats [{:id 1 :name "pifjpdsijf" :artikel "Nfüokeüofkwagel" :menge 102 :datum "1.1.01324234"}]))


(try
  (assign! batRef (convertToBats [{:id 1 :name "pifjpdsijf" :menge 102 :datum "1.1.01324234"}]))
  (catch IllegalArgumentException e "Catched assert Exception."))





(def bestellungen [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}
                   {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}
                   {:id 4 :name "Max" :artikel "Hammer" :menge 1 :datum "1.2.01"}
                   {:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])

(def bestellungenBat (convertToBats bestellungen))

(def batRef (batvar bestellungenBat))

(insert! batRef :id 5 5)
(insert! batRef :name 5 "Alan")
(insert! batRef :artikel 5 "Säge")
(insert! batRef :menge 5 1)


(insert! batRef {:id 6 :name "Kevin" :artikel "Schraubenzieher" :menge 2 :datum "Irgendwann"})


(makeTable! batRef)
(clojure.core/reverse (makeTable! [:id] batRef))

(def bestellungen [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}
                   {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}
                   {:id 4 :name "Max" :artikel "Hammer" :menge 1 :datum "1.2.01"}
                   {:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])

(def bestellungenBat (convertToBats bestellungen))

(def batRef (batvar bestellungenBat))

(update! batRef :artikel 3 "Hammer" "Spachtel")

(update! batRef :artikel "Nagel" "Nägel")



   (def bestellungen [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}
                   {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}
                   {:id 4 :name "Max" :artikel "Hammer" :menge 1 :datum "1.2.01"}
                   {:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])

(def bestellungenBat (convertToBats bestellungen))

(def batRef (batvar bestellungenBat))

(delete! batRef :name 3 "Max")


(def bestellungen [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}
                   {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}
                   {:id 4 :name "Max" :artikel "Hammer" :menge 1 :datum "1.2.01"}
                   {:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])

(def bestellungenBat (convertToBats bestellungen))

(def batRef (batvar bestellungenBat))

(delete! batRef {:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"})

(makeTable! batRef)
 (delete! batRef {:artikel "Nagel"})

 (makeTable! batRef)

(save-batvar batRef "/home/seegy/Desktop/Test-batRef.file")

(load-batvar "/home/seegy/Desktop/Test-batRef.file")


(def batRef1 (batvar (convertToBats [{:id 1 :name "Peter" :artikel "Nagel" :menge 10 :datum "1.1.01"}])))
(def batRef2 (batvar (convertToBats [{:id 2 :name "Max" :artikel "Nagel" :menge 20 :datum "1.2.01"}])))
(def batRef3 (batvar (convertToBats [{:id 3 :name "Max" :artikel "Nagel" :menge 20 :datum "2.2.01"}])))


(def database {:batRef1 batRef1 :batRef2 batRef2 :batRef3 batRef3})

(save-db database "/home/seegy/Desktop/Test-batRef-db.file")
(load-db "/home/seegy/Desktop/Test-batRef-db.file")
