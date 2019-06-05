(ns folatp.tableaux-test
  (:require [clojure.test :refer :all]
            [folatp.tableaux :refer :all]
            [folatp.test-defs :refer :all]))

(deftest test-init-fmla-structs
  (is (= '({:id 1,
            :type :gamma
            :fmla (forall x (forall y (forall z (((R x y) and (R y z)) imp (R x z))))),
            :deriv premise
           	:free-vars ()
            :dependencies #{}}
           {:id 2,
            :type :gamma
            :fmla (forall x (forall y ((R x y) imp (R y x)))),
            :deriv premise
            :free-vars ()
            :dependencies #{}}
           {:id 3, 
            :type :gamma
            :fmla (forall x (exists y (R x y))), 
            :deriv premise
            :free-vars ()
            :dependencies #{}}
           {:id 4, 
            :type :delta
            :fmla (not (forall x (R x x))), 
            :deriv goal
            :free-vars ()
            :dependencies #{}})
         (init-fmla-structs (list transitivity symmetry nontriviality) reflexivity))))


