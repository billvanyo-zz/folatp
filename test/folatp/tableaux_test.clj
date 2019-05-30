(ns folatp.tableaux-test
  (:require [clojure.test :refer :all]
            [folatp.tableaux :refer :all]
            [folatp.test-defs :refer :all]))

(deftest test-init-fmla-structs
  (is (= '({:id 1,
            :fmla (forall x (forall y (forall z (((R x y) and (R y z)) imp (R x z))))),
            :deriv (axiom 1)
           	:free-vars ()}
           {:id 2,
            :fmla (forall x (forall y ((R x y) imp (R y x)))),
            :deriv (axiom 2)
            :free-vars ()}
           {:id 3, 
            :fmla (forall x (exists y (R x y))), 
            :deriv (axiom 3)
            :free-vars ()}
           {:id 4, 
            :fmla (not (forall x (R x x))), 
            :deriv (negated goal)
            :free-vars ()})
         (init-fmla-structs (list transitivity symmetry nontriviality) reflexivity))))


