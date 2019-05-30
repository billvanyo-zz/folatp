(ns folatp.syntactics-test
  (:require [clojure.test :refer :all]
            [folatp.syntactics :refer :all]
            [folatp.test-defs :refer :all]))

(deftest test-matches
  (is (= '{:v x, :f (P x)} (matches '(forall :v :f) '(forall x (P x)))))
  (is (= '{:v x, :f (P x)} (matches '(not (forall :v :f)) '(not (forall x (P x))))))
  (is (= '{:f1 (P x), :f2 (Q y)} (matches '(not (:f1 and :f2)) '(not ((P x) and (Q y))))))
)

(deftest test-print-formatter
  (is (=
       "¬((∃w)(∀x)R(x,w,f(x,w)) ⊃ (∃w)(∀x)(∃y)R(x,w,y))"
       (print-formula '(not ((exists w (forall x (R x w (f x w))))
                             imp
                             (exists w (forall x (exists y (R x w y)))))) )
       ))

  (is (= "(∀x)(∀y)(∀z)((R(x,y) ∧ R(y,z)) ⊃ R(x,z))"
         (print-formula transitivity)))

  (is (= "(∀x)(∀y)(R(x,y) ⊃ R(y,x))"
         (print-formula symmetry)))

  (is (= "(∀x)(∃y)R(x,y)"
         (print-formula nontriviality)))

  (is (= "(∀x)R(x,x)"
         (print-formula reflexivity)))

  (is (= "R(x₁,y₂,f₁(x₁,y₂))"
         (print-formula '(R x1 y2 (f1 x1 y2)))))
)

