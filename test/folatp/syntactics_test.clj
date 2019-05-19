(ns folatp.syntactics-test
  (:require [clojure.test :refer :all]
            [folatp.syntactics :refer :all]))

(deftest test-matches
  (is (= '{:v x, :f (P x)} (matches '(forall :v :f) '(forall x (P x)))))
  (is (= '{:v x, :f (P x)} (matches '(not (forall :v :f)) '(not (forall x (P x))))))
  (is (= '{:f1 (P x), :f2 (Q y)} (matches '(not (:f1 and :f2)) '(not ((P x) and (Q y))))))
)

(deftest test-print-formatter
  (is (=
       "¬((∃w)(∀x)R(x,w,f(x,w)) ⊃ (∃w)(∀x)(∃y)Rx(w,y))"
       (print-formula '(not ((exists w (forall x (R x w (f x w))))
                             imp
                             (exists w (forall x (exists y (Rx w y)))))) )
       ))
)

