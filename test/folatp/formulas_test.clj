(ns folatp.formulas-test
  (:require [clojure.test :refer :all]
            [folatp.formulas :refer :all]
            [folatp.test-defs :refer :all]))


(deftest test-alpha-decomposition
  (is (= '{:a1 (P x), :a2 (Q x)}             (alpha '((P x) and (Q x)))))
  (is (= false                               (alpha '((P x) or (Q x)))))
  (is (= false                               (alpha '((P x) imp (Q x)))))
  (is (= false                               (alpha '((P x) if (Q x)))))
  (is (= false                               (alpha '((P x) nand (Q x)))))
  (is (= '{:a1 (not (P x)), :a2 (not (Q x))} (alpha '((P x) nor (Q x)))))
  (is (= '{:a1 (P x), :a2 (not (Q x))}       (alpha '((P x) nimp (Q x)))))
  (is (= '{:a1 (not (P x)), :a2 (Q x)}       (alpha '((P x) nif (Q x)))))
  (is (= false                               (alpha '(not ((P x) and (Q x))))))
  (is (= '{:a1 (not (P x)), :a2 (not (Q x))} (alpha '(not ((P x) or (Q x))))))
  (is (= '{:a1 (P x), :a2 (not (Q x))}       (alpha '(not ((P x) imp (Q x))))))
  (is (= '{:a1 (not (P x)), :a2 (Q x)}       (alpha '(not ((P x) if (Q x))))))
  (is (= '{:a1 (P x), :a2 (Q x)}             (alpha '(not ((P x) nand (Q x))))))
  (is (= false                               (alpha '(not ((P x) nor (Q x))))))
  (is (= false                               (alpha '(not ((P x) nimp (Q x))))))
  (is (= false                               (alpha '(not ((P x) nif (Q x))))))
  (is (= false                               (alpha '((P x) iff (Q x)))))
  (is (= false                               (alpha '((P x) xor (Q x)))))
  (is (= false                               (alpha '(not ((P x) iff (Q x))))))
  (is (= false                               (alpha '(not ((P x) xor (Q x))))))
)

(deftest test-beta-decomposiotion
  (is (= false                               (beta '((P x) and (Q x)))))
  (is (= '{:b1 (P x), :b2 (Q x)}             (beta '((P x) or (Q x)))))
  (is (= '{:b1 (not (P x)), :b2 (Q x)}       (beta '((P x) imp (Q x)))))
  (is (= '{:b1 (P x), :b2 (not (Q x))}       (beta '((P x) if (Q x)))))
  (is (= '{:b1 (not (P x)), :b2 (not (Q x))} (beta '((P x) nand (Q x)))))
  (is (= false                               (beta '((P x) nor (Q x)))))
  (is (= false                               (beta '((P x) nimp (Q x)))))
  (is (= false                               (beta '((P x) nif (Q x)))))
  (is (= '{:b1 (not (P x)), :b2 (not (Q x))} (beta '(not ((P x) and (Q x))))))
  (is (= false                               (beta '(not ((P x) or (Q x))))))
  (is (= false                               (beta '(not ((P x) imp (Q x))))))
  (is (= false                               (beta '(not ((P x) if (Q x))))))
  (is (= false                               (beta '(not ((P x) nand (Q x))))))
  (is (= '{:b1 (P x), :b2 (Q x)}             (beta '(not ((P x) nor (Q x))))))
  (is (= '{:b1 (not (P x)), :b2 (Q x)}       (beta '(not ((P x) nimp (Q x))))))
  (is (= '{:b1 (P x), :b2 (not (Q x))}       (beta '(not ((P x) nif (Q x))))))
  (is (= '{:b1 ((P x) and (Q x)), :b2 ((not (P x)) and (not (Q x)))}   (beta '((P x) iff (Q x)))))
  (is (= '{:b1 ((P x) and (not (Q x))), :b2 ((not (P x)) and (Q x))}   (beta '((P x) xor (Q x)))))
  (is (= '{:b1 ((P x) and (not (Q x))), :b2 ((not (P x)) and (Q x))}   (beta '(not ((P x) iff (Q x))))))
  (is (= '{:b1 ((P x) and (Q x)), :b2 ((not (P x)) and (not (Q x)))}   (beta '(not ((P x) xor (Q x))))))
)
  
(deftest test-gamma-decomposition
  (is (= '{:v x, :f ((P x) or (Q x))}        (gamma '(forall x ((P x) or (Q x))))))
  (is (= '{:v x, :f (not ((P x) or (Q x)))}  (gamma '(not (exists x ((P x) or (Q x)))))))
  (is (= false                               (gamma '(not (forall x ((P x) or (Q x)))))))
  (is (= false                               (gamma '(exists x ((P x) or (Q x))))))
)

(deftest test-delta-decomposition
  (is (= false                               (delta '(forall x ((P x) or (Q x))))))
  (is (= false                               (delta '(not (exists x ((P x) or (Q x)))))))
  (is (= '{:v x, :f (not ((P x) or (Q x)))}  (delta '(not (forall x ((P x) or (Q x)))))))
  (is (= '{:v x, :f ((P x) or (Q x))}        (delta '(exists x ((P x) or (Q x))))))
)

