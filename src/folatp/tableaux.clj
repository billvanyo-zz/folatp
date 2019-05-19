(ns folatp.tableaux
  (:require [folatp.tableaux :refer :all]
            [folatp.syntactics :refer :all]))

(defn negate [f] (list 'not f))

(def alphas 
  { 
   '(:a1 and :a2) {:a1 identity :a2 identity}
   '(not (:a1 or :a2)) {:a1 negate :a2 negate}
   '(not (:a1 imp :a2)) {:a1 identity :a2 negate}
   '(not (:a1 if :a2)) {:a1 negate :a2 identity}
   '(not (:a1 nand :a2)) {:a1 identity :a2 identity}
   '(:a1 nor :a2) {:a1 negate :a2 negate}
   '(:a1 nimp :a2) {:a1 identity :a2 negate}
   '(:a1 nif :a2) {:a1 negate :a2 identity}
   })

(def betas
  { 
   '(not (:b1 and :b2)) {:b1 negate :b2 negate}
   '(:b1 or :b2) {:b1 identity :b2 identity}
   '(:b1 imp :b2) {:b1 negate :b2 identity}
   '(:b1 if :b2) {:b1 identity :b2 negate}
   '(:b1 nand :b2) {:b1 negate :b2 negate}
   '(not (:b1 nor :b2)) {:b1 identity :b2 identity}
   '(not (:b1 nimp :b2)) {:b1 negate :b2 identity}
   '(not (:b1 nif :b2)) {:b1 identity :b2 negate}
   })


;;; returns false, or a map containing the two components of the conjunction
(defn alpha
  [f]
  (loop [alphas alphas]
    (if (empty? alphas)
      false
      (let [[first & rest] alphas
            match (matches (get first 0) f)]
        (if match
          {:a1 ((:a1 (get first 1)) (:a1 match)) 
           :a2 ((:a2 (get first 1)) (:a2 match))}
          (recur rest))))))

;;; returns false, or a map containing the two components of the disjunction
(defn beta
  [f]
  (loop [betas betas]
    (if (empty? betas)
      false
      (let [[first & rest] betas
            match (matches (get first 0) f)]
        (if match
          {:b1 ((:b1 (get first 1)) (:b1 match)) 
           :b2 ((:b2 (get first 1)) (:b2 match))}
          (recur rest))))))

(defn double-negated
  [f]
  (let [match (matches '(not (not :f)) f)]
    (if match (:f match) false)))

;;; gamma - 𝛾 - universal
(defn gamma 
  [f] 
  (let [match (matches '(forall :v :f) f)]
    (if match
      {:v (:v match)
       :f (:f match)}
      (let [match (matches '(not (exists :v :f)) f)]
        (if match
           {:v (:v match)
            :f (list 'not (:f match))}
          false)))))

;;; delta - 𝛿 - existential
(defn delta 
  [f] 
  (let [match (matches '(exists :v :f) f)]
    (if match
      {:v (:v match)
       :f (:f match)}
      (let [match (matches '(not (forall :v :f)) f)]
        (if match
           {:v (:v match)
            :f (list 'not (:f match))}
          false)))))



