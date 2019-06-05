;;;; functions for identifying formulas by type and for decomposing formulas
;;;; types: :atomic, :double-negated, :alpha, :beta, :gamma, :delta

(ns folatp.formulas
  (:require [folatp.syntactics :refer :all]))

(def duals
  {:alpha :beta
   :beta :alpha
   :secondary :secondary
   :gamma :delta
   :delta :gamma
   :atomic :atomic
   }
)

(defn fmla-type-subr
  [fmla]
  (cond
    (= 'not (first fmla)) (if (= 'not (first (second fmla))) 
                            :double-negated 
                            ((fmla-type-subr (second fmla)) duals))
    (= 'forall (first fmla)) :gamma
    (= 'exists (first fmla)) :delta
    (contains? #{'and 'nor 'nimp 'nif} (second fmla)) :alpha
    (contains? #{'or 'imp 'if 'nand} (second fmla)) :beta
    (contains? #{'xor 'iff} (second fmla)) :secondary
    :else :atomic
    )
)

(defn fmla-type
  [fmla]
  (let [ftype (fmla-type-subr fmla)]
    (if (= ftype :secondary) :beta ftype))
)

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

;;; the secondary connectives are equivalent to a disjunction of two conjunctions
;;; A iff B --> (A and B) or ((not A) and (not B))
;;; A xor B --> (A and (not B)) or ((not A) and B)
(defn secondary
  [f]
  (or 
   (let [match (or (matches '(:f1 iff :f2) f) (matches '(not (:f1 xor :f2)) f))] 
     (and match
          (let [f1 (:f1 match)
                f2 (:f2 match)]
            {:b1 (list f1 'and f2)
             :b2 (list (list 'not f1) 'and (list 'not f2))}
            )))
   (let [match (or (matches '(:f1 xor :f2) f) (matches '(not (:f1 iff :f2)) f))] 
     (and match
          (let [f1 (:f1 match)
                f2 (:f2 match)]
            {:b1 (list f1 'and (list 'not f2))
             :b2 (list (list 'not f1) 'and f2)}
            )))
   ))

;;; returns false, or a map containing the two components of the disjunction
(defn beta
  [f]
  (loop [betas betas]
    (if (empty? betas)
      (secondary f)
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

