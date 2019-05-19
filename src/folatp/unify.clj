(ns folatp.unify
  (:require [folatp.syntactics :refer :all]))


(defn occurs-in 
  [var term]
  ;(println "occurs-in" var term)
  (cond
    (= term '()) false

    (list? term) (or (occurs-in var (first term)) (occurs-in var (rest term)))

    :else (= var term)))

;;; This is the unification algorithm by Martelli & Montanari (1982), adapted from 
;;; exposition by J.W. Kloop in Handbook of Logic in Computer Science, Volume 2, 
;;; Chapter 1, Term Rewriting Systems (section 2.6, Unification)
;;; Modification is made to not apply derived substitutions to the list of equations,
;;; but only to raw variables as they are encountered.
(defn unify-equations
  ([e]
   (unify-equations e {}))
  ([e subst]
   (loop [e e
          subst subst]
     (if 
         (empty? e) 
       subst
       (let [[eq & e] e
             t1 (if (lower-case-symbol? (first eq)) 
                  (get subst (first eq) (first eq)) 
                  (first eq))
             t2 (if (lower-case-symbol? (second eq)) 
                  (get subst (second eq) (second eq)) 
                  (second eq))]
                                        ;(println eq)
                                        ;(println t1)
                                        ;(println t2)
         (cond
                                        ; (1) term decomposition (todo: make concat & map into single function)
                                        ; combined with (5) failure rule
           (and (list? t1) (list? t2))
           (if (and (= (first t1) (first t2)) (= (count t1) (count t2))) 
             (recur (concat (map #(list %1 %2) (rest t1) (rest t2)) e) subst)
             false)

                                        ; (2) removal of trival equations
                                        ;(and (symbol? t1) (symbol? t2) (= t1 t2)) 
           (= t1 t2)
           (recur e subst)

                                        ; (3) swap
           (and (lower-case-symbol? t2) (not (lower-case-symbol? t1)))
           (recur (cons (list t2 t1) e) subst)
           
                                        ; (4) variable elimination
           (and (lower-case-symbol? t1) (not (occurs-in t1 t2)))
           (recur e (assoc subst t1 t2))

           :else
           false))))))

(defn unify-terms
  ([t1 t2] 
   (unify-equations (list (list t1 t2)))))

(defn unify-substitutions
  [s1 s2]
  nil
)

