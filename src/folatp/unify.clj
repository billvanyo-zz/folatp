;;;; collection of functions related to unification and working with substitutions

;;;; substitutions are maps from variables (lowercase symbols) to terms.
;;;; terms are either variables, constants (neither a variable nor list), or lists 
;;;; where first element is a predicate or function symbol, and rest of elements are terms.

;;;; there are also "annotated substitutions", which are maps containing:
;;;; :subst - a plain substitution
;;;; :type - atomic or beta-merge (result of closing two atomic formulas, or of 
;;;; merging substitutions from two tableau beta branches
;;;; :pos-fmla & :neg-fmla - for atomic closure, the formula structures
;;;; :beta1-subst & beta2-subst - for beta merge, the annotated substitutions that
;;;; were merged.
;;;; When an annotated substitution is propagated to the root node of a tableau,
;;;; it contains all the information to produce a pruned tableau proof (i.e. containing
;;;; only what is essential to the final proof).


(ns folatp.unify
  (:require [folatp.syntactics :refer :all]
            [clojure.set :refer :all]))

;;; check if a variable occurs in a term
(defn occurs-in 
  [var term]
  (cond
    (= term '()) false
    (seq? term) (or (occurs-in var (first term)) (occurs-in var (rest term)))
    :else (= var term)))

;;; replace var with val throughout term  
(defn replace-in-term
  [term var val]
  (cond
    (lower-case-symbol? term) (if (= var term) val term)
    (seq? term) (cons 
                 (if (seq? (first term)) 
                   (replace-in-term (first term) var val)
                   (first term))
                 (map (fn [t] (replace-in-term t var val)) (rest term)) 
                 )
    :else term)
    )

;;; apply a substitution to a term
(defn apply-subst
  [term subst]
  (reduce (fn [t [var val]] (replace-in-term t var val)) term subst))

;;; replace var with val throughout all terms in substitution
(defn replace-in-subst-terms
  [subst var val]
  (reduce-kv (fn [m k v] (assoc m k (replace-in-term v var val))) {} subst)
)

;;; core of the unification algorithm
(defn unify-equations
  ([e]
   (unify-equations e {}))
  ([e subst]
   (loop [e e
          subst subst]
     (if (empty? e) 
       subst
       (let [[eq & e] e
             t1 (apply-subst (first eq) subst)
             t2 (apply-subst (second eq) subst)]
         (cond
           ;; term decomposition combined with failure rule
           (and (seq? t1) (seq? t2))
           (if (and (= (first t1) (first t2)) (= (count t1) (count t2))) 
             (recur (concat (map #(list %1 %2) (rest t1) (rest t2)) e) subst)
             false)

           ;; removal of trival equations
           (= t1 t2)
           (recur e subst)

           ;; swap
           (and (lower-case-symbol? t2) (not (lower-case-symbol? t1)))
           (recur (conj e (list t2 t1)) subst)
           
           ;; variable elimination
           (and (lower-case-symbol? t1) (not (occurs-in t1 t2)))
           (recur e (assoc (replace-in-subst-terms subst t1 t2) t1 t2))

           :else
           false))))))

;;; simple unification of two terms (returns false if not unifiable)
(defn unify-terms
  ([t1 t2] 
   (unify-equations (list (list t1 t2)))))

;;; simple unification of two substitutions (returns false if not unifiable)
(defn unify-substitutions
  [s1 s2]
  (unify-equations (into () s1) s2)
)

;;; unify fmla & it's negation
(defn closing-subst
  [fmla1 fmla2]
  (if (= (= 'not (first fmla1)) (= 'not (first fmla2)))
    false
    (if (= 'not (first fmla1))
      (unify-terms (second fmla1) fmla2)
      (unify-terms fmla1 (second fmla2)))))

;;; =========================================================================
;;; the following functions all deal with formula structures and 
;;; annotated substitution structures

;;; formula structures contain formulas and additional info (derivation, etc)
;;; unifying formula structures produces an annotated substitution structure
(defn unify-fmla-structs
  [fstr1 fstr2]
  (let [f1 (:fmla fstr1)
        f2 (:fmla fstr2)
        unifier (closing-subst f1 f2)
        dependencies (clojure.set/union #{(:id fstr1) (:id fstr2)} 
                                        (:dependencies fstr1) 
                                        (:dependencies fstr2))]
    (if unifier
      {:subst unifier
       :type :atomic
       :fmla1 fstr1
       :fmla2 fstr2
       :dependencies dependencies
       }
      false )))

;;; determine if sub1 <= sub2 (more general or equivalent)
;;; this is used to keep sets of most general unifiers.
;;; checks that all pairs of sub1 are unified by sub2
(defn more-general
  [str1 str2]
  (let [sub1 (:subst str1)
        sub2 (:subst str2)
        result (every?
                (fn [[var val]] 
                  (= (get sub2 var var) (apply-subst val sub2)))
                sub1
                )]
    result
    )
)

;;; add a substitution to a set of most general substitutions. 
;;; returns structure with 
;;; :mguset - set of most general substitution structures
;;; :subsumed - was the new substitution subsumed by a more general one already in set
;;; :displaced - list of less general substitutions in set that were displaced
;;; note: set is an antichain - no elements x, y such that x<=y
;;; therefore: if sub < x in set, no element y in set s.t. y<=sub.
;;; (see relevance below)
(defn add-subst-to-mguset
  [sub set]
  (loop [inset set
         result ()
         displaced ()]
    (cond
      (empty? inset) 
      {:mguset (cons sub result) 
       :subsumed false 
       :displaced displaced}
      
      (and (empty? displaced) (more-general (first inset) sub)) ; set element first <= sub
      {:mguset set 
       :subsumed true 
       :displaced displaced}
      ;; since above test failed, first != sub
      
      (more-general sub (first inset)) ; sub<first, therefore no other set element x<=sub
      (recur (rest inset) 
             result 
             (cons (first inset) displaced)) 
      
      :else 
      (recur (rest inset) 
             (cons (first inset) result) 
             displaced)
      )))

;;; add multiple substitutions to a set of most general substitutions.
;;; returns structure with 
;;; :mguset - set of most general substitution structures
;;; :added - list of the new substitutions actually added (not subsumed by a more 
;;; general substition already in set)
;;; :displaced - list of less general substitutions in set that were displaced
(defn add-substs-to-mguset
  [subs set]
  (loop [result set
         newsubs subs
         added ()
         displaced ()]
    (if (empty? newsubs)
      {:mguset result
       :added added
       :displaced displaced
       }
      (let [add-first (add-subst-to-mguset (first newsubs) result)]
        (recur
         (:mguset add-first)
         (rest newsubs)
         (if (:subsumed add-first) added (cons (first newsubs) added))
         (concat (:displaced add-first) displaced)
         )))))

;;; takes fmla and list of formulas, produces set of all closing substitution structures
(defn closing-substitutions
  [fmla-struct fmla-struct-list]
  (reduce 
   (fn [substructs fstruct]
     (let [unifier-struct (unify-fmla-structs fmla-struct fstruct)]
       (if unifier-struct
         (cons unifier-struct substructs)
         substructs)))
   ()
   fmla-struct-list))

(defn unify-subst-structures
  [substr1 substr2]
  (let [unified (unify-substitutions (:subst substr1) (:subst substr2))
        dependencies (clojure.set/union (:dependencies substr1) (:dependencies substr2))]
    (if unified
      {:subst unified
       :type :beta
       :beta1 substr1
       :beta2 substr2
       :dependencies dependencies
       }
      false
      )))

(defn cartesian-sum-of-subst-sets
  [set1 set2]
  (filter 
   identity 
   (for [sub1 set1 
         sub2 set2] 
     (unify-subst-structures sub1 sub2))))

