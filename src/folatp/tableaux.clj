(ns folatp.tableaux
  (:require [folatp.formulas :refer :all]
            [folatp.syntactics :refer :all]
            [folatp.utils :refer :all]
            [folatp.tree-printer :refer :all]
            [folatp.gensymbols :refer :all]))

;;; for indexed map representation of binary tree
(defn left [n] (inc (* 2 n)))
(defn right [n] (* 2 (inc n)))
(defn parent [n] (quot (dec n) 2))
(defn sibling [n] (if (odd? n) (inc n) (dec n)))

;;; Returns a list of formula structures, which are maps containing formula, 
;;; integer identifier, derivation and free variables list.
(defn init-fmla-structs
  [axioms-lst goal]
  (loop [id 1
         fmla-structs-vec []
         axioms-lst axioms-lst]
    (if (empty? axioms-lst)
      (apply list (conj fmla-structs-vec 
                        {:id id
                         :fmla (list 'not goal)
                         :deriv '(negated goal)
                         :free-vars '()}))
      (recur
       (inc id)
       (conj fmla-structs-vec 
             {:id id
              :fmla (first axioms-lst)
              :deriv (list 'axiom id)
              :free-vars '()})
       (rest axioms-lst)))))

(defn substitute
  [var val fmla]
  (if (= fmla ()) 
    ()
    (if (symbol? fmla)
      (if (= var fmla) val fmla)
      (conj (substitute var val (rest fmla)) (substitute var val (first fmla)))))
)

(defn apply-rule
  [fmla-struct fmla-count]
  (let [id (:id fmla-struct)
        fmla (:fmla fmla-struct)
        free-vars (:free-vars fmla-struct)]
    (cond-let ; see macro definition in utils.clj

     [double-neg (double-negated fmla)]
     double-neg
     {:rule 'double-negative
      :fmla {:id (inc fmla-count) 
             :fmla double-neg
             :deriv (list 'double-negative id)
             :free-vars free-vars}}

     [alpha (alpha fmla)]
     alpha
     {:rule 'alpha
      :alpha1 {:id (inc fmla-count) 
             :fmla (:a1 alpha)
             :deriv (list 'alpha1 id)
             :free-vars free-vars}
      :alpha2 {:id (+ 2 fmla-count) 
             :fmla (:a2 alpha)
             :deriv (list 'alpha2 id)
             :free-vars free-vars}}
 
     [beta (beta fmla)]
     beta
     {:rule 'beta
      :beta1 {:id (inc fmla-count) 
             :fmla (:b1 beta)
             :deriv (list 'beta1 id)
             :free-vars free-vars}
      :beta2 {:id (+ 2 fmla-count) 
             :fmla (:b2 beta)
             :deriv (list 'beta2 id)
             :free-vars free-vars}}

     [gamma (gamma fmla)]
     gamma
     (let [new-var (gensymbol (:v gamma))]
         {:rule 'gamma
          :fmla {:id (inc fmla-count) 
                 :fmla (substitute (:v gamma) new-var (:f gamma))
                 :deriv (list 'gamma id)
                 :free-vars (conj free-vars new-var)}})

     [delta (delta fmla)]
     delta
     (let [skolem-func (gensymbol (:v delta))]
      {:rule 'delta
       :fmla {:id (inc fmla-count) 
              :fmla (substitute (:v delta) (conj free-vars skolem-func) (:f delta))
              :deriv (list 'delta id)
              :free-vars free-vars}})

     [] true {:rule 'atomic}  ; no applicable decomposition rule (atomic fmla)
     )))



;;; tableau consists of three elements:
;;; 1) :tree-map - tree, represented as map with integer node indexes, 
;;; where each node contains formula structures
;;; and closing substitutions
;;; 2) :branch-q - queue of branch structures for fair processing of branches
;;; 3) :fmla-count - count of fmlas in tree
(defn init-tableau
  [axioms goal]
  (let [init-fmlas (init-fmla-structs axioms goal)
        branch-struct {:leaf-index 0
                       :fmla-q (reduce conj clojure.lang.PersistentQueue/EMPTY init-fmlas)
                       :atomic-fmlas '()}
        branch-q (conj clojure.lang.PersistentQueue/EMPTY branch-struct)
        fmla-count (count init-fmlas)]
    {:tree-map {0 {:fmlas init-fmlas :closing-substs '()}}
     :branch-q branch-q
     :fmla-count fmla-count }))

;;; expand tableau by a single rule application
(defn single-step 
  [tableau]
  (let [tree-map (:tree-map tableau)
        branch-q (:branch-q tableau)
        fmla-count (:fmla-count tableau)

        select-branch (peek branch-q)

        leaf-index (:leaf-index select-branch)
        fmla-q (:fmla-q select-branch)
        atomic-fmlas (:atomic-fmlas select-branch)

        fmla-struct (peek fmla-q) ; the fmla to apply a rule to

        rule-application (apply-rule fmla-struct fmla-count)
        rule (:rule rule-application)]

    ;; TODO check if root contains a closing substitution


    (cond
      (= rule 'alpha)
      ;; add a1 & a2 to leaf
      ;; remove alpha from fmla-q for branch and add a1 & a2
      ;; rotate branch-q
      {:tree-map (assoc-in tree-map [leaf-index :fmlas]
                           (concat (:fmlas (get tree-map leaf-index)) 
                                   (list (:alpha1 rule-application)
                                         (:alpha2 rule-application))))
       :branch-q (conj (pop branch-q) 
                       (assoc select-branch :fmla-q
                              (conj (pop fmla-q) 
                                    (:alpha1 rule-application) 
                                    (:alpha2 rule-application))))
       :fmla-count (+ 2 fmla-count)
       }


      (= rule 'beta)
      ;; create two new leaf nodes with b1 & b2
      ;; 
      ;; remove leaf node from branch-q and add new leaf nodes
      (let [beta1-fmla-struct {:fmlas (list (:beta1 rule-application))
                               :closing-substs '()
                               }
            beta2-fmla-struct {:fmlas (list (:beta2 rule-application))
                               :closing-substs '()
                               }
            new-beta1-branch-q (conj (pop branch-q)
                                     {:leaf-index (left leaf-index)
                                      :fmla-q (conj (pop fmla-q) beta1-fmla-struct)
                                      :atomic-fmlas atomic-fmlas
                                      })
            new-beta2-branch-q (conj (pop branch-q)
                                     {:leaf-index (right leaf-index)
                                      :fmla-q (conj (pop fmla-q) beta2-fmla-struct)
                                      :atomic-fmlas atomic-fmlas
                                      })]
        {:tree-map (assoc tree-map 
                          (left leaf-index) beta1-fmla-struct
                          (right leaf-index) beta2-fmla-struct)
         :branch-q (conj (pop branch-q) new-beta1-branch-q new-beta2-branch-q)
         :fmla-count (+ 2 fmla-count)
         })


      (= rule 'gamma)
      ;; here we don't pop fmla-qput fmla-struct back on fmla-q, 
      ;; since gamma rule can be applied multiple times
      {:tree-map (assoc-in tree-map [leaf-index :fmlas]
                           (concat (:fmlas (get tree-map leaf-index)) 
                                   (list (:fmla rule-application))))
       :branch-q (conj (pop branch-q)
                       (assoc select-branch :fmla-q
                              (conj (pop fmla-q) 
                                    (:fmla rule-application)
                                    fmla-struct)))
       :fmla-count (inc fmla-count)
       }

      (or (= rule 'delta) (= rule 'double-negative))
      ;; same as gamma, but don't reuse fmla-struct
      {:tree-map (assoc-in tree-map [leaf-index :fmlas]
                           (concat (:fmlas (get tree-map leaf-index)) 
                                   (list (:fmla rule-application))))
       :branch-q (conj (pop branch-q)
                       (assoc select-branch :fmla-q
                              (conj (pop fmla-q) 
                                    (:fmla rule-application))))
       :fmla-count (inc fmla-count)
       }


      (= rule 'atomic)
      ;; attempt to unify this formula with it's negation and propagate substitution 
      ;; toward root
      (let [atomic-fmla (:fmla rule-application)]
        {:tree-map :TODO
         :branch-q :TODO
         :fmla-count :TODO
         }
        )
      ))



;;; convert to structure suitable for printing
  (defn convert-tree-vector
    ([tree-vector subst]
     (convert-tree-vector tree-vector 0 subst)
     )

    ([tree-vector root-index subst]
     (if (empty? (get tree-vector root-index))
       nil
       (let [left-tree (convert-tree-vector tree-vector (left root-index) subst)
             right-tree (convert-tree-vector tree-vector (right root-index) subst)
             root-fmlas (reverse (:fmlas (get tree-vector root-index)))
             tree {:label (print-formula-struct (first root-fmlas)) ; TODO add id and derivation
                   :left left-tree
                   :right right-tree}]
         (loop [rest-fmlas (rest root-fmlas)
                tree tree]
           (if (empty? rest-fmlas)
             tree
             (recur
              (rest rest-fmlas)
              {:label (print-formula-struct (first rest-fmlas)) ; TODO add id and derivation
               :left tree
               :right nil}
              ))))))))

