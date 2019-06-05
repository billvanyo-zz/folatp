(ns folatp.tableaux
  (:require [folatp.formulas :refer :all]
            [folatp.syntactics :refer :all]
            [folatp.tree-printer :refer :all]
            [folatp.gensymbols :refer :all]
            [folatp.unify :refer :all]
            [folatp.formula-queue :refer :all]            
            ))

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
                         :type (fmla-type (list 'not goal))
                         :fmla (list 'not goal)
                         :deriv 'goal
                         :free-vars '()
                         :dependencies #{}}))
      (recur
       (inc id)
       (conj fmla-structs-vec 
             {:id id
              :type (fmla-type (first axioms-lst))
              :fmla (first axioms-lst)
              :deriv 'premise
              :free-vars '()
              :dependencies #{}})
       (rest axioms-lst)))))

(defn substitute
  [var val fmla]
  (if (= fmla ()) 
    ()
    (if (not (seq? fmla))
      (if (= var fmla) val fmla)
      (conj (substitute var val (rest fmla)) (substitute var val (first fmla))))))

(defn apply-rule
  [fmla-struct fmla-count]
 (let [id (:id fmla-struct)
       fmla (:fmla fmla-struct)
       ftype (:type fmla-struct)
       free-vars (:free-vars fmla-struct)
       dependencies (:dependencies fmla-struct)]
    (cond
      (= :double-negated ftype)
      (let [double-neg (double-negated fmla)]
        {:rule 'double-negative
         :fmla {:id (inc fmla-count) 
                :type (fmla-type double-neg)
                :fmla double-neg
                :deriv (list 'double-negative id)
                :free-vars free-vars
                :dependencies (conj dependencies id)}})
      
      (= :alpha ftype)
      (let [alpha (alpha fmla)]
        {:rule 'alpha
         :alpha1 {:id (inc fmla-count) 
                  :type (fmla-type (:a1 alpha))
                  :fmla (:a1 alpha)
                  :deriv (list 'alpha1 id)
                  :free-vars free-vars
                  :dependencies (conj dependencies id)}
         :alpha2 {:id (+ 2 fmla-count) 
                  :type (fmla-type (:a2 alpha))
                  :fmla (:a2 alpha)
                  :deriv (list 'alpha2 id)
                  :free-vars free-vars
                  :dependencies (conj dependencies id)}})
      
      (= :beta ftype)
      (let [beta (beta fmla)]
        {:rule 'beta
         :beta1 {:id (inc fmla-count) 
                 :type (fmla-type (:b1 beta))
                 :fmla (:b1 beta)
                 :deriv (list 'beta1 id)
                 :free-vars free-vars
                 :dependencies (conj dependencies id)}
         :beta2 {:id (+ 2 fmla-count) 
                 :type (fmla-type (:b2 beta))
                 :fmla (:b2 beta)
                 :deriv (list 'beta2 id)
                 :free-vars free-vars
                 :dependencies (conj dependencies id)}})
      
      (= :gamma ftype) 
      (let [gamma (gamma fmla)
            new-var (gensymbol (:v gamma))
            gamma-instance (substitute (:v gamma) new-var (:f gamma))] 
        {:rule 'gamma
         :fmla {:id (inc fmla-count) 
                :type (fmla-type gamma-instance)
                :fmla gamma-instance
                :deriv (list 'gamma id)
                :free-vars (conj free-vars new-var)
                :dependencies (conj dependencies id)}})
      
      (= :delta ftype)
      (let [delta (delta fmla)
            skolem-func (gensymbol (:v delta))
            skolemized-delta (substitute (:v delta) 
                                         (conj free-vars skolem-func) 
                                         (:f delta))]
        {:rule 'delta
         :fmla {:id (inc fmla-count) 
                :type (fmla-type skolemized-delta)
                :fmla skolemized-delta
                :deriv (list 'delta id)
                :free-vars free-vars
                :dependencies (conj dependencies id)}})

      (= :atomic ftype)
      {:rule 'atomic}  ; no applicable decomposition rule (atomic fmla)
     )))

;;; tableau consists of these elements:
;;; 1) :tree-map - tree, represented as map with integer node indexes, 
;;; where each node contains formula structures
;;; and closing substitutions
;;; 2) :branch-q - list of branch structures for fair processing of branches
;;; 3) :fmla-count - count of fmlas in tree
;;; 4) :gamma-count - how many gamma rule applications have been done
(defn init-tableau
  [axioms goal]
  (let [init-fmlas (init-fmla-structs axioms goal)
        branch-struct {:leaf-index 0
                       :fmla-q (apply new-formula-queue init-fmlas)
                       :atomic-fmlas '()}
        branch-q (conj clojure.lang.PersistentQueue/EMPTY branch-struct)
        fmla-count (count init-fmlas)]
    {:tree-map {0 {:fmlas init-fmlas :closing-substs '()}}
     :branch-q branch-q
     :fmla-count fmla-count 
     :gamma-count 0}))

(declare propagate-substs)

;;; expand tableau by a single rule application
(defn single-step 
  [tableau]
  (let [tree-map (:tree-map tableau)
        branch-q (:branch-q tableau)
        fmla-count (:fmla-count tableau)
        gamma-count (:gamma-count tableau)

        select-branch (peek branch-q)

        leaf-index (:leaf-index select-branch)
        fmla-q (:fmla-q select-branch)
        atomic-fmlas (:atomic-fmlas select-branch)

        fmla-struct (next-fmla fmla-q) ; the fmla to apply a rule to

        rule-application (apply-rule fmla-struct fmla-count)
        rule (:rule rule-application)]
    (cond
      (= rule 'alpha)
      ;; add a1 & a2 to leaf
      ;; remove alpha from fmla-q for branch and add a1 & a2 to front
      ;; rotate branch-q
      (let [fmla-q (enqueue-fmlas (pop-fmla-queue fmla-q) 
                                  (:alpha1 rule-application)
                                  (:alpha2 rule-application))]
        {:tree-map (assoc-in tree-map [leaf-index :fmlas]
                             (concat (:fmlas (get tree-map leaf-index)) 
                                     (list (:alpha1 rule-application)
                                           (:alpha2 rule-application))))
         :branch-q (conj (pop branch-q) (assoc select-branch :fmla-q fmla-q))
         :fmla-count (+ 2 fmla-count)
         :gamma-count gamma-count
         })


      (= rule 'beta)
      ;; create two new leaf nodes with b1 & b2
      ;; remove leaf node from branch-q and add new leaf nodes
      (let [beta1-fmla-struct {:fmlas (list (:beta1 rule-application))
                               :closing-substs '()
                               }
            beta2-fmla-struct {:fmlas (list (:beta2 rule-application))
                               :closing-substs '()
                               }
            new-beta1-branch {:leaf-index (left leaf-index)
                              :fmla-q (enqueue-fmlas (pop-fmla-queue fmla-q) 
                                                     (:beta1 rule-application))
                              :atomic-fmlas atomic-fmlas
                              }
            new-beta2-branch {:leaf-index (right leaf-index)
                              :fmla-q (enqueue-fmlas (pop-fmla-queue fmla-q) 
                                                     (:beta2 rule-application))
                              :atomic-fmlas atomic-fmlas
                              }
            ]
        {:tree-map (assoc tree-map 
                          (left leaf-index) beta1-fmla-struct
                          (right leaf-index) beta2-fmla-struct)
         :branch-q (conj (pop branch-q) new-beta1-branch new-beta2-branch)
         :fmla-count (+ 2 fmla-count)
         :gamma-count gamma-count
         })


      (= rule 'gamma)
      ;; put instantiated gamma and used gamma on fmla queue
      (let [fmla-q (enqueue-used-gamma 
                    (enqueue-fmlas (pop-fmla-queue fmla-q) (:fmla rule-application)) 
                    fmla-struct)]
        {:tree-map (assoc-in tree-map [leaf-index :fmlas]
                             (concat (:fmlas (get tree-map leaf-index)) 
                                     (list (:fmla rule-application))))
         :branch-q (conj (pop branch-q) (assoc select-branch :fmla-q fmla-q))
         :fmla-count (inc fmla-count)
         :gamma-count (inc gamma-count)
         })

      (or (= rule 'delta) (= rule 'double-negative))
      ;; same as gamma, but don't reuse fmla-struct
      (let [fmla-q (enqueue-fmlas (pop-fmla-queue fmla-q) (:fmla rule-application))]
        {:tree-map (assoc-in tree-map [leaf-index :fmlas]
                             (concat (:fmlas (get tree-map leaf-index)) 
                                     (list (:fmla rule-application))))
         :branch-q (conj (pop branch-q) (assoc select-branch :fmla-q fmla-q))
         :fmla-count (inc fmla-count)
         :gamma-count gamma-count
         })

      (= rule 'atomic)
      ;; attempt to unify negation of this formula with a prior atomic formula 
      ;; on this branch
      ;; and propagate substitution toward root
      (let [closing-substs (closing-substitutions fmla-struct atomic-fmlas)]
        {:tree-map (propagate-substs leaf-index closing-substs tree-map) 
         :branch-q (conj (pop branch-q) 
                         (assoc select-branch
                                :fmla-q (pop-fmla-queue fmla-q)
                                :atomic-fmlas (conj atomic-fmlas fmla-struct)))  
         :fmla-count fmla-count
         :gamma-count gamma-count
         })
      )))


;;; adds known closing substitutions at a node in tree and propagates upward as
;;; much as possible
(defn propagate-substs
  [index substs tree-map]
  (loop [index index
         substs substs
         tree-map tree-map]
    (if (empty? substs)
      tree-map
      (let [prior-substs (:closing-substs (get tree-map index))
            add-result (add-substs-to-mguset substs prior-substs)
            added-substs (:added add-result)
            displaced-substs (:displaced add-result)]
        (if (empty? added-substs)
          (if (empty? displaced-substs) 
            tree-map
            (assoc-in tree-map [index :closing-substs] (:mguset add-result)))
          (let [partially-updated-tree-map 
                (assoc-in tree-map [index :closing-substs] (:mguset add-result))]
            (if (= 0 index)
              partially-updated-tree-map
              (let [sibling-substs (:closing-substs (get partially-updated-tree-map 
                                                    (sibling index)))
                    cart-sum (cartesian-sum-of-subst-sets 
                              added-substs 
                              sibling-substs)]
                (recur (parent index) cart-sum partially-updated-tree-map)))))))))

;;; convert to structure suitable for printing
(defn convert-tree-map
  ([tree-map subst]
   (convert-tree-map tree-map 0 subst)
   )

  ([tree-map root-index subst]
   (if (empty? (get tree-map root-index))
     nil
     (let [left-tree (convert-tree-map tree-map (left root-index) subst)
           right-tree (convert-tree-map tree-map (right root-index) subst)

           root-fmlas (reverse (:fmlas (get tree-map root-index)))
           tree {:label (print-formula-struct (update-in (first root-fmlas) [:fmla] apply-subst subst)) 
                 :left left-tree
                 :right right-tree}]
       (loop [rest-fmlas (rest root-fmlas)
              tree tree]
         (if (empty? rest-fmlas)
           tree
           (recur
            (rest rest-fmlas)
            {:label (print-formula-struct (update-in (first rest-fmlas) [:fmla] apply-subst subst)) 
             :left tree
             :right nil}
            )))))))

(defn pruned-tree-map
  [tree-map fmla-ids]
  (loop [map tree-map
         pruned-map {}]
    (if (empty? map)
      pruned-map
      (let [[index node] (first map)
            fmlas (:fmlas node)
            filtered-fmlas (filter (fn [f] (contains? (set fmla-ids) (:id f))) fmlas)]
        (recur
         (rest map)
         (if (empty? filtered-fmlas) 
           pruned-map
           (assoc pruned-map index {:fmlas filtered-fmlas})))))))


(defn attempt-proof
  [tableau max-gamma]
  (reset-gensymbols)
  (loop [tableau tableau
         counter 1]
    (cond
      (not (empty? (:closing-substs (get (:tree-map tableau) 0)))) ; success
      ;; TODO - if more than one substitution, print alternate proofs or shortest proof?
      (let [closing-subst (first (:closing-substs (get (:tree-map tableau) 0)))
            relevant-fmla-ids (:dependencies closing-subst)
            pruned-tree (pruned-tree-map (:tree-map tableau) relevant-fmla-ids)
            printable-tree (convert-tree-map pruned-tree (:subst closing-subst))
            ]
        (println)
        (print-tree printable-tree)
        (println)
        ;(print-tree (convert-tree-map (:tree-map tableau) (:subst closing-subst)))
        true)

      (= (:gamma-count tableau) max-gamma) ; failure
      (do
        ;(print-tree (convert-tree-map (:tree-map tableau) nil))
        (list 'gamma-count max-gamma 'reached))

      :else
      (recur (single-step tableau) (inc counter)))))

