(ns folatp.debugging
  (:require [folatp.formulas :refer :all]
            [folatp.syntactics :refer :all]
            [folatp.tree-printer :refer :all]
            [folatp.gensymbols :refer :all]
            [folatp.unify :refer :all]
            [folatp.formula-queue :refer :all]
            [folatp.tableaux :refer :all]))

;;; return leaf indexes of tree in left-to-right order
(defn leaf-indexes
  ([tree-map]
   (leaf-indexes tree-map 0))
  ([tree-map root]
   (if (not (contains? tree-map (left root)))
     (list root)
     (concat (leaf-indexes tree-map (left root))
             (leaf-indexes tree-map (right root))
             ))))

(defn atomics
  [tree-map leaf-index]
  (loop [index leaf-index
         all-atomics ()]
    (let [node-atomics (filter (fn [f] (= :atomic (:type f))) 
                               (:fmlas (get tree-map index)))
          ]
      (if (= 0 index)
        (concat node-atomics all-atomics)
        (recur (parent index) (concat node-atomics all-atomics))))))

(defn positive
  [atomics]
  (filter (fn [f] (not= 'not (first f))) atomics))

(defn negative
  [atomics]
  (filter (fn [f] (= 'not (first f))) atomics))

(defn atomic-closing-unifiers
  [atomics]
  (let [pos (positive atomics)
        neg (map second (negative atomics))
        ]
    (filter identity (for [pf pos nf neg] (unify-terms pf nf)))))

;;;
(defn tableau-atomics-by-branch
  [tableau]
  (let [tree-map (:tree-map tableau)
        leaf-indexes (leaf-indexes tree-map)]
    (map (fn [i] {i (atomics tree-map i)}) leaf-indexes)
    ))


(defn iterate-steps
  [tableau fmla-count]
  (loop [tableau tableau]
    (if (< (:fmla-count tableau) fmla-count)
      (recur (single-step tableau))
      tableau)))

;;; takes two sets of substitutions and finds all substitutions that
;;; unify one from each set
(defn unify-subst-sets
  [set1 set2]
  (filter 
   identity 
   (for [sub1 set1 
         sub2 set2] 
     (unify-substitutions sub1 sub2))))

