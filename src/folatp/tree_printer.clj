;;;; Functions for printing a tableau tree structure in ASCII.
;;;; This is an adaptation of Java code at https://github.com/billvanyo/tree_printer, but without 
;;;; all the unnecessary options.

(ns folatp.tree-printer)

;;; print representation of a tree is a vector of 3-element map structures, with the 3 elements being:
;;; :l - a single line to be printed
;;; :loff - how far this line extends to left of root position
;;; :roff - how far this line extends to right of root position

;;; touch-distance calculates the minimum distance between the root of two print representations of
;;; trees so that some pair of lines just 'touch' without overlapping
(defn touch-distance
  [left-linevec right-linevec]
  (let [min-height (min (count left-linevec) (count right-linevec))]
    (loop [x 0
           max-dist 0]
      (if (= x min-height)
        max-dist
        (recur 
         (+ x 1)
         (max max-dist (- (:roff (get left-linevec x)) (:loff (get right-linevec x)))))))))

;;; given two subtree print representations (either/both of which may be empty), and distance between 
;;; their roots, combine them under a new root
(defn combine-root-and-subtrees 
  [root-label root-dist left-lines right-lines]
  (let [root-line {:l root-label 
                   :loff (quot (- 1 (count root-label)) 2) 
                   :roff (quot (count root-label) 2)}]
    (if (and (empty? left-lines) (empty? right-lines))
      [root-line]
      (let [one-tree? (not= (empty? left-lines) (empty? right-lines))
            adjust (if one-tree? 0 (+ 1 (quot root-dist 2)))
            branch-line (if one-tree?
                          ;{:l "\u2502" :loff 0 :roff 0} ; vertical bar between node and single subnode
                          false
                          (let [hbar (apply str (repeat (quot root-dist 2) "\u2500"))
                                branch-str (str "\u250C" hbar "\u2534" hbar "\u2510")]
                            {:l branch-str :loff (- adjust) :roff adjust}))
            max-height (max (count left-lines) (count right-lines))]
        (loop [lines (if branch-line [root-line branch-line] [root-line])
               i 0]
          (if (= i max-height)
            lines
            (let [line (cond
                         ; 3 cases: only lines of right or left subtree remain, or lines of both which
                         ; need to be concatenated with appropriate number of spaces between them
                         (>= i (count left-lines)) (let [l (get right-lines i)] 
                                                     {:l (:l l) 
                                                      :loff (+ (:loff l) adjust) 
                                                      :roff (+ (:roff l) adjust)})
                         (>= i (count right-lines)) (let [l (get left-lines i)] 
                                                      {:l (:l l) 
                                                       :loff (- (:loff l) adjust) 
                                                       :roff (- (:roff l) adjust)})
                         :else (let [ll (get left-lines i)
                                     lr (get right-lines i)
                                     num-spaces (+ root-dist (- (:roff ll)) (:loff lr))
                                     lstr (str (:l ll) (apply str (repeat num-spaces " ")) (:l lr))] 
                                 {:l lstr :loff (- (:loff ll) adjust) :roff (+ (:roff lr) adjust)})
                         )]
              (recur (conj lines line) (+ 1 i)))))))))

(defn build-tree-lines
  [tree]
  (if (empty? tree)
    []
    (let [root-label (:label tree)
          left-lines (build-tree-lines (:left tree))
          right-lines (build-tree-lines (:right tree))
          ; calculate "touch distance" for the two lists of lines
          touch-dist (touch-distance left-lines right-lines)
          root-dist (if (even? touch-dist) (+ touch-dist 3) (+ touch-dist 2))
          ]
          ; now create root line and branches, and zip the sub trees together
      (combine-root-and-subtrees root-label root-dist left-lines right-lines))))


;;; takes vector of {:l "..." :loff -5 :roff 7}
(defn print-tree-lines
  [linevec]
  (let [min-loff 
        (apply min (map :loff linevec))
        ]
    (doseq [line linevec] 
      (printf (str "%" (inc (- (:loff line) min-loff)) "s%s%n") "" (:l line))
)))

(defn print-tree [tree] (print-tree-lines (build-tree-lines tree)))

