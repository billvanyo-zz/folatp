(ns folatp.tree-printer-test
  (:require [clojure.test :refer :all]
            [folatp.tree-printer :refer :all]))

;;; reverse collatz sequence tree
(defn collatz-tree
  ([depth]
   (collatz-tree 1 1 depth))
  ([start cur-length max-length]
   (if (<= cur-length max-length)
     {:label (str start)
      :left (collatz-tree (* 2 start) (inc cur-length) max-length)
      :right (if (and (= 4 (mod start 6)) (> start 4)) 
               (collatz-tree (/ (dec start) 3) (inc cur-length) max-length)
               nil)   
      }
     nil)))

(defn random-tree
  ([n]
   (random-tree 1 n))
  ([first-val last-val]
   (if (> first-val last-val)
     nil
     (let [tree-size (inc (- last-val first-val))
           left-count (rand-int tree-size)]
       {:label (str (+ first-val left-count)) 
        :left (random-tree first-val (dec (+ first-val left-count))) 
        :right (random-tree (+ 1 first-val left-count) last-val)}
       ))))

(deftest visual-tree-print-test
  ; this is a visual test - tree needs to "look right"
  (print-tree (collatz-tree 14))
  (println)
  (print-tree (random-tree 50))
  (println)
  (print-tree (random-tree 50))
  (println)
  (print-tree (random-tree 50))
  (is true)
)

