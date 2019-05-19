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

(deftest visual-tree-print-test
  (print-tree (collatz-tree 14))
  (is true)
)

