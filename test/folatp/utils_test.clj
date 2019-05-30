(ns folatp.utils-test
  (:require [clojure.test :refer :all]
            [folatp.utils :refer :all]))

(deftest test-cond-let

  (is (= '(b 7 12) 
         (cond-let 

          [x (+ 1 2)
           y (* 2 3)] 
          (= x 2) 
          (list 'a x y)

          [x (+ 3 4)
           y (* 3 4)] 
          (= x 7) 
          (list 'b x y)

          [] true 'else)))

  (is (= '(a 3 6) 
         (cond-let 

          [x (+ 1 2)
           y (* 2 3)] 
          (= x 3) 
          (list 'a x y)
          
          [x (+ 3 4)
           y (* 3 4)] 
          (= x 7) 
          (list 'b x y)
          
          [] true 'else)))

  (is (= 'else 
         (cond-let 

          [x (+ 1 2)
           y (* 2 3)] 
          (= x 2) 
          (list 'a x y)

          [x (+ 3 4)
           y (* 3 4)] 
          (= x 5) 
          (list 'b x y)

          [] true 'else)))
)


