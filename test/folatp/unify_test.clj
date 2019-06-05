(ns folatp.unify-test
  (:require [clojure.test :refer :all]
            [folatp.unify :refer :all]))

(deftest test-unify-terms
  ;; these tests were taken from clojure core.unify 
  (is (= {}                           (unify-terms '(A B)            '(A B)              )))
  (is (= {}                           (unify-terms '(f a b)          '(f a b)            )))
  (is (= false                        (unify-terms '(f a a)          'A                  )))
  (is (= '{x 1}                       (unify-terms '(f x y)          '(f 1 y)            )))
  (is (= '{b 2, a 1}                  (unify-terms '(f a b)          '(f 1 2)            )))
  (is (= '{a (q)}                     (unify-terms '(f a b)          '(f (q) b)          )))
  (is (= '{x Y}                       (unify-terms 'x                'Y                  )))
  (is (= '{x y}                       (unify-terms '(f x x)          '(f y y)            )))
  (is (= '{x y}                       (unify-terms '(f x x x)        '(f y y y)          )))
  (is (= false                        (unify-terms '(f x y)          '(g x y)            )))    
  (is (= false                        (unify-terms '(A A)            '(1 2)              )))    
  (is (= false                        (unify-terms '(f a)            '(g 42)             )))    
  (is (= false                        (unify-terms '(A A)            'A                  )))    
  (is (= '{y (h), x (h)}              (unify-terms '(f x (h))        '(f (h) y)          )))
  (is (= false                        (unify-terms '(f (g x) y)      '(f y x)            )))    
  (is (= false                        (unify-terms 'x                '(f x)              )))    
  (is (= '{y (g x)}                   (unify-terms '(f (g x) y)      '(f y (g x))        )))
  (is (= '{z (g x), y (g x)}          (unify-terms '(f (g x) y)      '(f y z)            )))
  (is (= '{a A}                       (unify-terms 'a                'A                  )))
  (is (= '{y Bar, x Foo}              (unify-terms '(f x 42 y 108)   '(f Foo 42 Bar 108) )))
  (is (= false                        (unify-terms '(A B)            '(1 2)              )))
  (is (= '{y A, x A}                  (unify-terms '(f x y A)        '(f y x x)          ))))

(deftest exp-cases
  (is (= '{x1 (g x0 x0),
           x2 (g (g x0 x0) (g x0 x0)),
           x3 (g (g (g x0 x0) (g x0 x0)) (g (g x0 x0) (g x0 x0))),
           x4 (g (g (g (g x0 x0) (g x0 x0)) (g (g x0 x0) (g x0 x0)))
                 (g (g (g x0 x0) (g x0 x0)) (g (g x0 x0) (g x0 x0)))),
           x5 (g (g (g (g (g x0 x0) (g x0 x0)) (g (g x0 x0) (g x0 x0)))
                    (g (g (g x0 x0) (g x0 x0)) (g (g x0 x0) (g x0 x0))))
                 (g (g (g (g x0 x0) (g x0 x0)) (g (g x0 x0) (g x0 x0)))
                    (g (g (g x0 x0) (g x0 x0)) (g (g x0 x0) (g x0 x0)))))}
         (unify-terms '(f x1 x2 x3 x4 x5) '(f (g x0 x0) (g x1 x1) (g x2 x2) (g x3 x3) (g x4 x4)))))
  
  (is (= '{x0 (g (g (g (g (g x5 x5) (g x5 x5)) (g (g x5 x5) (g x5 x5)))
                    (g (g (g x5 x5) (g x5 x5)) (g (g x5 x5) (g x5 x5))))
                 (g (g (g (g x5 x5) (g x5 x5)) (g (g x5 x5) (g x5 x5)))
                    (g (g (g x5 x5) (g x5 x5)) (g (g x5 x5) (g x5 x5))))),
           x1 (g (g (g (g x5 x5) (g x5 x5)) (g (g x5 x5) (g x5 x5)))
                 (g (g (g x5 x5) (g x5 x5)) (g (g x5 x5) (g x5 x5)))),
           x2 (g (g (g x5 x5) (g x5 x5)) (g (g x5 x5) (g x5 x5))),
           x3 (g (g x5 x5) (g x5 x5)),
           x4 (g x5 x5)}
         (unify-terms '(f x0 x1 x2 x3 x4) '(f (g x1 x1) (g x2 x2) (g x3 x3) (g x4 x4) (g x5 x5)))))
  
  (is (= false
         (unify-terms '(f x1 x2 x3 x4 x0) '(f (g x0 x0) (g x1 x1) (g x2 x2) (g x3 x3) (g x4 x4))))))


(deftest test-norvig-bug-cases
  (is (= '{x y}                     (unify-terms '(p x y)             '(p y x))))
  (is (= '{y A, x A}                (unify-terms '(p x y A)           '(p y x x))))
  (is (= '{y x, z (p x x)}          (unify-terms '(q (p x y) (p y x)) '(q z z)))))

(deftest test-unify-substs
  (is (= '{x (f A B), y A, z A, a A, b B}
         (unify-substitutions '{x (f A b) y a} '{x (f a B) y z})))

  (is (= false
         (unify-substitutions '{x (f A b) y a} '{x (f a B) y b})))
)
