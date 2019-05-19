(ns folatp.unify-test
  (:require [clojure.test :refer :all]
            [folatp.unify :refer :all]))

;;; NOTE - these unification tests were taken from clojure core.unify and are (IMHO) technically incorrect in spots.
;;; Example: unifying variables x and y can correctly return either {x y} or {y x}, while 
;;; tests expect a particular one.

(deftest test-unify-terms
  (is (= {}                           (unify-terms '(A B)            '(A B)              )))
  (is (= {}                           (unify-terms '(f a b)          '(f a b)            )))
  (is (= false                        (unify-terms '(f a a)          'A                  )))
  (is (= '{x 1}                       (unify-terms '(f x y)          '(f 1 y)            )))
  (is (= '{b 2, a 1}                  (unify-terms '(f a b)          '(f 1 2)            )))
  (is (= '{a (q)}                     (unify-terms '(f a b)          '(f (q) b)          )))
  (is (= '{x Y}                       (unify-terms 'x                'Y                  )))
  (is (= '{x y}                       (unify-terms '(f x x)          '(f y y)            )))
  (is (= '{x y}                       (unify-terms '(f x x x)        '(f y y y)          )))
  (is (= false                        (unify-terms '(f x y)          '(g x y)            )))     ; clash
  (is (= false                        (unify-terms '(A A)            '(1 2)              )))     ; clash
  (is (= false                        (unify-terms '(f a)            '(g 42)             )))     ; clash
  (is (= false                        (unify-terms '(A A)            'A                  )))     ; clash
  (is (= '{y (h), x (h)}              (unify-terms '(f x (h))        '(f (h) y)          )))
  (is (= false                        (unify-terms '(f (g x) y)      '(f y x)            )))     ; cycle
  (is (= false                        (unify-terms 'x                '(f x)              )))     ; cycle
  (is (= '{y (g x)}                   (unify-terms '(f (g x) y)      '(f y (g x))        )))
  (is (= '{z (g x), y (g x)}          (unify-terms '(f (g x) y)      '(f y z)            )))
  (is (= '{a A}                       (unify-terms 'a                'A                  )))
  (is (= '{y Bar, x Foo}              (unify-terms '(f x 42 y 108)   '(f Foo 42 Bar 108) )))
  (is (= false                        (unify-terms '(A B)            '(1 2)              )))
  (is (= '{y A, x y}                  (unify-terms '(f x y A)        '(f y x x)          ))))


(deftest test-norvig-bug-cases
  (testing "that the unification of the problem cases in Norvig's paper
            'Correcting A Widespread Error in Unification Algorithms'. An
            incorrect unifier will return nil or loop forever."
    (is (= '{x y}                     (unify-terms '(p x y)             '(p y x))))
    (is (= '{y A, x y}                (unify-terms '(p x y A)           '(p y x x))))
    ;; higher-order predicates!
    (is (= '{y x, z (p x y)}          (unify-terms '(q (p x y) (p y x)) '(q z z))))))
