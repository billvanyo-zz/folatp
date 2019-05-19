(ns folatp.gensymbols-test
  (:require [clojure.test :refer :all]
            [folatp.gensymbols :refer :all]))


(deftest test-gensyms
  (reserve-symbol "x" 3)
  (reserve-symbol "x" 2)
  (is (= 'f1 (gensymbol "f")))
  (is (= 'f2 (gensymbol "f")))
  (is (= 'f3 (gensymbol "f")))
  (is (= 'f4 (gensymbol "f")))
  (is (= 'x4 (gensymbol "x")))
  (is (= 'f5 (gensymbol "f")))
  (is (= 'y1 (gensymbol "y")))
  (is (= 'x5 (gensymbol "x")))
  (reset-gensymbols)
  (is (= 'f1 (gensymbol "f")))
  (is (= 'f2 (gensymbol "f")))
  (is (= 'x1 (gensymbol "x")))
  (is (= 'x2 (gensymbol "x")))
)

