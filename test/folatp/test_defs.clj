(ns folatp.test-defs)

(def transitivity '(forall x (forall y (forall z (((R x y) and (R y z)) imp (R x z))))))

(def symmetry '(forall x (forall y ((R x y) imp (R y x)))))

(def nontriviality '(forall x (exists y (R x y))))

(def reflexivity '(forall x (R x x)))
