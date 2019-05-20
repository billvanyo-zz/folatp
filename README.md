# tableaux - This is an incomplete work in progress.

## Tableaux Theorem Prover / Proof Assistant for First-Order Logic
This is an implementation, written in Clojure, of a tableaux based automated theorem prover / proof assistant for first-order logic.  It is based on methods described in the book First-Order Logic and Automated Theorem Proving by Melvin Fitting (https://www.amazon.com/First-Order-Automated-Theorem-Proving-Computer/dp/0387945938).

This is a console based application.  First-order logic formulas are encoded and manipulated as lists of symbols, such as:

'(not ((exists w (forall x (R x w (f x w)))) imp (exists w (forall x (exists y (R x w y))))))

In the tableaux proof tree, they are printed in a more traditional fashion, such as:

¬((∃w)(∀x)R(x,w,f(x,w)) ⊃ (∃w)(∀x)(∃y)R(x,w,y))

