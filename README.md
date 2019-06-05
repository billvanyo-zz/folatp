# First-Order Logic Tableaux Theorem Prover implemented in Clojure

Note: This is newly developed, basically working code, but needs much cleaning up.  I will also post a link to a youtube video demonstrating it in action.

This is an implementation, written in Clojure, of a tableaux based automated theorem prover / proof assistant for first-order logic.  It is based on methods described in the book [First-Order Logic and Automated Theorem Proving by Melvin Fitting](https://www.amazon.com/First-Order-Automated-Theorem-Proving-Computer/dp/0387945938).

This is a console based application.  First-order logic formulas are encoded and manipulated as lists of symbols, such as:

'(not ((exists w (forall x (R x w (f x w)))) imp (exists w (forall x (exists y (R x w y))))))

In the tableaux proof tree, they are printed in a more traditional fashion, such as:

¬((∃w)(∀x)R(x,w,f(x,w)) ⊃ (∃w)(∀x)(∃y)R(x,w,y))

## The Tableaux Proof Method, as implemented

The tableaux method proves a formula through refutation of the formula's negation.  Refutation is by deriving a contradiction.  See [Wikipedia on First-order tableau with unification](https://en.wikipedia.org/wiki/Method_of_analytic_tableaux#First-order_tableau_with_unification) for details.

The guts of the implementation consists of:
* a data structure which we'll call a tableau
* a function called `single-step` which takes a tablea and returns a tableau

`single-step` applies one of several possible rules to a tableau data structure to generate a new tableau data structure. Briefly, the rules are:
* alpha - a conjunctive formula on a branch is split into its conjuncts and they are added to the branch
* beta - a disjuntive formula on a branch is split into its disjuncts and they are placed on two new sub-branches
* gamma - a universally quantified formula has the quantified variable replaced with a new free variable
* delta - an existentially quantified formula has the quantified variable replaced with a new constant, or more generally a new skolem function of any free variables in the formula
* atomic closure - an atomic formula is unified with occurrences of its negation on the same branch, producing unifying substitutions, which are propagated toward the root of the tree

The gamma rule is the only rule that may need to be applied multiple times for a given formula.  This is because a universally quantified formula is true for a multiplicity of objects, and multiple of these may need to be considered in demonstrating a contradiction exists.

The beta rule is the only rule that increases the number of branches on a tableau.

Indefinitely increasing branching of a tableau occurs when repeated applications of the gamma rule expose new instances of beta formulas.

Successful application of the atomic closure rule demonstrates a contradiction on a branch of the tableau, in that there is a substitution of values for free variables which make an atomic formula and it's negation identical.  These substitutions are found by a unification algorith.  However, a tableau is essentially a disjunction of all its branches, and to refute a disjunction, each of the disjuncts must be refuted. An atomic closure must occur on each branch.  Moreover, the substitutions generated in finding these atomic closures must themselves all be unifiable to a single substitution.  Much of the code deals with operations on substitutions and sets of substitutions (maintaining sets of substitutions as antichains under a "more general" partial ordering, computing cartesian sums of sets of substitions that close two beta branches, etc).

At the heart of a tableau structure is a binary tree. This is represented as a map from integer node indexes to node structures.  The root node has index 0.  A node with index `n` has a left sub-node with index `2n+1` and a right sub-node with index `2n+2`.

A branch of a tableau consists of the nodes from a leaf node up to the root.

The tableau data structure contains:
* the binary tree of nodes, each of which which contains:
  * a collection of annotated formula structures
  * a collection of annotated substitution structures
* a queue of branch structures, each of which contains:
  * index of the leaf node for the branch
  * a priority queue of formulas on the branch that rules may be applied to
  * a list of atomic formulas that are on the branch

An annotated formula structure contains:
* a first-order formula (as a list structure)
* a unique integer id
* a type (alpha, beta, gamma, delta, double-negative or atomic)
* the formula's derivation (axiom, negated goal, or derived by rule application from prior formula)
* list of free variables in the formula

An annotated substitution structure which contains:
* a substitution (which is a map from variables to terms)
* a type (was it generated from atomic formulas, or by unification of substitutions from two beta branches)
* two atomic formula structures (if generated from closure on two atomic formulas)
* two substition structures (if generated by unification of substitutions from two beta sibling branches)

When atomic closure happens, substitutions are propagated toward the root of the tree by unification with substitutions from sibling branches.  When a substitution reached the root of the tree, a proof has been found, and the substitution structure contains all the information relevant to deriving that substitution.  This information can be used to prune the tree of unnecessary speculative rule applications and possibly branches.  The substitution can be applied to all formulas in the resulting pruned tree, and the tree printed, to make the proof clear.

# How To Use It

For now, some quick examples.  First, a proof that transitivity, symmetry and nontriviality implies reflexivity.  We define the axioms and the thing to be proved, illustrating the syntax (this will be defined precisely at a later time, but may be obvious).

```
(def transitivity '(forall x (forall y (forall z (((R x y) and (R y z)) imp (R x z))))))
(def symmetry '(forall x (forall y ((R x y) imp (R y x)))))
(def nontriviality '(forall x (exists y (R x y))))
(def reflexivity '(forall x (R x x)))
```
Then we call a function to create an initial tableau data structure, passing a list of the axioms, and the theorem to be proved, as follows:

```
(def tab1 (init-tableau (list transitivity symmetry nontriviality) reflexivity))
```
Then we call the `attempt-proof` function, passing the initial tableau, and an integer, which specifies the maximum number of times the gamma rule may be applied.  In this case, I know that 8 is sufficient. So:

```
folatp.tableaux> (attempt-proof tab1 8)

   (∀x)(∀y)(∀z)((R(x,y) ∧ R(y,z)) ⊃ R(x,z)) 1=premise
          (∀x)(∀y)(R(x,y) ⊃ R(y,x)) 2=premise
                (∀x)(∃y)R(x,y) 3=premise
                  ¬(∀x)R(x,x) 4=¬goal
                    ¬R(X₁,X₁) 5=δ.4
      (∀y)(∀z)((R(X₁,y) ∧ R(y,z)) ⊃ R(X₁,z)) 6=γ.1
             (∀y)(R(X₁,y) ⊃ R(y,X₁)) 7=γ.2
                   (∃y)R(X₁,y) 8=γ.3
                   R(X₁,y₁(X₁)) 9=δ.8
  (∀z)((R(X₁,y₁(X₁)) ∧ R(y₁(X₁),z)) ⊃ R(X₁,z)) 10=γ.6
          (R(X₁,y₁(X₁)) ⊃ R(y₁(X₁),X₁)) 11=γ.7
   ((R(X₁,y₁(X₁)) ∧ R(y₁(X₁),X₁)) ⊃ R(X₁,X₁)) 12=γ.10
              ┌────────────┴────────────┐
    ¬R(X₁,y₁(X₁)) 13=β₁.11   R(y₁(X₁),X₁) 14=β₂.11
                        ┌───────────────┴───────────────┐
     ¬(R(X₁,y₁(X₁)) ∧ R(y₁(X₁),X₁)) 17=β₁.12   R(X₁,X₁) 18=β₂.12
           ┌────────────┴────────────┐
 ¬R(X₁,y₁(X₁)) 21=β₁.17   ¬R(y₁(X₁),X₁) 22=β₂.17
```

The tree displays formulas with numeric labels and a notation indicating how they were derived.  The first three are premises or axioms.  The 4th is the negated goal (the goal to be proved; the negated goal to be refuted). The 5th is the result of applying the delta rule to 4. Note that variables are lowercase, constant symbols are uppercase.  Relation symbols can be either, and are understood by context, though I use the convention of relation symbols being uppercase, and function symbols being lowercase. The 6th, 7th and 8th formulas result from application of the gamma rule. The 9th formula is from the delta rule, and illustrates introduction of a Skolem function  There are 3 more gamma rule applications, and then the beta rule is applied to formulas 11, 12 and 17 resulting in four branches.

Note that the numeric labels are not contiguous because the final tree is "pruned" to include only formulas relevant to the final proof.

## Planned Improvements
1) Currently, the input formulas should not contain subscripted variables or function symbols like x1 or f1, as the proof procedure generates new variables and function symbols, and they may not be 'new' if they are in the input formulas.
2) Notation will be added at the ends of branches to indicate which formula it closes with.
3) Skolem functions in the final output will be replaced with unique constants (this may be an option; some may prefer to see the skolemization in the output).
4) Numeric formula labels will be renumbered in the end to keep them contiguous.
5) Add the equality relation.
6) Concurrency (probably a significant rewrite).






