(ns folatp.utils)


;;; macro to combine conditional expression with let style bindings.
;;; The application is in cases where we would have a cond expression, but some function calls
;;; are used for both the test part of the cond clause, and for the return value.
;;; In this case, we put those function calls in a let style binding, and use the variables 
;;; in both the clause test and the return value.
;;; TODO validate arguments (multiple of three, bindings must be a vector as is acceptable in let)
(defmacro cond-let
	([] nil)
	([bindings test expr & rest]
	`(let ~bindings (if ~test ~expr (cond-let ~@rest)))
	)
)


