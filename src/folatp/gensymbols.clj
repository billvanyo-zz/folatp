(ns folatp.gensymbols)


;;;; symbol generator for tableax proofs

;;; The gamma rule for universally quantified formulas generates new free variables.
;;; The delta rule for existentially quantified formulas generates new Skolem function symbols.
;;; We could use the built-in gensym function, but we prefer symbols like f1, f2, f3 over f7472, etc,
;;; since these symbols will appear in proofs, and we want some degree of elegance in output.

(let [symbol-counter-map (atom {})]

  ; this generates a new symbol with the specified prefix
  (defn gensymbol
    [prefix]
    (symbol (str prefix
                 (get
                  (swap! symbol-counter-map update-in [prefix]
                         (fn [old] (inc (or old 0))))
                  prefix))))

  ; this resets all the symbol counters (as in for starting a new proof)
  (defn reset-gensymbols [] (swap! symbol-counter-map (fn [old] {})))

  ; this sets the counter for a symbol above the value seen for some input symbol during
  ; loading of formulas.
  ; NOTE: not currently employed; it's currently assumed that subscripted variables and 
  ; function symbols won't appear in the initial input formulas.
  (defn reserve-symbol 
    [prefix subscript]
    (swap! symbol-counter-map update-in [prefix] (fn [old] (max (or old 0) subscript))))

)
