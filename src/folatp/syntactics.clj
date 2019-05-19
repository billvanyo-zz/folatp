(ns folatp.syntactics)

(defn upper-case-symbol? 
  [x] 
  (and (symbol? x) (Character/isUpperCase (get (name x) 0)))
  )

(defn lower-case-symbol? 
  [x] 
  (and (symbol? x) (Character/isLowerCase (get (name x) 0)))
  )

(defn binary-connective?
  [x]
  (let [binary-connectives #{'and 'or 'imp 'if 'iff 'xor 'nif 'nimp 'nor 'nand}]
    (contains? binary-connectives x))
)

;;; if it's a term, returns either true, or a non-empty list of free variables of the term
;;; otherwise returns nil
(defn is-term?
  [x]
  nil ; TBD?
)

;;; a primitive structural matcher
;;; pattern is a list with :keywords
(defn matches
  ([pattern expr]
   (matches pattern expr {}))
  ([pattern expr map]
   (cond
     (keyword? pattern) (if (contains? map pattern)
                          (and (= (pattern map) expr) map)
                          (assoc map pattern expr))
     (symbol? pattern) (and (= pattern expr) map)

     (symbol? expr) false
     (and (list? pattern) (list? expr)) 
     (cond
       (and (empty? pattern) (empty? expr)) map
       (or (empty? pattern) (empty? expr)) false
       :else (let [match-first (matches (first pattern) (first expr) map)]
               (if (not match-first)
                 match-first
                 (matches (rest pattern) (rest expr) match-first))))
     :else false)))

;;; a map of the logical symbols
(def sym 
  {
   :not \u00AC
   :and \u2227
   :or \u2228
   :imp \u2283
   :if \u2228
   :iff \u2261
   :xor \u2262
   :nif \u2284
   :nimp \u2285
   :nor \u2193
   :nand \u2191
   :forall \u2200
   :exists \u2203
   :possibly \u25C7
   :necessarily \u25FB
   })

;;; a map of digit subscripts, for printing subscripted symbols
(def sub 
  {"0" \u2080
   "1" \u2081
   "2" \u2082
   "3" \u2083
   "4" \u2084
   "5" \u2085
   "6" \u2086
   "7" \u2087
   "8" \u2088
   "9" \u2089})


;;; The print-table is used by a print function, which converts list representations
;;; of formulas to pretty FOL strings for printing.
;;; It contains patterms for syntax matching with formulas, paired with functions.
;;; The functions take the print function as argument (for recursive calls)
;;; as well as the map result of the pattern match.
;;; TODO: I feel like this can be further reduced, but maybe at expense of making it cryptic.
(def print-table 
  {
   '(not :f) (fn [pf map] (str (:not sym) (pf (:f map))))
   '(:f1 and :f2)    (fn [pf map] (str "(" (pf (:f1 map)) " " (:and sym) " " (pf (:f2 map)) ")"))
   '(:f1 or :f2)     (fn [pf map] (str "(" (pf (:f1 map)) " " (:or sym) " " (pf (:f2 map)) ")"))
   '(:f1 imp :f2)    (fn [pf map] (str "(" (pf (:f1 map)) " " (:imp sym) " " (pf (:f2 map)) ")"))
   '(:f1 if :f2)     (fn [pf map] (str "(" (pf (:f1 map)) " " (:if sym) " " (pf (:f2 map)) ")"))
   '(:f1 iff :f2)    (fn [pf map] (str "(" (pf (:f1 map)) " " (:iff sym) " " (pf (:f2 map)) ")"))
   '(:f1 xor :f2)    (fn [pf map] (str "(" (pf (:f1 map)) " " (:xor sym) " " (pf (:f2 map)) ")"))
   '(:f1 nif :f2)    (fn [pf map] (str "(" (pf (:f1 map)) " " (:nif sym) " " (pf (:f2 map)) ")"))
   '(:f1 nimp :f2)   (fn [pf map] (str "(" (pf (:f1 map)) " " (:nimp sym) " " (pf (:f2 map)) ")"))
   '(:f1 nor :f2)    (fn [pf map] (str "(" (pf (:f1 map)) " " (:nor sym) " " (pf (:f2 map)) ")"))
   '(:f1 nand :f2)   (fn [pf map] (str "(" (pf (:f1 map)) " " (:nand sym) " " (pf (:f2 map)) ")"))
   '(forall :v :f)   (fn [pf map] (str "(" (:forall sym) (pf (:v map)) ")" (pf (:f map))))
   '(exists :v :f)   (fn [pf map] (str "(" (:exists sym) (pf (:v map)) ")" (pf (:f map))))
   '(possibly :f)    (fn [pf map] (str (:possibly sym) (pf (:f map))))
   '(necessarily :f) (fn [pf map] (str (:necessarily sym) (pf (:f map))))
   }
)

;;; Generates print string for symbols.  
;;; Generated free variables and skolem functions start with '?' (internally) and have subscripts.
;;; Original variables and functions may have subscripts too.
(defn print-symbol
  [s]
  (let [name (if (= \? (get (name s) 0)) (subs (name s) 1) (name s))]
    (clojure.string/replace name #"[0123456789]" #(str (get sub %1)))))

(declare print-formula)

;;; Generates comma separated list for predicate and function applications.
;;; TODO: simplify
(defn print-params
  [lst]
  (loop [lst lst
         string ""]
    (if (empty? (rest lst))
      (if (= string "") (print-formula (first lst)) (str string "," (print-formula (first  lst))))
      (recur 
       (rest lst) 
       (if (= string "") (print-formula (first lst)) (str string "," (print-formula (first  lst))))))))

(defn print-formula
  [f]
  (if (symbol? f)
    ; either variable or constant
    (print-symbol f)
    (loop [prttab print-table]
      (if (empty? prttab) ; it's either a function, predicate or something unexpected
        (if (list? f) ; this ignores unexpected case of empty list
          (str (print-symbol (first f)) "(" (print-params (rest f)) ")")
          (str " ??>" f "<?? ")) ; ad hoc indicator of problem that won't ever happen
        (let [[first & rest] prttab
              match (matches (get first 0) f)]
          (if match
            ((get first 1) print-formula match)
            (recur rest)))))))

