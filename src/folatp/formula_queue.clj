;;;; priority queue for formula structures

;;;; formula structures are wrapped in a structure that contains a counter for fifo ordering
;;;; of used gamma formulas

(ns folatp.formula-queue
  (:require [folatp.formulas :refer :all]
))

(use 'clojure.data.priority-map)

(def fmla-type-priorities
  {:atomic 0
   :double-negated 1
   :alpha 2
   :delta 3
   :gamma 4
   :beta 5
   :used-gamma 6 ; this is converted to :gamma on removal from queue
   }
)

;;; comparator for wrapped formula structures, based first on type, then formula id, except
;;; if both are used gammas, which are forced fifo by using counter
(defn f-queue-comp
  [wrapped-fstr1 wrapped-fstr2]
  (let [type1 (:type (:fstr wrapped-fstr1))
        counter1 (:counter wrapped-fstr1)
        id1  (:id (:fstr wrapped-fstr1))
        type2 (:type (:fstr wrapped-fstr2))
        counter2 (:counter wrapped-fstr2)
        id2  (:id (:fstr wrapped-fstr2))]
    (if (and (= :used-gamma type1)
             (= :used-gamma type2))
      (- counter1 counter2)
      (let [pri1 (type1 fmla-type-priorities)
            pri2 (type2 fmla-type-priorities)]
        (if (= pri1 pri2)
          (- id1 id2)
          (- pri1 pri2)
          )))))

(defn empty-formula-queue
  []
  {:counter 1 :queue (priority-map-by f-queue-comp)})

(defn enqueue-fmla
  [fqueue fmla]
  ;(println "   enqueue-fmla: fqueue" fqueue)
  ;(println "   enqueue-fmla: fmla" fmla)
  (let [counter (:counter fqueue)
        wrapped {:counter counter :fstr fmla}]
    {:counter (inc counter)
     :queue (assoc (:queue fqueue) counter wrapped)}))

(defn enqueue-fmlas
  [fqueue & fmlas]
  ;(println "enqueue-fmlas: fqueue" fqueue)
  ;(println "enqueue-fmlas: fmlas" fmlas)
  (loop [fqueue fqueue
         fmlas fmlas]
    (if (empty? fmlas)
      fqueue
      (recur 
       (enqueue-fmla fqueue (first fmlas))
       (rest fmlas)))))


(defn new-formula-queue
  [& fstructs]
  (apply enqueue-fmlas (empty-formula-queue) fstructs))


(defn enqueue-used-gamma
  [fqueue gamma]
  (enqueue-fmla fqueue (assoc gamma :type :used-gamma)))

(defn pop-fmla-queue
  [fqueue]
  {:counter (:counter fqueue)
   :queue (pop (:queue fqueue))})

(defn next-fmla
  [fqueue]
  (let [wrapped (get (peek (:queue fqueue)) 1)
        fmla (:fstr wrapped)]
    (if (= :used-gamma (:type fmla))
      (assoc fmla :type :gamma)
      fmla
      )))


