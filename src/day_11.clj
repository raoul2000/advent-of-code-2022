(ns day-11
  (:require [clojure.string :refer [split-lines split]]))

;; https://adventofcode.com/2022/day/11

;; - part 1 -------------------------------------------


(def sample "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
")

(defn divisible-by [div num]
  (zero? (rem num div)))


(defn make-monkey [[items operation divisible if-true if-false]]
  {:items     (mapv #(Integer/parseInt % 10) (split items #", "))
   :op        operation
   :div       (partial divisible-by (Integer/parseInt divisible 10))
   :if-true   if-true
   :if-false  if-false})

(defn input->monkeys [s]
  (->> s
       (re-seq #"Monkey (\d+):\n.+items: (.+)\n.+new = (.+)\n.+by (\d+)\n.+monkey (\d+)\n.+monkey (\d+)")
       (reduce (fn [acc [_ monkey-id & other]]
                 (assoc acc monkey-id (make-monkey other)))
               {})))

(comment
  (input->monkeys sample)
  (rem 5 2)
  (divisible-by  23 100)
  (divisible-by 13 2080)

  (def divisible-by-23? (partial divisible-by 23))

  (divisible-by-23? 230)
  (split-lines sample)

  (->> (split-lines sample)
       (partition-by #(= % ""))
  ;;     (remove #(= 1 (count %)))
       (map #(apply str %)))

  (def matcher (re-matcher #"Monkey (\d+):\n.+items: (.+)\n.+new = (.+)\n.+by (\d+)\n.+monkey (\d+)\n.+monkey (\d+)" sample))
  (re-find matcher)

  (def re-parser #"Monkey (\d+):\n.+items: (.+)\n.+new = (.+)\n.+by (\d+)\n.+monkey (\d+)\n.+monkey (\d+)")
  (re-seq re-parser sample)

  (reduce (fn [acc [_ monkey-id items operation divisible if-true if-false]]
            (assoc acc monkey-id {:items (mapv #(Integer/parseInt % 10) (split items #", "))
                                  :op operation
                                  :div (Integer/parseInt divisible 10)
                                  :if-true if-true
                                  :if-false if-false}))
          {}
          (re-seq re-parser sample))


  (split  "12, 54, 28" #", ")
  (mapv #(Integer/parseInt % 10) (split  "12, 54, 28" #", "))

  ;; testin eval
  (eval "(+ 2 2)")
  (def bob 2)
  (eval (read-string "(+ bob 1)"))
  (eval (read-string "(let [old 3] (+ old 1))"))

  ;;
  )



(defn input->instructions
  "Converts string *s* into a vector of instructions represented as [command operand] pairs. 
   Command with no operand (e.g. 'noop') have a *nil* operand."
  [s]
  (reduce (fn [acc instr]
            (let [[cmd maybe-operand] (split instr #" ")]
              (conj acc [cmd (when maybe-operand (Integer/parseInt maybe-operand))]))) [] (split-lines s)))


(defn solution-1 [])

(comment
  (solution-1)
  ;; 
  ;;
  )

;; ---- part 2 ------------------------------------------------


(defn solution-2 [])


(comment
  (solution-2)

  ;; 
  )


