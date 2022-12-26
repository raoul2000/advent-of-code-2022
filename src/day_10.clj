(ns day-10
  (:require [clojure.string :refer [split-lines split]]))

;; https://adventofcode.com/2022/day/10

;; - part 1 -------------------------------------------

;; The clock circuit ticks at a constant rate; each tick is called a cycle
;; The CPU has a single register, X, which starts with the value 1
;; 
;; - addx V takes two cycles to complete. After two cycles, the X register is 
;; increased by the value V. (V can be negative.)
;; - noop takes one cycle to complete. It has no other effect.


(def sample "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
")

(defn input->instructions
  [s]
  (reduce (fn [acc instr]
            (let [[cmd maybe-operand] (split instr #" ")]
              (conj acc [cmd (when maybe-operand (Integer/parseInt maybe-operand))]))) [] (split-lines s)))


(comment
  (input->instructions sample)
  ;;
  )

(defn instructions->cmd-per-cycle [instr-coll]
  (reduce (fn [acc [cmd operand]]
            (case cmd
              "addx" (into acc [identity (partial + operand)])
              "noop" (conj acc identity))) [] instr-coll))

(defn register-per-cycle [cmd-per-cycle]
  (loop [cmd-per-cycle cmd-per-cycle
         x-by-cycle [1]]
    (if (empty? cmd-per-cycle)
      x-by-cycle
      (recur (rest cmd-per-cycle)
             (conj x-by-cycle ((first cmd-per-cycle) (last x-by-cycle)))))))

(defn solution-1 []
  (let [x-per-cycle (->> (input->instructions 
                          ;;sample
                          (slurp "./resources/puzzle_10.txt")
                          ;;
                          )
                         (instructions->cmd-per-cycle)
                         (register-per-cycle))]
    (+ (* 20 (get x-per-cycle 19))
       (* 60 (get x-per-cycle 59))
       (* 100 (get x-per-cycle 99))
       (* 140 (get x-per-cycle 139))
       (* 180 (get x-per-cycle 179))
       (* 220 (get x-per-cycle 219)))))

(comment
  (solution-1)
  ;; 14060 !!
  ;;
  )

;; ---- part 2 ------------------------------------------------


(defn solution-2 [])


(comment
  (solution-2)
  ;;
  )