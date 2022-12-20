(ns day-9
  (:require [clojure.string :refer [split-lines split]]))

;; https://adventofcode.com/2022/day/9

;; - part 1 -------------------------------------------

(def sample "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
")

(defn input->steps
  "Turns puzzle input into a seq of directed steps. For example
   'U 4' is turned into ['U' 'U' 'U' 'U']."
  [s]
  (reduce (fn [acc motion]
            (let [[dir steps] (split motion #" ")]
              (into acc (repeat (Integer/parseInt steps) dir)))) [] (split-lines s)))

(comment
  (input->steps sample)
  ;;
  )

(defn move-one-step
  "Move coordinates *[x y]* one unit in *direction* and returns the new position"
  [[x y] direction]
  (case direction
    "U" [x (dec y)]
    "D" [x (inc y)]
    "L" [(dec x) y]
    "R" [(inc x) y]))

(comment
  (move-one-step [0 0] "U")
  (loop [steps (input->steps sample)
         cur-pos [0 0]
         pos-history []]
    (if (empty? steps)
      pos-history
      (recur (rest steps)
             (move-one-step cur-pos (first steps))
             (conj pos-history cur-pos))))
  ;;
  )

(defn adjacent-pos?
  "Returns TRUE if position *[x1 y1]* and *[x2 y2]* are adjacent position
   or the same position (overlapping)"
  [[x1 y1] [x2 y2]]
  (let [adjacent-positions (for [dx [-1 0 1]
                                 dy [-1 0 1]]
                             [(+ dx x1) (+ dy y1)])]
    (some #{[x2 y2]} adjacent-positions)))

(comment
  (adjacent-pos? [1 1] [1 1])
  (adjacent-pos? [1 1] [2 1])
  (adjacent-pos? [1 1] [2 2])
  ;;
  )

(defn solution-1 []
  (let [motions ;;sample
        (slurp "./resources/puzzle_9.txt")
        ;;
        ]))

