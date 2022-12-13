(ns day-8
  (:require [clojure.string :refer [split-lines split]]
            [clojure.walk :refer [walk]]))

;; https://adventofcode.com/2022/day/8

;;

(def sample "30373
25512
65332
33549
35390
")

(defn sample->grid [s]
  (->> s
       (split-lines)
       (mapv #(mapv (fn [c] (Character/digit c 10)) %))))

(defn count-edge-trees [grid]
  (+ (* 2 (count grid))
     (* 2 (- (count (first grid)) 2))))


(def grid-width (comp count first))
(def grid-height count)

(defn get-coll [x grid]
  (mapv #(get % x) grid))

(defn get-row [y grid]
  (get grid y))

(defn get-xy [grid x y]
  (get (into [] (for [idx (range 0 (grid-width grid))]
                  (get (get grid idx) x))) y))

(defn split-around-idx [v idx]
  (reduce-kv (fn [[start end :as acc] k v]
               (cond
                 (< k idx) [(conj start v) end]
                 (> k idx) [start (conj end v)]
                 :else acc)) [[] []] v))

(defn inner-grid-xy [width height]
  (for [x (range 1 (dec width))
        y (range 1 (dec height))]
    [x y]))


(comment

  (def grid [[3 0 3 7 3]
             [2 5 5 1 2]
             [6 5 3 3 2]
             [3 3 5 4 9]
             [3 3 5 4 9]
             [3 5 3 9 0]])

  (defn visible? [[x y] grid]
    (let [[top bottom] (split-around-idx (get-coll x grid) y)
          [left right] (split-around-idx (get-row  y grid) x)
          tree-height  (get-xy grid x y)]
      (some (partial < tree-height) (concat top bottom left right))))

  (loop [xy (inner-grid-xy 5 5)
         mx grid
         cnt 0]
    (if (empty? xy)
      cnt
      (recur (rest xy)
             mx
             (if (visible? (first xy) grid)
               (do
                 (println "visible:" (first xy))
                 (inc cnt)
                 )
               
               cnt))))

  (apply into [1] [2 4 5] [:a :v])
  (concat [1] [2 4 5] [:a :v])
  (apply > [5 4 3 2 3 4])
  (some (partial < 5) [1 2 3 6])

  ;;
  )


(comment

  (->> sample
       (split-lines)
       (mapv #(mapv (fn [c] (Character/digit c 10)) %)))

  (def grid [[3 0 3 7 3]
             [2 5 5 1 2]
             [6 5 3 3 2]
             [3 3 5 4 9]
             [3 3 5 4 9]
             [3 5 3 9 0]])
  (def width (count (first grid)))
  (def height (count  grid))
  ;; start idx = width + 2
  ;; len = width - 2


  (grid-width  grid)
  ;; ---------------------------------
  ;; get col 1
  (get (get grid 0) 1)
  (get (get grid 1) 1)
  (get (get grid 2) 1)
  (get (get grid 3) 1)
  (get (get grid 4) 1)
  ;; get col 1
  (for [idx (range 0 (inc width))]
    (get (get grid idx) 1))
  ;; get col 2
  (for [idx (range 0 (inc width))]
    (get (get grid idx) 2))
  (map #(get % 2) grid)

  ;; get coll x
  (defn get-coll [x grid]
    (let [width (count (first grid))]
      (for [idx (range 0 (inc width))]
        (get (get grid idx) x))))

  (get-coll 0 grid)
  (get-coll 1 grid)

  ;; -----------------------------------
  ;; get line 1
  (get grid 1)
  ;; get line 2
  (get grid 2)
  ;; get line 2
  (get grid 2)
  (defn get-row [y grid]
    (get grid y))

  (get-row 0 grid)
  (get-row 1 grid)

  ;; split a vector in 2 parts before and after idx

  (defn split-by-idx [v idx]
    (reduce-kv (fn [[start end :as acc] k v]
                 (cond
                   (< k idx) [(conj start v) end]
                   (> k idx) [start (conj end v)]
                   :else acc)) [[] []] v))

  (split-by-idx [3 4 5 6 7]  3)
  (split-by-idx [3 4 5 6 7]  1)

  ;; get all (x,y) for inner grid
  (defn inner-grid-xy [width height]
    (for [x (range 1 (dec width))
          y (range 1 (dec height))]
      [x y]))

  (inner-grid-xy 5 5)

  ;; item at pos x=1 y=1
  (get (into [] (for [idx (range 0 width)]
                  (get (get grid idx) 1))) 1)

  (defn get-xy [v x y]
    (get (into [] (for [idx (range 0 width)]
                    (get (get v idx) x))) y))
  (get-xy grid 1 1)
  (get-xy grid 2 1)
  (get-xy grid 2 0)
  (get-xy grid 3 1)
  (get-xy grid 2 2)

  ;;
  )


;; (slurp "./resources/puzzle_7.txt")
(defn solution-1 [])

(comment
  (solution-1))


;; part 2 -------------------------------------------------


(defn solution-2 [])

(comment
  (solution-2)
  ;; => 13210366
  )
