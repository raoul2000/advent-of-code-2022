(ns day-5
  (:require [clojure.string :refer [split-lines split]]
            [clojure.set :refer [intersection]]))

;; https://adventofcode.com/2022/day/5

;; are moved one at a time
;; which crate will end up on top of each stack
;;

(def sample "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

;; stacks line
;; ... ... ...\n
;; [Z] [M] [P]
;; stack will be vector where top element is first vector item
;; [G]
;; [T]
;; [Y]
;;
;; => [G T Y]


(comment
  (re-matches #"[ \d]+" "   ")
  (re-matches #"(... ?)+"  "    [P]                 [Q]     [T]")
  (def m (re-matcher #"(... ?)+"  "    [P]                 [Q]     [T]"))
  (re-find m)

  (partition 3 4 "    [P]                 [Q]     [T]")

  (map (fn [coll]
         (if (= \space (first coll))
           nil
           (second coll))) (partition 3 4 "    [P]                 [Q]     [T]"))

  (split-lines sample)
  (partition-by #(= "" %) (split-lines sample))
  (let [[indexed-stack _  moves] (partition-by #(= "" %) (split-lines sample))
        stack (butlast indexed-stack)]
    ;;stack
    moves)

  (re-matches #"move (\d+) from (\d+) to (\d+)"  "move 1 from 2 to 8")
  ;;
  )

(defn parse-move
  "Given *s* a move description, returns a coll of 3 int values where
   the first is the count of items to move, the second and thirst the 1 based index
   of the source and target stack."
  [s]
  (->> (re-matches #"move (\d+) from (\d+) to (\d+)"  s)
       (rest)
       (map #(Integer/parseInt % 10))))

(comment
  (parse-move  "move 8 from 1 to 4")
  (parse-move  "move 12 from 8 to 7"))

(defn parse-stack-line [s]
  (map (fn [coll]
         (when-not (= \space (first coll))
           (second coll))) (partition 3 4 s)))

(defn create-stack-coll
  "Given coll of *lines* representing stacks content, returns a vector
   of stacks where the first item is the last inserted item."
  [lines]
  (->> (map parse-stack-line lines)
       (apply map (fn [& args] (apply vector args)))
       (map (partial drop-while nil?))
       (into [])))

(defn apply-single-move [stack-coll [q from to]]
  (let [to-move (take q (stack-coll (dec from)))]
    (-> stack-coll
        (update ,,, (dec from) #(drop q %))
        (update ,,, (dec to)   #(into % to-move)))))

(comment

  (def st ['(\F \H \M \T \V \L \D)
           '(\P \N \T \C \J \G \Q \H)
           '(\H \P \M \D \S \R)
           '(\F \V \B \L)
           '(\T \M \Z \J \Q \L \D \R)])

  (st 0)
  ;; take 2 from top 
  (drop 2 (st 0))
  ;; put 2 on top
  (into (st 1) '(:a :b))



  (def v-move (take 2 (st 0)))
  (update st 0 #(drop 2 %))
  (update st 1 #(into % v-move))

  (apply-single-move st [2 1 2])
  ;;
  )


(defn apply-all-moves [stack-coll move-coll]
  (if (empty? move-coll)
    stack-coll
    (recur (apply-single-move stack-coll (first move-coll))
           (rest move-coll))))

(defn solution-1 []
  (let [lines                          (split-lines (slurp "./resources/puzzle_5.txt"))
        [indexed-stack _  moves-lines] (partition-by #(= "" %) lines)
        stack-coll                     (create-stack-coll (butlast indexed-stack))
        move-coll                      (map parse-move moves-lines)]
    (apply-all-moves stack-coll move-coll)))

(comment
  (solution-1)
  ;;
  ;;[(\H \N \M \C)
  ;;(\N \J)
  ;;(\S)
  ;;(\N \V \L \L)
  ;;(\M \D \L \C)
  ;;(\T \D \H \M \G \T \R \B \H \W \L \P \J \Q \T \W \Q \F \V \B \R \M)
  ;;(\L \L \P \H \G \P \D \G \Z \R \F \Q \G \D)
  ;;(\H \C \M)
  ;;(\Q \R)]
  ;;
  ;;=> HNSNMTLHQ
  )



;; part 2 -------------------------------------------------

;;
;;


