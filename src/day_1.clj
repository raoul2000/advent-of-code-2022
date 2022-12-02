(ns day-1
  (:require [clojure.string :refer [split-lines blank?]]))

;; https://adventofcode.com/2022/day/1

(comment
  (println "let's start !! "))

(defn str-coll->int-coll [coll]
  (map #(Integer/parseInt %) coll))

(defn solution []
  (->> (slurp "./resources/puzzle_1.txt")
       split-lines
       (partition-by blank?)
       (remove #(= "" (first %)))
       (map str-coll->int-coll)
       (map #(apply + %))
       (apply max)))

;; => 68442 !!
