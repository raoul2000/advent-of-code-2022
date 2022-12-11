(ns day-7
  (:require [clojure.string :refer [split-lines split]]
            [clojure.walk :refer [walk]]))

;; https://adventofcode.com/2022/day/7

;; The filesystem consists of a tree of files (plain data) and directories 
;; (which can contain other directories or files)
;; you need to determine the total size of each directory
;;

(def sample "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
")

(defn add-file-to-cur-dir [acc file-v]
  (update-in acc [:cur 1] conj file-v))

(comment

  (add-file-to-cur-dir {:cur ["/" []]
                        :fs []}
                       [3333 "new.xml"])
  (add-file-to-cur-dir {:cur ["/path" [[1125 "file.txt"]]]
                        :fs []}
                       [3333 "new.xml"])
  ;;
  )

(defn command? [s]
  (.startsWith s "$"))

(comment
  (command? "")
  (command? "$ cd"))

(defn cd-path 
  "Given an absolute *path* and the arg of a cd command, returns
   the modified *path* value."
  [path cd-arg]
  (case cd-arg
    "/"     "/"
    ".."    (let [result (->> (split path #"/")
                              (butlast)
                              (interpose "/")
                              (apply str))]
              (if (= "" result)
                "/"
                result))
    (str (when-not (= "/" path) path) "/" cd-arg)))

(defn update-path-info [path-info-coll [path ls]]
  (println path ls)
  (if (some #{path} (map first path-info-coll))
    (mapv (fn [[p l]]
            (if (= p path)
              (vector p ls)
              (vector p l))) path-info-coll)
    (conj path-info-coll [path ls])))

(comment
  (update-path-info [["a" [:a :b :c]]] ["b" [1 2 3]])
  (update-path-info [["a" [:a :b :c]]] ["a" [1 2 3]])
  ;;
  )

(defn find-path-info [path-info-coll path]
  (first (filter #(= path (first %))  path-info-coll)))

(comment
  (find-path-info [["a" [:a :b :c]]] "a")
  (find-path-info [["a" [:a :b :c]]] "z")
  ;;
  )

(defn evaluate-cd [arg acc]
  (-> acc
      (update ,,, :fs update-path-info (:cur acc))
      (update ,,, :cur (fn [[path _]]
                         (let [new-path (cd-path path arg)]
                           (or (find-path-info (:fs acc) new-path)
                               [new-path []]))))))


(defn evaluate-ls [_ acc]
  (update acc :cur (fn [[path _]] (vector path []))))

(defn evaluate-cmd [acc cmd]
  (let [[_ op arg] (split cmd #" ")]
    (case op
      "cd" (evaluate-cd arg acc)
      "ls" (evaluate-ls arg acc)
      (do
        (println "command not supported")
        acc))))

(comment

  (->> (split-lines sample)

       (map #(split % #" ")))
  (re-matches #"\$ (.+) (.+)" "$ cd e")
  (.startsWith "yddd" "y")

  ;;
  )

(defn evaluate
  "Given a coll of lines, return a vector describing folders where the first
   item is the forlder name, and the second item is a vector of files or sub-folders
   directly in this folder"
  [lines-v]
  (->> (reduce (fn [acc line]
                 (if (command? line)
                   (evaluate-cmd acc line)
                   (add-file-to-cur-dir acc line)))
               {:cur []
                :fs []} lines-v)
       (evaluate-cd "/")
       :fs
       (remove #{[nil nil]})))

(comment
  (evaluate (split-lines sample))
  ;;
  )


(defn descendants [path folders]
  (->> folders
       (filter (fn [[p _]]
                 (or  (= p path)
                      (.startsWith p path))))
       (map second)
       (into [])
       (flatten)
       (vector path)))

(comment
  (descendants "/a/e" (evaluate (split-lines sample)))
  ;;
  )

(defn dir? [f]
  (.startsWith f "dir"))

(defn file-size [file-info]
  (Integer/parseInt (first (split file-info #" ")) 10))

(comment
  (Integer/parseInt (first (split "2265 d.e" #" ")) 10)
  (remove dir? '("dir e" "29116 f" "2557 g" "62596 h.lst" "584 i"))
  (map file-size (remove dir? '("dir e" "29116 f" "2557 g" "62596 h.lst" "584 i")))
  ;;
  )

(defn solution-1 []
  (let [eval-result (->> ;;sample
                     (slurp "./resources/puzzle_7.txt")
                     (split-lines)
                     (evaluate))]
    (->> eval-result
         (map first)                             ;; each path
         (map #(descendants % eval-result))      ;; deep content
         (map (fn [[path  info]]                 ;; total folder size
                (vector path (->> info
                                  (remove dir?)
                                  (map file-size)
                                  (apply +)))))
         (filter (fn [[_ total-size]]
                   (< total-size 100000)))       ;; keep some
         (map second)
         (apply +)                               ;; sum all
         )))


(comment
  (solution-1)
  ;; => 1306611 !!
  )



;; part 2 -------------------------------------------------

;;
;;

(defn solution-2 [])
