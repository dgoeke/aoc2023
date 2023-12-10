(ns aoc2023.day10
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(defn neighbors [y x height width]
  (remove nil? [(when (> y 0) [(dec y) x])
                (when (> x 0) [y (dec x)])
                (when (< x width) [y (inc x)])
                (when (< y height) [(inc y) x])]))

(defn connected-to [ch [y x]]
  (condp = ch
    \F #{[(inc y) x] [y (inc x)]}
    \7 #{[y (dec x)] [(inc y) x]}
    \L #{[(dec y) x] [y (inc x)]}
    \J #{[y (dec x)] [(dec y) x]}
    \| #{[(dec y) x] [(inc y) x]}
    \- #{[y (dec x)] [y (inc x)]}
    \S #{[y (dec x)] [y (inc x)] [(dec y) x] [(inc y) x]}
    #{}))

(defn connected? [input dst src]
  (let [src-conns (connected-to (get-in input src) src)
        dst-conns (connected-to (get-in input dst) dst)]
    (and (src-conns dst) (dst-conns src))))

(defn connections [input [y x]]
  (set (filter (partial connected? input [y x]) (neighbors y x (count input) (count (first input))))))

(defn step [input [current-pos last-pos]]
  [(first (disj (connections input current-pos) last-pos)) current-pos])

(defn loop-path [input]
  (let [start-pos (first (filter second (map-indexed #(vector %1 (str/index-of %2 \S)) input)))]
    (into [start-pos] (map first (take-while #(not= (first %) start-pos) (rest (iterate (partial step input) [start-pos nil])))))))

(defn count-interior-line [y line path]
  (first (reduce (fn [[result n-seen] x]
                   (cond
                     (and (#{\| \J \L} (get line x)) (path [y x])) [result (inc n-seen)]
                     (and (odd? n-seen) (not (path [y x])))        [(inc result) n-seen]
                     :else                                         [result n-seen]))
                 [0 0] (range (count line)))))

(defn part1 [input] (/ (count (loop-path input)) 2))

(defn part2 [input]
  (let [path (set (loop-path input))]
    (reduce + (map-indexed #(count-interior-line %1 %2 path) input))))

(let [input (str/split-lines (aoc/day 10))]
  (assert (= 6979 (part1 input)))
  (assert (= 443 (part2 input))))
