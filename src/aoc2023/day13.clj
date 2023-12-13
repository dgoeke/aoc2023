(ns aoc2023.day13
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(defn count-diffs [a b] (count (filter false? (map = a b))))  ; count characters different between 2 lines

(defn check-full-vert-reflect [pred lines line-num]
  (pred (let [p1 (reverse (subvec lines 0 (inc line-num)))  ; split the list of lines at a potential mirror point
              p2 (subvec lines (inc line-num))]             ; reverse the first half
          (reduce + (map count-diffs p1 p2)))))             ; sum # different characters between halves (stop when small half finished)

(defn horiz-reflect-line [pred lines]
  (first (->> (partition 2 1 lines)                                         ; make pairs of lines
              (map-indexed (fn [i [l1 l2]] [i (> 2 (count-diffs l1 l2))]))  ; map list to [index of pair, bool(#diffs <= 1)]
              (filter second)                                               ; filter down to only pairs with 0 or 1 diff
              (map first)                                                   ; select the line-pairs
              (filter (partial check-full-vert-reflect pred lines)))))      ; check each pair for a full reflection

(defn summarize [pred lines]
  (let [horiz-line (horiz-reflect-line pred lines)                       ; check for a reflection between horizontal lines
        vert-line (horiz-reflect-line pred (apply mapv vector lines))]   ; transpose matrix and do the same (now it's a vertical check)
    (or (some-> vert-line inc)
        (some-> horiz-line inc (* 100)))))

(defn solution [pred input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map (partial summarize pred))
       (reduce +)))

(assert (= 34100 (solution zero?    (aoc/day 13)))) ; part 1 splits have 0 different characters
(assert (= 33106 (solution #(= 1 %) (aoc/day 13)))) ; part 2 splits have 1 different character (15ms)
