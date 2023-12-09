(ns aoc2023.day9
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(defn diffs [nums] (map #(- %2 %1) nums (rest nums)))

(defn triangle [nums] (take-while (comp not (partial every? zero?))
                                  (iterate diffs nums)))

(defn part1 [nums] (->> (triangle nums) (map last) (reduce +)))

(defn part2 [nums] (->> (triangle nums) (map first) reverse (reduce #(- %2 %1) 0)))

(defn solution [f input]
  (->> (str/split-lines input)
       (map #(f (->> (re-seq #"([-\d]+)" %)
                     (map (comp read-string first)))))
       (reduce +)))

(assert (= 1898776583 (solution part1 (aoc/day 9))))
(assert (= 1100 (solution part2 (aoc/day 9))))
