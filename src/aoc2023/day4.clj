(ns aoc2023.day4
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn count-winners [line]
  (let [[left right] (str/split line #"\|")
        winners (set (map read-string (rest (re-seq #"\d+" left))))
        mine (set (map read-string (re-seq #"\d+" right)))]
    (count (set/intersection winners mine))))

(defn part1 [input]
  (->> (str/split-lines input)
       (map count-winners)
       (filter pos?)
       (map (comp int #(Math/pow 2 %) dec))
       (reduce +)))

(defn part2 [input]
  (let [data (map count-winners (str/split-lines input))
        size (count data)
        counts (vec (repeat size 1))]
    (->> (reduce (fn [[counts idx] winners]
                   (let [this-count (get counts idx)]
                     [(reduce (fn [counts idx] (update counts idx + this-count))
                              counts
                              (range (inc idx) (min (inc (+ idx winners)) size)))
                      (inc idx)]))
                 [counts 0]
                 data)
         first
         (reduce +))))

(assert (= 21088 (part1 (aoc/day 4))))
(assert (= 6874754 (part2 (aoc/day 4))))
