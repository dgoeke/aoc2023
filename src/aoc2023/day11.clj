(ns aoc2023.day11
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn empty-rows [lines]
  (set (->> (map-indexed #(vector %1 (str/index-of (str %2) \#)) lines)
            (filter (comp nil? second))
            (map first))))

(defn galaxy-locs [lines]
  (set (->> (map-indexed (fn [y line] (map-indexed (fn [x ch] [[y x] ch]) line)) lines)
            (apply concat)
            (filter #(= \# (second %)))
            (map first))))

(defn parse [input]
  (let [lines (str/split-lines input)]
    {:galaxies (galaxy-locs lines)
     :blank-rows (empty-rows lines)
     :blank-cols (empty-rows (apply map vector lines))}))

(defn distance [{:keys [blank-rows blank-cols]} expansion-factor [[y1 x1] [y2 x2]]]
  (let [xs (set (range (min x1 x2) (max x1 x2)))
        ys (set (range (min y1 y2) (max y1 y2)))
        extra-xs (count (set/intersection xs blank-cols))
        extra-ys (count (set/intersection ys blank-rows))]
    (+ (count xs) (count ys) (* extra-xs (dec expansion-factor)) (* extra-ys (dec expansion-factor)))))

(defn distances [expansion-factor {:keys [galaxies] :as input}]
  (->> (set (for [g1 galaxies g2 galaxies :when (not= g1 g2)] (sort [g1 g2])))
       (map (partial distance input expansion-factor))))

(defn solution [distance-mult input] (reduce + (distances distance-mult (parse input))))

(assert (= 9329143 (solution 2 (aoc/day 11))))
(assert (= 710674907809 (solution 1000000 (aoc/day 11))))   ; 550ms
