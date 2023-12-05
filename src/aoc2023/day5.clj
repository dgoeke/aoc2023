(ns aoc2023.day5
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(defn functionify [otherwise-fn [dst src size]]
  (fn [x]
    (if (and (<= src x) (< x (+ src size)))
      (+ dst (- x src))
      (otherwise-fn x))))

(defn parse-map [m]
  (let [[name-line & items] (str/split m #"\n")
        name (keyword (subs name-line 0 (str/index-of name-line " ")))
        f (->> items
               (map #(->> % (re-seq #"(\d+)") (map first) (map read-string)))
               (reduce functionify identity))]
    [name f]))

(defn merge-maps [{:keys [seed-to-soil soil-to-fertilizer fertilizer-to-water water-to-light
                          light-to-temperature temperature-to-humidity humidity-to-location]}]
  (comp humidity-to-location temperature-to-humidity light-to-temperature
        water-to-light fertilizer-to-water soil-to-fertilizer seed-to-soil))

(defn part1 [input]
  (let [[seeds-line & maps] (str/split input #"\n\n")
        seeds (map (comp read-string first) (re-seq #"(\d+)" seeds-line))
        maps (into {} (map parse-map maps))
        converter (merge-maps maps)]
    (reduce min (map converter seeds))))

(assert (= 525792406 (part1 (aoc/day 5))))
