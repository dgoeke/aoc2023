(ns aoc2023.day1
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(def nums
  (map-indexed vector ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]))

(defn convert-digits [[fst & rst :as s]]
  (if-not rst
    (str fst)
    (let [s (apply str s)
          [n n-str] (first (filter #(str/starts-with? s (second %)) nums))]
      (concat (str (if n-str n fst)) (convert-digits rst)))))

(defn solution [converter input]
  (->> (str/split-lines input)
       (map converter)
       (map (partial filter (set "0123456789")))
       (map (partial (juxt first last)))
       (map (partial apply str))
       (map read-string)
       (reduce +)))

(def part1 (partial solution identity))
(def part2 (partial solution convert-digits))

(assert (= 54159 (part1 (aoc/day 1))))
(assert (= 53866 (part2 (aoc/day 1))))
