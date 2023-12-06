(ns aoc2023.day6
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

; holding the button for `x` ms, distance = x * (time-x) = Tx - x^2.
; so for time=7, distance=9, we solve the equation 7x - x^2 = 9 to find
; the intercepts where we cross the "distance=9" line. the difference
; between those two x values (in integers) is the number of of different
; times the button can be held down to win. use the quadratic equation.
(defn check-timing [[t d]]
  (->> [(int (Math/ceil (/ (- t (Math/sqrt (- (* t t) (* 4 d)))) 2)))
        (int (Math/ceil (/ (+ t (Math/sqrt (- (* t t) (* 4 d)))) 2)))]
       sort reverse (apply -)))

(defn part1 [input]
  (->> (str/split-lines input)
       (map #(map read-string (re-seq #"\d+" %)))
       (apply map vector)
       (map check-timing)
       (apply *)))

(defn part2 [input]
  (->> (str/split-lines input)
       (map #(->> (re-seq #"\d+" %)
                  (apply concat)
                  (apply str)
                  read-string))
       check-timing))

(assert (= 393120 (part1 (aoc/day 6))))
(assert (= 36872656 (part2 (aoc/day 6))))
