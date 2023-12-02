(ns aoc2023.day2
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(defn parse-line [input]
  (let [[game-str all-blocks] (str/split input #": ")
        [_ game-id] (str/split game-str #" ")
        games (map parse-blocks (str/split all-blocks #";"))]
    [(read-string game-id) games]))

(defn parse-blocks [block-list]
  (let [[_ r-str] (re-find #"(\d+) red" block-list)
        [_ g-str] (re-find #"(\d+) green" block-list)
        [_ b-str] (re-find #"(\d+) blue" block-list)]
    (map (fnil read-string "0") [r-str g-str b-str])))

(defn valid-game? [[r g b]] (and (<= r 12) (<= g 13) (<= b 14)))

(defn valid-game-list? [[_ games]] (every? valid-game? games))

(defn part1 [input]
  (->> (str/split-lines input)
       (map parse-line)
       (filter valid-game-list?)
       (map first)
       (reduce +)))

(defn smallest-necessary [[game-id games]]
  (let [groups (apply mapv vector (map vec games))]
    (map (partial apply max) groups)))

(defn part2 [input]
  (->> (str/split-lines input)
       (map #(->> (parse-line %)
                  smallest-necessary
                  (reduce *)))
       (reduce +)))

(assert (= 2283 (part1 (aoc/day 2))))
(assert (= 78669 (part2 (aoc/day 2))))
