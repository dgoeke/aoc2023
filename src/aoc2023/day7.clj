(ns aoc2023.day7
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(def card-values-p1 {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14 nil 0})
(def card-values-p2 (assoc card-values-p1 \J 1))

(defn parse-line [line]
  (let [[cards bid] (str/split line #" ")]
    {:cards cards :jokers? false :all-cards cards :num-jokers 0 :bid (Integer/parseInt bid)}))

(defn get-hand-type [{:keys [cards jokers? num-jokers all-cards bid]}]
  (let [[[_ c1] [_ c2]] (reverse (sort-by second (frequencies cards)))
        c1 (+ (or c1 0) num-jokers)
        hand-type (cond
                    (= 1 c1)                 0   ; high card
                    (and (= 2 c1) (= 1 c2))  1   ; pair
                    (and (= 2 c1) (= 2 c2))  2   ; two pair
                    (and (= 3 c1) (= 1 c2))  3   ; three of a kind
                    (and (= 3 c1) (= 2 c2))  4   ; full house
                    :else (inc c1))]             ; four or five of a kind
    [(into [hand-type] (mapv (if jokers? card-values-p2 card-values-p1) all-cards)) bid]))

(defn extract-jokers [{:keys [cards bid]}]
  (let [{non-jokers false jokers true} (group-by (comp some? #{\J}) cards)]
    {:cards (apply str non-jokers) :num-jokers (count jokers) :all-cards cards :jokers? true :bid bid}))

(defn results [parse-fn input]
  (->> (str/split-lines input)
       (map (comp get-hand-type parse-fn))
       (sort-by first)
       (map-indexed (fn [idx [_ bid]] (* (inc idx) bid)))
       (reduce +)))

(def part1 (partial results parse-line))
(def part2 (partial results (comp extract-jokers parse-line)))

(assert (= 250120186 (part1 (aoc/day 7))))
(assert (= 250665248 (part2 (aoc/day 7))))
