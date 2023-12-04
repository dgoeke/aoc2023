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
       (map count-winners)                   ; count the number of winners for each card
       (filter pos?)                         ; ignore cards without winners
       (map (comp int #(Math/pow 2 %) dec))  ; score for each card is 2^(winners-1)
       (reduce +)))                          ; add up the scores

(defn part2 [input]
  (let [data (map count-winners (str/split-lines input))  ; 'data' seqence holds the number of winners for each card
        size (count data)
        counts (vec (repeat size 1))]        ; 'counts' vector tracks how many of each card we have, starting as [1 1 1 1 1 1 ...]
    (->> (reduce (fn [[counts idx] winners]  ; outer reduce: for each card in order, update the counts for those immediately after it in the list
                   (let [this-count (get counts idx)]
                     [(reduce (fn [counts idx] (update counts idx + this-count))  ; inner reduce: if this card is index 25 with 4 winners, update
                              counts                                              ; the range [26 27 28 29]. increment those counts to add the
                              (range (inc idx) (min (inc (+ idx winners)) size))) ; number of copies of card 25 that we have.
                      (inc idx)]))
                 [counts 0]
                 data)
         first
         (reduce +))))

(assert (= 21088 (part1 (aoc/day 4))))
(assert (= 6874754 (part2 (aoc/day 4))))
