(ns aoc2023.day12
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(defn parse [input]
  (map #(let [[p1 p2] (str/split % #" ")]
          [p1 (mapv read-string (str/split p2 #","))])
       (str/split-lines input)))

(defn expand [[row record]]
  [(str/join "?" (repeat 5 row)) (->> (repeat 5 record) flatten vec)])

(declare dp) ; mutually recursive fun ahead

(def advance
  (memoize
   (fn [row record i j]
     (let [n (get record j)]
       (cond
         (>= j (count record))       0
         (< (- (count row) i) n)     0
         (some (comp not #{\# \?})
               (subs row i (+ i n))) 0
         (= (- (count row) i) n)     (dp row record (count row) (inc j))
         (not (#{\. \?}
               (get row (+ i n))))   0
         :else                       (dp row record (inc (+ i n)) (inc j)))))))

(def dp
  (memoize
   (fn [row record i j]
     (cond
       (>= i (count row)) (if (>= j (count record)) 1 0)
       (= \. (get row i)) (recur row record (inc i) j)
       (= \# (get row i)) (advance row record i j)
       :else              (+ (advance row record i j) (dp row record (inc i) j))))))

(defn solution [f input]
  (->> (parse input)
       (map (comp (fn [[row rec]] (dp row rec 0 0)) f))
       (reduce +)))

(assert (= 7169          (solution identity (aoc/day 12))))  ; part 1
(assert (= 1738259948652 (solution expand   (aoc/day 12))))  ; part 2 (10ms)
