(ns aoc2023.day8
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(defn step [nodemap cur-node dir]
  (get-in nodemap [cur-node dir]))

(defn parse [input]
  (let [[path nodes] (str/split input #"\n\n")
        nodemap (->> (str/split-lines nodes)
                     (map #(->> (re-seq #"(\w+)" %)
                                (mapv first)
                                ((fn [[a b c]] [a {\L b \R c}]))))
                     (into {}))]
    [path nodemap]))

(defn walk [nodemap path start-node end-node?]
  (count (take-while (comp not end-node?) (reductions (partial step nodemap) start-node (cycle path)))))

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))
(defn lcm [v] (reduce #(/ (* %1 %2) (gcd %1 %2)) v))

(defn part1 [path nodemap] (walk nodemap path "AAA" #(= % "ZZZ")))

(defn part2 [path nodemap]
  (let [nodes (->> (keys nodemap) (filter #(str/ends-with? % "A")))]
    (lcm (map #(walk nodemap path % (fn [n] (str/ends-with? n "Z"))) nodes))))

(let [[path nodemap] (parse (aoc/day 8))]
  (assert (= 19099 (part1 path nodemap)))
  (assert (= 17099847107071 (part2 path nodemap))))
