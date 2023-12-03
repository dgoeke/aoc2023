(ns aoc2023.day3
  (:require [aoc2023.core :as aoc]
            [clojure.string :as str]))

(def numeric? (comp some? (set "0123456789")))

(defn x-pos [index width] (mod index width))
(defn y-pos [index width] (quot index width))

(defn find-locations [[x num-start accum] ch]
  (let [accum (if (and num-start (not (numeric? ch)))
                (conj accum [num-start x])
                accum)]
    (cond
      (and (numeric? ch) num-start) [(inc x) num-start accum]
      (numeric? ch) [(inc x) x accum]
      (not= \. ch) [(inc x) nil (conj accum x)]
      :else [(inc x) nil accum])))

(defn adjacent-to [numbers y x width height]
  (->> [(when (and (> y 0) (> x 0)) (get numbers [(dec y) (dec x)]))
        (when (> y 0) (get numbers [(dec y) x]))
        (when (and (> y 0) (< x width)) (get numbers [(dec y) (inc x)]))
        (when (> x 0) (get numbers [y (dec x)]))
        (when (< x width) (get numbers [y (inc x)]))
        (when (and (< y height) (> x 0)) (get numbers [(inc y) (dec x)]))
        (when (< y height) (get numbers [(inc y) x]))
        (when (and (< y height) (< x width)) (get numbers [(inc y) (inc x)]))]
       (remove nil?)
       (into #{})))

(defn parse-line [line locations width height]
  (let [{:keys [numbers symbols]}
        (reduce (fn [result index]
                  (if (number? index)
                    (update result :symbols assoc [(y-pos index width) (x-pos index width)]
                            {:symbol (get line index) :adjacent nil})
                    (let [[start end] index
                          n (read-string (subs line start end))]
                      (reduce (fn [result index]
                                (update result :numbers assoc [(y-pos index width) (x-pos index width)] n))
                              result
                              (range start end)))))
                {}
                locations)]
    (into {} (map (fn [[[y x] sym]]
                    [[y x] (assoc sym :adjacent (adjacent-to numbers y x width height))])
                  symbols))))

(defn part1 [data]
  (->> (vals data)      ; look at all symbols
       (map :adjacent)  ; consider the numbers they're adjacent to
       (apply concat)   ; make those numbers one big list
       (reduce +)))     ; add up the list

(defn part2 [data]
  (->> data
       (filter (fn [[_ v]] (= \* (:symbol v)))) ; look at all '*'s
       (map (comp :adjacent second))            ; consider the numbers they're adjacent to
       (filter #(= 2 (count %)))                ; find gears adjacent to exactly 2 numbers
       (map (partial apply *))                  ; multiply the two numbers for each gear
       (reduce +)))                             ; sum the results

(let [input (aoc/day 3)
      width (str/index-of input "\n")
      height (dec (quot (count input) width))
      line (str/replace input "\n" "")
      [_ _ locations] (reduce find-locations [0 nil []] line)
      data (parse-line line locations width height)]
  (assert (= 507214 (part1 data)))
  (assert (= 72553319 (part2 data))))
