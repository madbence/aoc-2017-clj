(ns aoc-2017-clj.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn ->int [c]
  (cond
    (string? c) (Integer/parseInt c)
    :else (- (int c) 48)))

(defn p01a []
  (let [input (read-line)]
    (reduce + (map #(if (= %1 %2) (->int %1) 0) input (drop 1 (cycle input))))))

(defn p01b []
  (let [input (read-line)]
    (reduce + (map #(if (= %1 %2) (->int %1) 0) input (drop (/ (count input) 2) (cycle input))))))

(defn p02a []
  (loop [line (read-line)
         sum 0]
    (if line
      (recur (read-line) (+ sum (let [result (reduce (fn [acc val] {:min (min val (:min acc)) :max (max val (:max acc))}) {:min 10000 :max 0} (map ->int (str/split line #"\t")))]
                                  (- (:max result) (:min result)))))
      sum)))

(defn p02b []
  (loop [sum 0]
    (if-let [line (read-line)]
      (let [nums (map ->int (str/split line #"\t"))]
        (recur (+ sum (first (for [a nums b nums :when (and (= 0 (rem a b)) (not= a b))] (quot a b))))))
      sum)))

(defn -main
  [& args]
  (println "Hello, World!"))
