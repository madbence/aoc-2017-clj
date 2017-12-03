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

(defn next-cell [{:keys [x y n k v dir]}]
  (let [next-dir (if (= n 0) (dir {:r :u :u :l :l :d :d :r}) dir)
        next-x (+ x (dir {:r 1 :u 0 :l -1 :d  0}))
        next-y (+ y (dir {:r 0 :u 1 :l  0 :d -1}))
        next-k (if (and (= n 0) (or (= next-dir :l) (= next-dir :r))) (inc k) k)]
    {:v (inc v)
     :x next-x
     :y next-y
     :dir next-dir
     :n (if (= n 0) next-k (dec n))
     :k next-k}))

(defn p03a [n]
  (let [cell (nth (iterate next-cell {:v 1 :x 0 :y 0 :n 0 :k 0 :dir :r}) (dec n))]
    (+ (Math/abs (:x cell)) (Math/abs (:y cell)))))

(defn neighbour-sum [board cell]
  (reduce + (for [x [-1 0 1] y [-1 0 1]]
              (let [x (+ x (:x cell))
                    y (+ y (:y cell))]
                (if (contains? board [x y])
                  (get board [x y])
                  0)))))

(defn p03b [n]
  (loop [board {[0 0] 1}
         cell {:v 1 :x 0 :y 0 :n 0 :k 0 :dir :r}]
    (let [next-c (next-cell cell)
          sum (neighbour-sum board next-c)]
      (if (> sum n) sum
        (recur (assoc board [(:x next-c) (:y next-c)] sum) next-c)))))

(defn -main
  [& args]
  (println "Hello, World!"))
