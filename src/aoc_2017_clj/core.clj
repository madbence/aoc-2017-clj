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

(defn p04a []
  (loop [valid 0]
    (if-let [line (read-line)]
      (let [words (str/split line #" ")
            invalid (some (fn [[a b]] (= (nth words a) (nth words b))) (for [a (range 0 (count words)) b (range 0 (count words)) :when (not= a b)] [a b]))]
        (println words invalid)
        (recur (+ valid (if invalid 0 1))))
      valid)))

(defn anagram? [a b]
  (and (every? (fn [c] (some (fn [c'] (= c c')) b)) a) (= (count a) (count b))))

(defn p04b []
  (loop [valid 0]
    (if-let [line (read-line)]
      (let [words (str/split line #" ")
            invalid (some (fn [[a b]] (anagram? (nth words a) (nth words b))) (for [a (range 0 (count words)) b (range 0 (count words)) :when (not= a b)] [a b]))]
        (recur (+ valid (if invalid 0 1))))
      valid)))

(defn p05a []
  (loop [instructions (loop [i []] (if-let [line (read-line)] (recur (conj i (->int line))) i))
         pc 0
         n 0]
    (if (or (neg? pc) (>= pc (count instructions))) n
      (recur
        (assoc instructions pc (inc (get instructions pc)))
        (+ pc (get instructions pc))
        (inc n)))))

(defn p05b []
  (loop [instructions (loop [i []] (if-let [line (read-line)] (recur (conj i (->int line))) i))
         pc 0
         n 0]
    (let [offset (get instructions pc)]
      (if (or (neg? pc) (>= pc (count instructions))) n
        (recur
          (assoc instructions pc (if (> offset 2) (dec offset) (inc offset)))
          (+ pc offset)
          (inc n))))))

(defn p06a []
  (loop [banks (vec (map ->int (str/split (read-line) #"\t")))
         history #{}
         n 0]
    (if (contains? history banks) n
      (let [index (map (fn [n i] [n i]) banks (iterate inc 0))
            offset (reduce (fn [offset [n i]] (if (> n (get banks offset)) i offset)) 0 index)]
        (recur (loop [remaining (get banks offset)
                      banks' (assoc banks offset 0)
                      offset' (rem (inc offset) (count banks))]
                 (if (zero? remaining) banks'
                   (recur
                     (dec remaining)
                     (assoc banks' offset' (inc (get banks' offset')))
                     (rem (inc offset') (count banks)))))
               (conj history banks)
               (inc n))))))

(defn p06b []
  (loop [banks (vec (map ->int (str/split (read-line) #"\t")))
         history {}
         n 0]
    (if (some? (get history banks)) (- n (get history banks))
      (let [index (map (fn [n i] [n i]) banks (iterate inc 0))
            offset (reduce (fn [offset [n i]] (if (> n (get banks offset)) i offset)) 0 index)]
        (recur (loop [remaining (get banks offset)
                      banks' (assoc banks offset 0)
                      offset' (rem (inc offset) (count banks))]
                 (if (zero? remaining) banks'
                   (recur
                     (dec remaining)
                     (assoc banks' offset' (inc (get banks' offset')))
                     (rem (inc offset') (count banks)))))
               (assoc history banks n)
               (inc n))))))

(defn -main
  [& args]
  (println "Hello, World!")
  (println (p05b)))
