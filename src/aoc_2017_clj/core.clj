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

(defn p07a []
  (let [lines (map (fn [line]
                     (let [match (re-find #"(\w+) \((\d+)\)( -> (.*))?$" line)]
                       {:name (get match 1) :children (if (get match 4) (str/split (get match 4) #", ") nil)}))
                   (loop [lines []] (if-let [line (read-line)] (recur (conj lines line)) lines)))
        tree (reduce (fn [tree node] (assoc tree (:name node) (:children node))) {} lines)]
    (filter (fn [[k v]] (every? (fn [[k' v']] (every? #(not= % k) v')) tree)) tree)))

(defn tower-weight [tree name]
  (let [[self-weight children] (get tree name)]
    (+ self-weight (if children (reduce + (map #(tower-weight tree %) children)) 0))))

(defn p07b []
  (let [lines (map (fn [line]
                     (let [match (re-find #"(\w+) \((\d+)\)( -> (.*))?$" line)]
                       {:name (get match 1) :weight (->int (get match 2)) :children (if (get match 4) (str/split (get match 4) #", ") nil)}))
                   (loop [lines []] (if-let [line (read-line)] (recur (conj lines line)) lines)))
        tree (reduce (fn [tree node] (assoc tree (:name node) [(:weight node) (:children node)])) {} lines)]
    (doseq [suspect (filter (fn [[name [weight children]]]
                              (if children
                                (let [first-weight (tower-weight tree (first children))]
                                  (some #(not= first-weight (tower-weight tree %)) (rest children)))
                                false)) tree)]
      (let [[name [weight children]] suspect]
        (println name weight children)
        (println (map #(tower-weight tree %) children))))))

(defn p08a []
  (let [lines (map (fn [line] (let [match (re-find #"^(\w+) (inc|dec) (-?\d+) if (\w+) (.+) (-?\d+)$" line)]
                                {:target (get match 1) :op (get match 2) :amount (->int (get match 3)) :cond-reg (get match 4) :cond-op (get match 5) :cond-val (->int (get match 6))}))
                   (loop [lines []] (if-let [line (read-line)] (recur (conj lines line)) lines)))
        registers (reduce (fn [reg ins]
                            (if ((get {"==" #(= %1 %2)
                                       "!=" #(not= %1 %2)
                                       "<=" #(<= %1 %2)
                                       ">=" #(>= %1 %2)
                                       ">"  #(> %1 %2)
                                       "<"  #(< %1 %2)} (:cond-op ins)) (get reg (:cond-reg ins) 0) (:cond-val ins))
                              (assoc reg (:target ins) ((get {"inc" +
                                                              "dec" -} (:op ins)) (get reg (:target ins) 0) (:amount ins)))
                              reg)) {} lines)]
    (reduce (fn [max' [name value]] (max max' value)) 0 registers)))

(defn p08b []
  (let [lines (map (fn [line] (let [match (re-find #"^(\w+) (inc|dec) (-?\d+) if (\w+) (.+) (-?\d+)$" line)]
                                {:target (get match 1) :op (get match 2) :amount (->int (get match 3)) :cond-reg (get match 4) :cond-op (get match 5) :cond-val (->int (get match 6))}))
                   (loop [lines []] (if-let [line (read-line)] (recur (conj lines line)) lines)))
        [_ m] (reduce (fn [[reg m] ins]
                        (if ((get {"==" #(= %1 %2)
                                   "!=" #(not= %1 %2)
                                   "<=" #(<= %1 %2)
                                   ">=" #(>= %1 %2)
                                   ">"  #(> %1 %2)
                                   "<"  #(< %1 %2)} (:cond-op ins)) (get reg (:cond-reg ins) 0) (:cond-val ins))
                          (let [reg' (assoc reg (:target ins) ((get {"inc" +
                                                                     "dec" -} (:op ins)) (get reg (:target ins) 0) (:amount ins)))]
                            [reg' (reduce (fn [m [_ value]] (max m value)) m reg')])
                          [reg m])) [{} 0] lines)]
    m))

(defn p09a []
  (let [line (read-line)]
    (reduce (fn [[state sum n] char]
              (case state
                :normal (case char
                          \{ [:normal sum (inc n)]
                          \} [:normal (+ sum n) (dec n)]
                          \< [:garbage sum n]
                          [:normal sum n])
                :garbage (case char
                           \> [:normal sum n]
                           \! [:ignore sum n]
                           [:garbage sum n])
                :ignore [:garbage sum n])) [:normal 0 0] line)))

(defn p09b []
  (let [line (read-line)]
    (reduce (fn [[state sum] char]
              (case state
                :normal (case char
                          \< [:garbage sum]
                          [:normal sum])
                :garbage (case char
                           \> [:normal sum]
                           \! [:ignore sum]
                           [:garbage (inc sum)])
                :ignore [:garbage sum])) [:normal 0] line)))

(defn -main
  [& args]
  (println "Hello, World!")
  (println (p09b)))
