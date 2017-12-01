(ns aoc-2017-clj.core
  (:gen-class))

(defn ->int [c] (- (int c) 48))

(defn p01a []
  (let [input (read-line)]
    (reduce + (map #(if (= %1 %2) (->int %1) 0) input (drop 1 (cycle input))))))

(defn p01b []
  (let [input (read-line)]
    (reduce + (map #(if (= %1 %2) (->int %1) 0) input (drop (/ (count input) 2) (cycle input))))))

(defn -main
  [& args]
  (println "Hello, World!"))
