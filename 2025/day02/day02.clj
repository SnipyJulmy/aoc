(ns day02)

(require '[clojure.string :as str] '[clojure.core.reducers :as r])


;; take "11-22" and return (11, 12, ..., 22)
(defn to-range
  [id-range]
  (let [[lhs rhs] (map Long/parseLong (str/split id-range #"-" 2))]
    (into [] (range lhs (inc rhs)))))


(defn sum-invalid-ids
  [ranges pred?]
  (reduce + (mapv (fn [r]
                    (reduce + (filterv pred? (to-range r)))) ranges)))


(defn repeated-twice?
  [x]
  (some? (re-matches #"^(\d+)\1$" (str x))))


(defn repeated-number?
  [x]
  (some? (re-matches #"^(\d+)\1+$" (str x))))


(defn -main
  [& args]
  (def content (slurp (first args)))
  (def ranges (str/split content #"[,\n]"))
  (def score1 (sum-invalid-ids ranges repeated-twice?))
  (def score2 (sum-invalid-ids ranges repeated-number?))
  (println "Part 1: " score1)
  (println "Part 2: " score2))
