(ns day02)

(require '[clojure.string :as str] '[clojure.core.reducers :as r])


(defn repeated-twice?
  [n]
  (def n-str (str n))
  (def size (count n-str))
  (and
    (= 0 (mod size 2))
    (let [fst (subs n-str 0 (/ size 2)) snd (subs n-str (/ size 2))]
      (= fst snd))))


;; test if a number x is a repetition of size n
(defn repeated-nth?
  [n x]
  (let [x-str (str x) size (count x-str)]
    (and
      (= 0 (mod size n))
      (let [parts (map str/join (partition n x-str))]
        (and (> (count parts) 1) (apply = parts))))))


;; test if a number is a repeated prefix, i.e. 121212 or 44444444 or 12345123451234512345 ...
(defn repeated-prefix?
  ([x] (repeated-prefix? x 1))
  ([x n]
   (let [x-str (str x) size (count x-str)]
     (if (<= n (/ size 2))
       (or
         (let [repeated (repeated-nth? n x)]
           (if repeated
             ())
           repeated)
         (recur x (inc n)))
       false))))


;; take "11-22" and return (11, 12, ..., 22)
(defn to-range
  [id-range]
  (let [[lhs rhs] (map Long/parseLong (str/split id-range #"-" 2))]
    (into [] (range lhs (inc rhs)))))


(defn sum-invalid-ids
  [ranges pred?]
  (reduce + (mapv (fn [r]
                    (reduce + (filterv pred? (to-range r)))) ranges)))


(defn -main
  [& args]
  (def content (slurp (first args)))
  (def ranges (str/split content #"[,\n]"))
  (def score1 (sum-invalid-ids ranges repeated-twice?))
  (def score2 (sum-invalid-ids ranges repeated-prefix?))
  (println "Part 1: " score1)
  (println "Part 2: " score2))
