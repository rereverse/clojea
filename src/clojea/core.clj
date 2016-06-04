(ns clojea.core
  (:require [clojure.core.async :as async]))

(defn select [coll]
  (let [n (count coll)
        triangle (take n (iterate inc 1))
        triangle-sum (double (* n (/ (+ n 1) 2)))
        coll-with-probs (map vector coll (map #(/ % triangle-sum) triangle))
        target (rand)]
    (reduce
      (fn [S [x p]]
        (let [s (+ S p)]
          (if (> s target) (reduced x) s)))
      0
      coll-with-probs)))

(def fitness-first
  (fn [x y]
    (let [fc (compare (:f x) (:f y))]
      (if (= 0 fc)
        (compare (:c x) (:c y))
        fc))))

(defn rand-boolean [probability-true]
  (>= (rand) (- 1.0 probability-true)))

(defn worthy? [individual population]
  (and
    (> (:f individual) (:f (first population)))
    (not (contains? population individual))))

(defn add [individual population]
  (-> population
      (disj (first population))
      (conj individual)))

(defn add-if-worhty [population individual]
  (if (worthy? individual population)
    (add individual population)
    population))

(defn variate [population variation fitness-function]
  (let [x (variation population)
        fx (fitness-function x)]
    (add-if-worhty population {:c x :f fx})))

(defn best [population]
  (last population))

(def gene-max-value 250)
(def chromosome-length 500)
(def population-size 20)
(def mutation-prob 0.05)
(def best-possible-solution (* gene-max-value chromosome-length))

(defn rand-gene [] (rand-int gene-max-value))
(defn rand-chromosome [] (into [] (repeatedly chromosome-length rand-gene)))
(defn rand-chromosomes [] (into [] (repeatedly population-size rand-chromosome)))
(defn fitness-function [chromosome] (apply + chromosome))
(defn mutation [population] (mapv #(if (rand-boolean mutation-prob) (rand-gene) %) (:c (select population))))
(defn crossover [population]
  (let [x (select population)
        y (first (filter (partial not= x) (repeatedly #(select population))))
        pivot (inc (rand-int (dec chromosome-length)))]
    (into [] (concat (take pivot (:c x)) (drop pivot (:c y))))))

(defn evolve [chromosomes fitness-f variations]
  (let [ff (memoize fitness-f)
        population (volatile! (apply (partial sorted-set-by fitness-first)
                                     (map #(hash-map :c %, :f (ff %)) chromosomes)))
        done (volatile! false)]

    (println "acceptable solution" (* 0.95 best-possible-solution))

    (async/go-loop [d @done]
      (when-not d
        (println (:f (best @population)))
        (Thread/sleep 3000)
        (recur @done)))

    (while (< (:f (best @population)) (* best-possible-solution 0.95))
      (doseq [v variations]
        (vswap! population variate v ff)))
    (vreset! done true)
    (println "after" (best @population))))



