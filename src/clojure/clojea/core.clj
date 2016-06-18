(ns clojea.core
  (:use [clojea.random.mtrandom]))

(defn- rand-gene [max]
  (tlmt-rand max))

(defn rand-chrom [alleles]
  (mapv rand-gene alleles))

(defn- rand-chroms [n alleles]
  (repeatedly n (partial rand-chrom alleles)))

(defn- eval-chroms [ff chroms]
  (->> chroms
       (pmap #(hash-map :chrom % :fness (ff %)))
       (sort-by :fness >)))

(defn- normalize [coll]
  (let [sum (double (apply + coll))]
    (map #(/ % sum) coll)))

(defn- cumul-probs [n]
  (map #(/ 1 %) (range 1 (inc n))))

(defn- unite [pop-size & pops]
  (loop [ps (vec pops) r []]
    (if (> pop-size (count r))
      (let [[m i] (->> (map #(vector (first %1) %2) ps (range))
                       (filter (comp some? first))
                       (reduce (fn [[m _ :as max] [x _ :as c]]
                                 (if (> (:fness x) (:fness m)) c max))))]
        (recur (update ps i rest) (conj r m)))
      r)))

(defn- roulette-wheel [coll probs]
  (let [coll-probs (map vector coll probs)
        target (tlmt-rand)]
    (reduce (fn [S [x p]]
              (let [s (+ S p)]
                (if (> s target) (reduced x) s)))
            0 coll-probs)))

(defn- rand-pivot [n]
  (inc (tlmt-rand (dec n))))

(defn- crossover [x y pivot]
  [(vec (concat (take pivot x) (drop pivot y)))
   (vec (concat (take pivot y) (drop pivot x)))])

(defn- mutate [chrom alleles mutation-p]
  (mapv (fn [g a]
          (if (tlmt-rand-boolean mutation-p)
            (->> (repeatedly #(rand-gene a))
                 (filter #(not= g %))
                 (first))
            g))
        chrom alleles))

(def ^:private default-conf
  {:alleles     []
   :pop-size    20
   :ff          identity
   :max-iter    -1
   :acceptable? (fn [_] false)
   })

(defn evolve
  ":pop-size (20)
  :alleles ([])
  :ff (identity)
  :max-iter    -1
  :acceptable? (fn [best-fitness] false)

  Returns [iter (first pop)]"
  [conf]
  (let [conf (merge default-conf conf)
        pop-size (:pop-size conf)
        alleles (:alleles conf)
        ff (:ff conf)
        acceptable? (:acceptable? conf)
        max-iter (:max-iter conf)
        selection-ps (normalize (cumul-probs pop-size))
        mutation-p 0.01
        eval (partial eval-chroms ff)]
    (loop [pop (eval (rand-chroms pop-size alleles)) i 0]
      (if (and (> max-iter i) (not (acceptable? (:fness (first pop)))))
        (recur
          (apply
            (partial unite pop-size)
            (map
              (partial take (/ pop-size 2))
              [pop
               (eval
                 (->> (repeatedly (partial roulette-wheel pop selection-ps))
                      (map :chrom)
                      (partition 2)
                      (mapcat #(crossover (first %) (second %) (rand-pivot (count alleles))))
                      (map #(mutate % alleles mutation-p))
                      (take pop-size)))]))
          (inc i))
        [i (first pop)]))))
