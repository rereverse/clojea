(ns clojea.adaptive
  (:use [clojea.random.mtrandom]))

(defn rand-gene [max]
  (inc (tlmt-rand max)))

(defn rand-chrom [alleles]
  (mapv rand-gene alleles))

(defn rand-chroms [n alleles]
  (repeatedly n (partial rand-chrom alleles)))

(defn eval-chroms [ff chroms]
  (->> chroms
       (map #(hash-map :chrom % :fness (ff %)))
       (sort-by :fness >)))

(defn normalize [coll]
  (let [sum (double (apply + coll))]
    (map #(/ % sum) coll)))

(defn cumul-probs [n]
  (map #(/ 1 %) (take n (iterate inc 1))))

(defn unite [pop-size & pops]
  (loop [ps (vec pops) r []]
    (if (> pop-size (count r))
      (let [[m i] (->> (map #(vector (first %1) %2) ps (range))
                       (filter (comp some? first))
                       (reduce (fn [[m _ :as max] [x _ :as c]]
                                 (if (> (:fness x) (:fness m)) c max))))]
        (recur (update ps i rest) (conj r m)))
      r)))

(defn select [coll probs]
  (let [coll-probs (map vector coll probs)
        target (tlmt-rand)]
    (reduce
      (fn [S [x p]]
        (let [s (+ S p)]
          (if (> s target) (reduced x) s)))
      0
      coll-probs)))

(defn rand-pivot [n]
  (inc (tlmt-rand (dec n))))

(defn crossover [x y pivot]
  [(vec (concat (take pivot x) (drop pivot y)))
   (vec (concat (take pivot y) (drop pivot x)))])

(defn mutate [chrom alleles mutation-p]
  (mapv (fn [g a] (if (tlmt-rand-boolean mutation-p)
                    (->> (repeatedly #(rand-gene a)) (filter #(not= g %)) (first))
                    g))
        chrom alleles))

(defn pnl [msg]
  (fn [x] (println msg x) x))

(defn evolve []
  (let [pop-size 20
        mutation-p 0.01
        selection-p (normalize (cumul-probs pop-size))
        alleles (interleave (repeat 50 100) (repeat 50 10))
        ff (partial apply +)
        max-iter 1000
        accept-sol (* 0.95 (ff alleles))
        eval (partial eval-chroms ff)]
    (loop [pop (eval (rand-chroms pop-size alleles)) i 0]
      (if (and (> max-iter i) (> accept-sol (:fness (first pop))))
        (recur
          (unite pop-size
                 pop
                 (eval
                   (->> (repeatedly (partial select pop selection-p))
                        (map :chrom)
                        (partition 2)
                        (mapcat #(crossover (first %) (second %) (rand-pivot (count alleles))))
                        (map #(mutate % alleles mutation-p))
                        (take pop-size))))
          (inc i))
        [i (:fness (first pop))]))))

(def experiments 100)

(defn measure [f]
  (mapv #(/ % (double experiments))
        (reduce (fn [a x]
                  (print ".")
                  (flush)
                  (mapv #(+ %1 %2) a x))
                [0 0] (repeatedly experiments f))))

;; [215.64 5227.52]
