(ns clojea.adaptive
  (:use [clojea.random.mtrandom]))


(defn rand-gene [max]
  (inc (tlmt-rand max)))

(defn rand-chromo [alleles]
  (mapv rand-gene alleles))

(defn rand-chroms [n alleles]
  (repeatedly n (partial rand-chromo alleles)))

(defn eval-chroms [ff chroms]
  (->> chroms
       (map #(hash-map :chrom % :fitness (ff %)))
       (sort-by :fitness >)))

(defn linear-probs [n]
  (let [decn (take n (iterate dec n))
        sum (double (* n (/ (+ n 1) 2)))]
    (map #(/ % sum) decn)))

(defn cumul-probs [n]
  (let [incn (take n (iterate inc 1))
        cumuln (map #(/ 1 %) incn)
        sum (double (apply + cumuln))]
    (map #(/ % sum) cumuln)))

(defn select [coll probs]
  (let [coll-probs (map vector coll probs)
        target (tlmt-rand)]
    (reduce
      (fn [S [x p]]
        (let [s (+ S p)]
          (if (> s target) (reduced x) s)))
      0
      coll-probs)))

(defn select-two [coll probs]
  (let [x (select coll probs)]
    [x (->> (repeatedly (partial select coll probs))
            (filter (partial not= x))
            (first))]))

(defn rand-pivot [n]
  (inc (tlmt-rand (dec n))))

(defn crossover [x y pivot]
  [(vec (concat (take pivot x) (drop pivot y)))
   (vec (concat (take pivot y) (drop pivot x)))])

(defn mutate [chrom alleles mutation-p]
  (mapv
    (fn [g a] (if (tlmt-rand-boolean mutation-p) (rand-gene a) g))
    chrom alleles))

(defn pnl [msg]
  (fn [x] (println msg x) x))

(defn evolve []
  (let [pop-size 20
        elitism 2
        crossover-p 1
        mutation-p 0.01
        selection-p (cumul-probs pop-size)
        alleles (repeat 100 2)
        ff (partial apply +)
        max-iter 1000
        accept-sol (ff alleles)
        eval (partial eval-chroms ff)]
    (loop [pop (eval (rand-chroms pop-size alleles)) i 0]
      (if (and (not= max-iter i) (> accept-sol (:fitness (first pop))))
        (recur
          (eval
            (concat
              (->> (take elitism pop)
                   (map :chrom))
              (->> (repeatedly (partial select-two pop selection-p))
                   (map #(map :chrom %))
                   (mapcat (fn [[x y :as xy]] (if (tlmt-rand-boolean crossover-p)
                                                (crossover x y (rand-pivot (count alleles))) xy)))
                   (map #(mutate % alleles mutation-p))
                   (take (- pop-size elitism)))))
          (inc i))
        [i (:fitness (first pop))]))))

(def experiments 100)

(defn measure [f]
  (mapv #(/ % (double experiments))
        (reduce (fn [a x]
                  (print ".")
                  (flush)
                  (mapv #(+ %1 %2) a x))
                [0 0] (repeatedly experiments f))))

(comment
  (measure evolve)                                          ;; no elitism [1000.0 9657.06]
  (measure evolve)                                          ;; w/ elitism [1000.0 9774.24]
  (measure evolve)                                          ;; w/ 1.0 COP [1000.0 9787.65]
  (measure evolve)                                          ;; [112.1 200.0]
  )
