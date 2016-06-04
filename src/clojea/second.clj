(ns clojea.second)

(def chromosome-len 500)
(def max-gene-val 250)
(def population-size 20)
(def mutation-prob 0.1)
(def survivors-size (/ population-size 2))
(def max-solution (* chromosome-len max-gene-val))

(defn rand-boolean [probability-true]
  (>= (rand) (- 1.0 probability-true)))

(def sum (partial apply +))

(defn random-gene [] (+ 1 (rand-int max-gene-val)))

(defn random-chromosome [] (seq (repeatedly chromosome-len random-gene)))

(defn random-population []
  (take population-size (repeatedly random-chromosome)))

(defn evaluate [fitness-fn population]
  (reduce
    (fn [acc chromosome] (conj acc {:result (fitness-fn chromosome) :chromosome chromosome}))
    []
    population))

(defn sort-population [population] (sort-by :result > population))

(defn select-survivors [eval-population] (take survivors-size eval-population))

(defn crossover [cx cy]
  (let [pivot (inc (rand-int (dec chromosome-len)))]
    (concat (take pivot cx) (drop pivot cy))))

(defn mutate [c]
  (assoc (vec c) (rand-int chromosome-len) (random-gene)))

(defn offspring-pop [eval-population]
  (repeatedly population-size
              #(let [offspring (apply crossover (map :chromosome (take 2 (shuffle eval-population))))]
                (if (rand-boolean mutation-prob)
                  (mutate offspring)
                  offspring))))

(defn evolve []
  (let [initial (random-population)]
    (loop [current-pop (evaluate sum initial)]
      (let [eval-pop (sort-population current-pop)
            best (:result (first current-pop))]
        (if (> best (* 0.9 max-solution))
          (println best)
          (do
            (println best)
            (recur (evaluate sum (offspring-pop (select-survivors eval-pop))))))))))
