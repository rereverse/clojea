(ns clojea.random.mtrandom
  (:import [clojea.random MersenneTwisterFast]))

(def ^:private thread-local-mersenne-twister
  (proxy [ThreadLocal] []
    (initialValue []
      (new MersenneTwisterFast
           (long (* (rand) Long/MAX_VALUE))))))

(defn tlmt-rand
  ([] (.. thread-local-mersenne-twister (get) (nextDouble)))
  ([n] (.. thread-local-mersenne-twister (get) (nextInt (int n)))))

(defn tlmt-rand-boolean
  ([] (.. thread-local-mersenne-twister (get) (nextBoolean)))
  ([p-true] (>= (tlmt-rand) (- 1.0 p-true))))
