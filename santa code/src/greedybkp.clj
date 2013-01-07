(ns greedybkp)

(defn nearby[ i n v]
  ^{:test (fn [] (= [2 3 4 6 7 8] (greedy/nearby 5 3 [0 1 2 3 4 5 6 7 8 9])))}
  (let [i2 (inc i)
        lst (let [i2n (+ i2 n)]
          (if (>= i2n (count v)) (subvec v i2) (subvec v i2 i2n)))
        flst (let [i2n (- i n)]
               (if (> 0 i2n) (subvec v 0 i) (subvec v i2n i))) ]
    (into flst  lst)))

(defn nextCand [src srch-dist sor_cities added-set-fn]
  (greedy/dist (sor_cities src) (greedy/nearby-fn src srch-dist added-set-fn sor_cities)))

(defn nearby-fn [i n fnx v]
  (let [taken (fn[s] (vec (take n (filter fnx s)))) 
        flst (taken (subvec v (inc i)))
        blst (taken (rseq (subvec v 0 i)))]
    (into blst flst)))