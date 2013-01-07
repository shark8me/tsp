(ns greedy)
(use '[cityutil :as ct])
(use '[clojure.test :as ctest])
;tsp path solver, chooses the nearest neighbour using local search.

(defn nearby2 [n vect]
  (vec (take n (rseq vect))))

(defn dist [src lst]
  (let [d (for [i (range (count lst))] [(ct/rmsdist [(src 1) ((lst i) 1)] [(src 2) ((lst i) 2)]) i])]
    (second (first (sort #(compare (%1 0) (%2 0)) d)))))

(defn nextCand2 [ src srch-dist sor_cities ]
  (let [ithindex (greedy/dist src (greedy/nearby2 srch-dist sor_cities))]
    [(sor_cities ithindex) 
     (into (subvec sor_cities 0 ithindex) 
           (subvec sor_cities (inc ithindex)))]))

(defn addOne [{:keys [sor_cities path]}]
  (let [srch-cand (peek path)
        [nc nsor_cities] (nextCand2 srch-cand 100 sor_cities)]
    {:sor_cities nsor_cities :path (conj path nc)}))

(defn makepath [sor_cities]
  (let [start (peek sor_cities)
        restn (pop sor_cities)
        res (last (take-while #(> (count (:sor_cities %)) 0) 
                        (iterate addOne {:sor_cities restn :path [start]})))]
    (conj (:path res) (peek (:sor_cities res)))))

(defn runalgo [cities fin-path]
  (time 
    (let [cit (ct/getCities cities)
          sc (cityutil/sortbyxy cit)
          res (greedy/makepath sc)]
      (println (str "total distance " (ct/measure-dist res)))
      (ct/writepath res fin-path))))
  
;(runalgo "D:\\home\\kaggle\\santa\\santa_cities.csv" "D:\\home\\kaggle\\santa\\cloj150k.txt")




  