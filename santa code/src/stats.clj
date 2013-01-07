(ns stats)

(defn maxDistOfNext [dist n globalMean mindist]
  (let [run-avg (/ (apply + (take n dist)) (float n))
        dist (- globalMean (- run-avg globalMean))]
    (if (< dist 5) mindist dist)))

(defn maxDistOfNext2 [run-avg globalMean mindist]
  (let [dist (- globalMean (- run-avg globalMean))]
    (if (< dist 5) mindist dist)))

(comment (defn getNextn [dist n globalMean mindist]
  (let [run-avg (/ (apply + (take n dist)) (float n))]
  (maxDistOfNext dist ))))