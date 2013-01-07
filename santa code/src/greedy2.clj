(ns greedy2)
(use '[cityutil :as ct])
(use '[pathutil :as pathutil])
(use '[stats :as stats])

(defn xy-ids [cityid]
  (let [qfn #(quot % 100)]
    [(qfn (cityid 1)) (qfn (cityid 2))]))

(defn makeds [cities]
  (let [ifn (fn[x y] 
              (conj x (first y)))]
    (reduce #(merge-with ifn %1 %2) (for [i cities] {(xy-ids i) #{(i 0)}}))))

(defn getboxes [city n]
  (let [end (- (* n 2) 1)
        rng (map #(- % (- n 1)) (range end))
        boxes (for [i rng j rng] [i j])]
    (for [i boxes] [(+ (city 1) (i 0)) (+ (city 2) (i 1))])))

(defn remove-chosen-city [chosen citiesds]
  "removes the chosen city id from the set (value) in 
  citiesds"
  (let [xy (xy-ids chosen)
        mapval (citiesds xy)]
    (assoc citiesds xy (disj mapval (chosen 0)))))

(defn search-one-ring [{:keys [src layer cities_ds cities disallowed_city_ids] :as m}]
  (do ;(println (str "src " src "layer " layer "disallowed " disallowed_city_ids))
  (let [srcxy (into [(src 0)] (xy-ids src))
        boxnos (getboxes srcxy  layer)
        wantednot (conj disallowed_city_ids (src 0)) ;add the src so it doesn't find path to itself
        cities2srch (filter #(not (wantednot %))
                            (reduce #(into %1 (cities_ds %2)) [] boxnos))
        mindist (first (sort #(compare (%1 0) (%2 0)) (pathutil/getdistance src cities2srch cities)))]
    (if (nil? mindist) (assoc m :layer (inc layer)) 
      (let [chosen (second mindist) ]
        (assoc m :cand mindist :cities_ds (remove-chosen-city chosen cities_ds)))))))

(defn run-until-cand-found [ mapin]
  (do
    ;(println (str "run-until-cand-found " (mapin :src)))
  (first (filter :cand (iterate search-one-ring mapin)))))
            
(defn getNextCand [{:keys [cities_ds cities path-so-far] :as m}]
  "get the cand with the least distance to src"
  (let [cnt (count path-so-far)
        lastcity (peek path-so-far) 
        mapin (assoc m :src (lastcity 1) :layer 1
                      :disallowed_city_ids 
                      (if (> (count path-so-far) 1)
                        #{(first (second (second (rseq path-so-far))))} #{}))
        nextCan (run-until-cand-found mapin)]
    (if (= 0 (rem cnt 100)) (println (str "finished " cnt)))
    ;(println (str "nextcan returned in getnextcan " (nextCan :cand)))
    (assoc (dissoc nextCan :cand) :path-so-far (conj path-so-far (nextCan :cand)))))

(defn makepath [pathop]
  (let [cities (ct/getCities "D:\\home\\kaggle\\santa\\santa_cities.csv")
        cities_ds (remove-chosen-city (first cities) (makeds cities))
        start (first cities)
        res ((last (take 149998 (iterate getNextCand 
                                {:cities_ds cities_ds :cities cities 
                                 :path-so-far [ [ 0 start]]}))) :path-so-far)]
    (cityutil/writepath (for [i res] (i 1)) pathop)
    (apply + (for [i res] (i 0)))))



(defn find-box-cand [city-xy-cords xymap]
  (xymap (xy-ids city-xy-cords)))
