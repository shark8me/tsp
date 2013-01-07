(ns cityutil)
(use ['clojure.string :only '(split)])
(use '[clojure.test :as t])
(import '(java.io FileReader BufferedReader))

(defn getCities [filename]
  (let [cities  (line-seq (BufferedReader. 
                 (FileReader. filename)))]
    (for [k (partition 3 
                     (for [i (rest cities), j (split i #",")] (Integer/parseInt j) ))] 
    (vec k))))

(defn sortbyxy [cities]
  ^{:doc "sort by x then y"
   :test (fn []
             (assert (= [[6 7 2] [5 10 9] [0 10 11] [5 10 15]]
                        (sortbyxy [[0 10 11] [5 10 9] [5 10 15] [6 7 2]]))))}
  (letfn [ (cpare [x y]
                  (let [fnx #(quot % 50) 
                        f1 (compare (fnx (x 1)) (fnx (y 1)))]
                    (if (zero? f1) (compare (fnx (x 2)) (fnx (y 2))) f1)))]
    (vec (sort cpare cities))))

(defn rmsdist [p1 p2]
  (Math/sqrt (+ (Math/pow (- (p1 0) (p2 0)) 2)
                (Math/pow (- (p1 1) (p2 1)) 2))))

(defn measure-dist [cities]
  (apply + (for [i (partition 2 1 cities) ] 
    (rmsdist (vec (rest (first i))) (vec (rest (second i)))))))

(defn writepath [path fileop]
  (spit fileop (clojure.string/join "\n" (vec (for [i path] (i 0))))))

(defn measure-dist-path[path,cities]
  (let [map-id-key (zipmap (for [i cities] (i 0)) cities)]
    (measure-dist (for [i path] (map-id-key i)))))

(def cit (getCities "D:\\home\\kaggle\\santa\\santa_cities.csv"))