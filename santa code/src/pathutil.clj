(ns pathutil)
(use '[cityutil :as ct])
(import '(java.io FileReader BufferedReader))

(defn readpath [filename]
  (let [path  (line-seq (BufferedReader. 
                 (FileReader. filename)))]
    (vec (for [i path] (Integer/parseInt i))))) 

(defn existing-edges [path]
  (let [rset #(set (partition 2 1 %))]
    (into (rset path) (rset (rseq path))))) 

(defn nearby-interleaved [ith vect]
  (interleave (subvec vect (inc ith)) (rseq (subvec vect 0 ith)))) 

(defn getdistance [src-city cityidlst cities]
  "Returns a list where each item is [dist, city] and
   the inputs are [32988,11399,2123] (vec (range 32990 33000)) cit"
  (let [xy #(subvec % 1)
        srcxy (xy src-city)]
    (vec (for [j (for [i cityidlst]
              ((vec cities) i))]
      [(ct/rmsdist srcxy (xy j)) j]))))      