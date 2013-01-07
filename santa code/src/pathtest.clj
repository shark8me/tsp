(ns pathtest)
(use '[clojure.test :as ctest])
(use '[greedy :as greedy])
(use '[cityutil :as ct])
(use '[altpath :as altpath])
(use '[pathutil :as pathutil])
(use '[stats :as stats])

(ctest/deftest reading
  (let [pfile "D:\\home\\kaggle\\santa\\cloj150k.txt"] 
  (ctest/is (= [118979 112543] (subvec (pathutil/readpath pfile) 0 2)))
  (ctest/is (= [118979 112543] (pathutil/nearby-interleaved 2 (vec (range 10)))))
  (ctest/is (= #{'(2 1) '(3 2) '(1 0) '(2 3) '(0 1) '(1 2)} 
               (pathutil/existing-edges (vec (range 4)))))))


(ctest/deftest stats-test
  (let [last5  (repeatedly 20 #(rand-int 21))
        t2 [12 14 16 1 1 ]] 
  ;(ctest/is (= 6.0 (stats/maxDistOfNext t2 3 10 2)))
  ;(ctest/is (= 6.0 (stats/maxDistOfNext t2 3 10 2)))
  (ctest/is (= 8 (stats/maxDistOfNext2 12 10 5)))
  (ctest/is (= 5 (stats/maxDistOfNext2 16 10 5)))))

 
(ctest/deftest measure-path
  (let [cit (ct/getCities "D:\\home\\kaggle\\santa\\santa_cities.csv")
        path (pathutil/readpath "D:\\home\\kaggle\\santa\\cloj150k.txt")
        dists (pathutil/getdistance [32988,11399,2123] (vec (range 32990 33000)) cit )]
    ;(ct/measure-dist-path path cit))
    (is (= 4711.448609504299 (first (first (sort #(compare (%1 0) (%2 0)) dists)))))))

(ctest/run-tests 'pathtest)
