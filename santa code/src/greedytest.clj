(ns greedytest)
(use '[clojure.test :as ctest])
(use '[greedy :as greedy])
(use '[greedybkp :as gbkp])
(use '[cityutil :as ct])

(ctest/deftest nearness
  (let [v [0 1 2 3 4 5 6 7 8 9]]
  (ctest/is (= [2 3 4 6 7 8] (gbkp/nearby 5 3 v)))
  (ctest/is (= [4 5 6 8 9] (gbkp/nearby 7 3 v)))
  (ctest/is (= [5 6 7 9] (gbkp/nearby 8 3 v)))
  (ctest/is (= [6 7 8] (gbkp/nearby 9 3 v)))
  (ctest/is (= [0 2 3 4] (gbkp/nearby 1 3 v)))
  (ctest/is (= [1 2 3] (gbkp/nearby 0 3 v)))))

(ctest/deftest near2
  (let [v [0 1 2 3 4 5 6 7 8 9]
        sfn (fn [x] #(not ((set x) %)))]
    (ctest/is (= [3 2 1 6 8 9] (greedy/nearby-fn 5 3 (sfn [7 4]) v)))
    (ctest/is (= [0 3 5 6] (greedy/nearby-fn 1 3 (sfn [2 4]) v)))))

(ctest/deftest srch
  (let [cit (ct/getCities "D:\\home\\kaggle\\santa\\santa_cities.csv")
        sc (cityutil/sortbyxy (take 20 cit))
        res (greedy/makepath sc)
        ]
    ;(ctest/is (= [8 8537 8648] (greedy/nextCand 10 5 sc #(not (#{} %)))))
    (ctest/is (= [10 8928 9387] ((greedy/nextCand2 (peek sc) 10 (pop sc)) 0)))
    (ctest/is (= [[9 19069 2226] [16 9562 1617]] 
                 ((greedy/addOne {:sor_cities (pop sc) :path [(peek sc)]}) :path)))
    (ctest/is (= 20 (count res)))
    (ctest/is (= 47545.74791949193 (ct/measure-dist res)))
    (ctest/is (= nil (ct/writepath res 
                                   "D:\\home\\kaggle\\santa\\clojtest.txt")))))



(ctest/run-tests 'greedytest)