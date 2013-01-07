(ns greedy2test)
(use '[clojure.test :as ctest])
(use '[cityutil :as ct])
(use '[greedy2 :as g2])

(ctest/deftest dstests
  (let [cit (ct/getCities "D:\\home\\kaggle\\santa\\santa_cities3.csv")
        cit2 (ct/getCities "D:\\home\\kaggle\\santa\\santa_cities.csv")
        m2 (g2/makeds cit2)
        ith 13421
        src (nth cit2 ith)
        init (m2 (greedy2/xy-ids src))
        defmap {:src src :layer 1 :cities_ds m2 
                :cities cit2 :disallowed_city_ids (set [])}
        defmap2 (assoc defmap :src [22695,3416,3389])
        ]
    (is (= 22108 (count (filter #(> (count (% 1)) 1) m2))))
    (is (= #{33000 40671 56327 56908 64363 109901 114083 120904 130291}
           (g2/find-box-cand [21 12501 3920] m2)))
    (is (= #{33000 40671 56327 56908 64363 109901 114083 120904 130291}
           ((g2/makeds cit2) [125 39])))
    (is (= [47.53945729601885 [76261 3536 229]] 
           ((greedy2/search-one-ring defmap) :cand)))
    (is (= nil 
           ((greedy2/search-one-ring defmap2) :cand)))
    (is (= [39.56008088970496 [110859 3619 203]]
           ((greedy2/search-one-ring (assoc defmap :layer 2)) :cand)))
    (is (= [54.91812087098393 [39950 3572 271]] 
           ((greedy2/search-one-ring (assoc defmap :disallowed_city_ids (set [76261]))) :cand)))
    (is (= #{76261 39950 75188} 
           ((greedy2/remove-chosen-city src m2) (greedy2/xy-ids src))))
    (is (= [109.7679370308106 [30341 3384 3284]] 
           ((greedy2/run-until-cand-found defmap2) :cand)))
    (is (= [109.7679370308106 [30341 3384 3284]] 
           ((greedy2/getNextCand (assoc defmap2 :path-so-far [[0 [22695,3416,3389]]])) :cand)))
    (is (= [74.84650960465692 [93741 3315 3255]] 
           ((greedy2/getNextCand 
              (assoc defmap2 :path-so-far 
                     [[0 [22695,3416,3389]] [109.7679370308106 [30341 3384 3284]]])) :cand))))
   )

(ctest/deftest total
  (time (greedy2/makepath "D:\\home\\kaggle\\santa\\testop.txt")))
(ctest/deftest others 
  (is (= '([1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2] [3 3])
         (g2/getboxes [0 2 2] 2)))
  (is (= '([2 2])
         (g2/getboxes [0 2 2] 1)))
  (is (= 25
         (count (g2/getboxes [0 2 2] 3)))))

;(ctest/run-tests 'greedy2test)