;;
;;
(ns 
    demo-may.utils-union)

;; shared
;       (swap! test-data #(assoc :data (uu/fast-join join-f (:data %) pairs)))
(def fast-join "faster, batches"
  (fn [join-f test-data pairs]
    (swap! test-data 
           #(loop [p pairs data (assoc % :pairs pairs)]
              (let [pair (first p)]
                (if (nil? pair) 
                  data
                  (recur (rest p) 
                         (let [a (first pair) b (second pair)]
                           (join-f data a b)))))))))
