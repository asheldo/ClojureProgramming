;;
;;
(ns 
    demo-may.utils-union)

;; shared
(def fast-join "faster, batches"
  (fn [join-f data pairs]
    (swap! data 
           #(loop [p pairs d %]
              (let [pair (first p)]
                (if (nil? pair) 
                  d
                  (recur (rest p) 
                         (join-f d (first pair) (second pair)))))))))
