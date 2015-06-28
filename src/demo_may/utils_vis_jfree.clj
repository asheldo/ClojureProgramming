(ns demo-may.utils-vis-jfree
  (:use [incanter.core] 
        [incanter.stats] 
        [incanter.charts] 
        [incanter.io])
  )

; todo - http://data-sorcery.org/category/jfreechart/

(def vis-qwu-comp "testing qwu vis"
  (fn [{:keys [pairs data sizes] :as test-data} 
       root-f xs ys start title]
    (let [ct (count data)
          plot-ct (int (* xs ys))
          data-set (dataset
                    ["x" "y" "root"]
                    (vec
                     (take plot-ct 
                           (for [ix (range plot-ct)
                                 :let [pos (+ start ix)
                                       root (root-f data pos)]
                                 :when (< 1 (get sizes root))] 
                             (let [y (int (. Math floor (/ ix xs))) 
                                   x (rem ix xs)]
                               [x y root])))))
          ; x (range -3 3 0.1)
          _ (println data-set)
          plot (scatter-plot :x :y
                :data data-set
                :group-by "root"
                :title (str "Start " start)
                :x-label "x" :y-label "y")]
      ;(view plot)
      (save plot (str "./plot-" title "-" start "_" plot-ct "of" ct ".png")))))


(def vis-test1 "testing"
  (fn [pairs]
    (let [x (range -3 3 0.1)
          plot (dynamic-scatter-plot 
                [mean (range -3 3 0.1)
                 sd (range 0.1 10 0.1)]
                [x (pdf-normal x :mean mean :sd sd)]
                :title "Normal PDF Plot")]
      ;(view plot)
      (save plot "./plot-ex.png"))))

(def vis-test2 "todo" 
  (fn [pairs]
    (let [x (range -3 3 0.1)
          plot (dynamic-scatter-plot 
                [mean (range -3 3 0.1)
                 sd (range 0.1 10 0.1)]
                (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
                :title "Normal PDF Plot")]
      (save plot "./plot-ex2.png"))))
