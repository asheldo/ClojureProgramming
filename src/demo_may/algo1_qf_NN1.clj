;; algo1-qg
;; "qf"
(ns demo-may.algo1-qf-nn1
  (:require [demo-may.utils-union :as uu])
  )

; q-f quick-find with cost init N, join N (worst), find 1

(def stats (atom {:runs-ct 1}))

(def a-data "init"
  (fn [n]
    (atom (vec (range 0 n)))))

; doesn't deserve a function
(def root "return 0-based indexes value (root)"
  (fn [data i]
    (get data i)))

;; TODO this should search for all components in vec
;;  that have current a-root
(def -join "replace 0-based index's value (new root)"
  (fn [data a b]
    (let [a-root (get data a) ; set all a to b
          b-root (get data b)
          ct (count data)]
      (loop [t-data (transient data) i 0]
        (if (= i ct)
          (persistent! t-data)
          (if (= a-root (get t-data i))
            (recur (assoc! t-data i b-root) (inc i))
            (recur t-data (inc i))))))))

;; TESTING

(def -log-it
  (fn [test-data file xs ys max]
    (if (< 0 max 10000)
      (do
        (spit file @test-data)
        (println file)))))

; FIXME
(def -make-low-high-pairs
  (fn [xs ys max]
    (let [half-max (int (* 0.5 max))] ; bottom half
      ; e.g. 500 rand Xs each paired with 5 rand Ys in other half
      (for [left (repeatedly xs #(rand-int half-max))
            right (repeatedly ys #(+ half-max (rand-int half-max)))]
        [left right]))))

(def -join-testdata 
  "e.g. xs=1000 ys=25 max=5000 - not sparse. Connect bottom half to top half (arbitrary)
   Then => (for [a (range 0 10) b (range 4990 5000) :when (a-connected-lazy at a b))] [a b])"
  (fn [test-data join-f pairs xs ys max]
    (let [start (. System currentTimeMillis)]
          ; (a-join test-data left right)
      (uu/fast-join join-f test-data pairs)
      (println (str "Elapsed: " (- (. System currentTimeMillis) start) 
                    " for " max))
      (println (str "take 5 pairs: " (apply str (take 5 pairs))))
      test-data)))

(def a-maketestdata 
  (fn [xs ys max]
    (let [test-data (a-data max)
          file (str "data." max "_" (* xs ys) ".log-it")
          pairs (-make-low-high-pairs xs ys max)]
      (-join-testdata test-data -join pairs xs ys max)
      (-log-it test-data file xs ys max)
      test-data)))










