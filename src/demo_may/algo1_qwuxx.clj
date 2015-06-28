;; algo1-qwuxx.clj
;; "qwu" quick weighted union (plus optimization of paths walked?)
;;  and find on average case, but not worst case (tall trees)
(ns demo-may.algo1-qwuxx
  (:require [demo-may.utils-union :as uu]
            [demo-may.utils-vis-jfree :as vis]
            [demo-may.algo1-qu-nnn :as qu])
  )

; q-u quick-union with cost init N, join N (worst), find N (worst)
; "tall trees"

(def stats (atom {:runs-ct 1}))

(def a-data "init"
  (fn [n]
    (atom {:sizes (vec (repeat n 1))
           :data (vec (range 0 n))})))

;; TODO weighted versions of everything:

; in quick-union and qwf, *does* deserve a function
(def -root "return 0-based indexes value (root)"
  (fn [data i]
    (loop [i i] 
      (let [parent-of-i (get data i)]
        (if (= i parent-of-i)
          i
          (recur parent-of-i))))))

; e.g. (def at (a-maketestdata 5000 5 200000)) 
; (take 1  (for [a [72] b (range 0 200000) :when (a-connected @at a b)] [a b]))
(def a-connected "fast if root fast, tree shallow"
  (fn [{:keys [data]} a b]
    (if (= a b)
      false
      (= (-root data a) (-root data b)))))

; meat of qwf - don't just arbitrarily pick which to get subsumed
; (mathematics? the log N times that can possibly double statement?
; e.g. how many times depth can increase by 1... 
; also depth increases monotonically with count (smaller always shorter)
; 4 x 3 = 8 x 4
; 1 x 1 + 1 x 1 = 2 x 2 + 1 x 1 = 3 x 2 + 2 x 2 = 5 x 3 + 4 x 3 = 9 x 4
(def -join "replace 0-based index's value (new root)"
  (fn [test-data a b]
    ; since we are called from fast-join, we don't use transient despite speed adv.
    (let [{:keys [sizes data]} test-data] ; "root" is position equal to its value
      (let [root-of-a (-root data a) root-of-b (-root data b)]
        (if (= root-of-a root-of-b)
          test-data
          (let [size-of-a (get sizes root-of-a) size-of-b (get sizes root-of-b)]
            (if (< size-of-a size-of-b)
              (assoc test-data
                :sizes (assoc sizes root-of-b (inc size-of-b))
                :data (assoc data root-of-a root-of-b))
              (assoc test-data
                :sizes (assoc sizes root-of-a (inc size-of-a))
                :data (assoc data root-of-b root-of-a)))))))))

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
 Then => (take 1  (for [a [72] b (range 0 200000) :when (a-connected @at a b)] [a b]))"
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

(def a-vis (fn [data xs ys start title]
             (vis/vis-qwu-comp data -root xs ys start title)))
