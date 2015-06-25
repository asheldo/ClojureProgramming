;; algo1
;;
(ns demo-may.algo1
  (:require [demo-may.utils-union :as uu])
  )

; join

(def stats (atom {:runs-ct 1 }))
(def -runs-mod (fn [s] (mod (:runs-ct s) 5)))
(def start-record (fn [s] 
                    (let [run (inc (-runs-mod s))] 
                      (assoc s :runs-ct run run 0 :seen 0 :path []
                             :start-time (. System currentTimeMillis)))))
(def circuit-break (fn [s] 
                     (let [max-steps 100]
                       (if (> (s (:runs-ct s)) max-steps)
                         (throw (Exception. (str "Circuit break: " s)))
                         s))))
(def record-step (fn [s msg] 
                   (let [run (:runs-ct s)]
                     (print msg) (circuit-break s)
                     (assoc s run (inc (s run))))))
(def end-record (fn [s] (assoc s 
                          :end-time (. System currentTimeMillis))))

(def -simple-connected
  (fn [all-conns left right]
    (let [left-conns (get all-conns left)]
      (and (some? left-conns) (contains? left-conns right)))))

; trampoline for tail recursion...
(def -recur-connected-lazy "Not eager b/c stops building component if A-B join is found. "
  (fn [all left-set right component]
    (swap! stats record-step "") ; (str "left+comp: " (+ (count left-set) (count component)))
    (if (or (= 0 (count left-set))) ; no more conns we haven't checked
      (let [_ (swap! stats #(assoc % :seen (count component)))]
        false) ; will end trampoline jumping
      (let [not-conn-set (for [left left-set 
                               :when (not (-simple-connected all left right))] 
                           left)
            connection (clojure.set/difference left-set not-conn-set)]
        (if (< 0 (count connection)) 
          (let [_ (swap! stats #(assoc % :path (conj [] (first connection))
                                       :seen (+ (count left-set) (count component))))] 
            true) ; will end trampoline jumping
          (let [connections (reduce #(clojure.set/union %1 (get all %2)) #{} left-set)
                check-set (clojure.set/difference connections component)
                grow-component (clojure.set/union component left-set)]
            (println (take 10 check-set))
            #(-recur-connected-lazy all check-set right grow-component)))))))

(def a-connected-lazy "Not eager b/c stops building component once A-B join is found in data."
  (fn [data-atom a b]
    (swap! stats start-record)
    (let [all (:all-connections @data-atom)
          ordered (list a b)
          left (first ordered)
          right (second ordered)
          component #{}
          has-l (contains? all left) 
          has-r (contains? all right)
          ok [left has-l right has-r]
          result (if (and has-l has-r)
                   (trampoline 
                    -recur-connected-lazy all #{left} right component)
                   false)]
      (swap! stats #(assoc (end-record %) :ok ok))
      (println (str "stats=" @stats))
      result)))

(def -join
  (fn [data a b]
    (let [a-set (get-in data [:all-connections a]) 
          b-set (get-in data [:all-connections b]) 
          update-a (if (nil? a-set) (hash-set b) (conj a-set b))
          update-b (if (nil? b-set) (hash-set a) (conj b-set a))]
      (-> data (assoc-in [:all-connections a] update-a)
          (assoc-in [:all-connections b] update-b)))))

;; deprecated
  (def -simple-join "join two objects/nodes"
  (fn [data a b]
    (swap! data -join a b)
    data))

;; starting data
(def dummy-object (. Integer MAX_VALUE)); get full component, > 0 for sort
(def a-data "init" 
  (fn [n] ; empty connections map to start
    (atom {:n n :all-connections {dummy-object #{ dummy-object}} })))

;;
;; test stuff
;;

(def -log-it
  "e.g. xs=1000 ys=25 max=5000 - not sparse. Connect bottom half to top half (arbitrary)"
  (fn [test-data file xs ys max]
    (if (and
         (< (* xs ys) 20000) (contains? @test-data :all-connections))      ; sort and 
      (do
        (swap! test-data #(assoc % :all-connections (into (sorted-map) (:all-connections %))))
        (spit file @test-data)
        (println file)))))

; FIXME
(def -make-low-high-pairs
  "e.g. xs=1000 ys=25 max=5000 - not sparse. Connect bottom half to top half (arbitrary)"
  (fn [xs ys max]
    (let [half-max (int (* 0.5 max))] ; bottom half
      ; e.g. 500 rand Xs each paired with 5 rand Ys in other half
      (for [left (repeatedly xs #(rand-int half-max))
            right (repeatedly ys #(+ half-max (rand-int half-max)))]
        [left right]))))

(def -join-testdata 
  "e.g. xs=1000 ys=25 max=5000 - not sparse. Connect bottom half to top half (arbitrary)
   Then => (for [a (range 0 10) b (range 4990 5000) :when (a-connected-lazy at a b))] [a b])"
  (fn [test-data join-f log-f pairs xs ys max]
    (let [start (. System currentTimeMillis)]
          ; (a-join test-data left right)
      (uu/fast-join join-f test-data pairs)
      (println (str "Elapsed: " (- (. System currentTimeMillis) start) 
                    " for " max))
      test-data)))

;; a-maketestdata
(def a-maketestdata 
  (fn [xs ys max]
    (let [test-data (a-data max)
          file (str "data." max "_" (* xs ys) ".log-it")
          pairs (-make-low-high-pairs xs ys max)]
      (-join-testdata test-data -join pairs xs ys max)
      (-log-it test-data file xs ys max)
      test-data)))

(def a-check "abstract function"
  (fn [a-data f]
     (for [n (range (:n @a-data)) 
           :when (f (get-in @a-data [:all-connections n]))] 
       n)))

(def a-checksparse ":when some?"
  (fn [a-data]
    (a-check a-data some?)))

(def a-checkdense ":when nil?"
  (fn [a-data]
    (a-check a-data nil?)))






