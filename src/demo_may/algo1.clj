(ns demo-may.algo1
  )

; join

(def stats (atom {:runs-ct 1 0 0 1 8}))
(def circuit-break (fn [s] 
                   (let [run (:runs-ct s)]
                     (if (> (s run) 20) (throw "Circuit break") s))))
(def record-step (fn [s] 
                   (let [run (:runs-ct s)]
                     (circuit-break s)
                     (assoc s run (inc (s run))))))
(def start-record (fn [s] 
                    (let [run (inc (:runs-ct s))] 
                      (assoc s :runs-ct run run 0))))

(def -simple-connected
  (fn [all-conns left right]
    (let [left-conns (get all-conns left)]
      (and (some? left-conns) (contains? left-conns right)))))

; trampoline...
(def -recur-connected-nostack
  (fn [all-conns left-set right seen-set]
    (swap! stats record-step)
    (if (or (= 0 (count left-set)))
      false
      (if (> (count left-set) 
             (count (for [left left-set 
                          :while 
                          (not (-simple-connected all-conns left right))] 
                      left)))
        true
        (let [left-conns 
              (clojure.set/difference ; remove already seen
               (reduce #(clojure.set/union %1 (get all-conns %2))
                       #{} left-set)
               seen-set)
              seen (clojure.set/union seen-set left-set)]
          (println left-conns)
          (println seen)
          #(-recur-connected-nostack all-conns left-conns right seen))))))

(def a-connected
  (fn [data-atom a b]
    (swap! stats start-record)
    (let [all-conns (:all-connections @data-atom)
          ordered (sorted-set a b)
          left (first ordered)
          right (second ordered)                                
          result (if (and (contains? all-conns left) (contains? all-conns right))
                   (trampoline 
                         -recur-connected-nostack all-conns #{left} right #{})
                   false)]
      (println (str "stats=" @stats))
      result)))

(def -join
  (fn [data a b]
    (let [ordered (sorted-set a b)
          left (first ordered)
          right (second ordered) ; always store connection with left
          left-set (get-in data [:all-connections left]) 
          right-set (get-in data [:all-connections right]) 
          update-left (if (nil? left-set)
                        (hash-set right) 
                        (conj left-set right))
          update-right (if (nil? right-set)
                        (hash-set left) 
                        (conj right-set left))]
      (-> data 
          (assoc-in [:all-connections left] update-left)
          (assoc-in [:all-connections right] update-right)))))

(def a-join "join two objects/nodes"
  (fn [data a b]
    (swap! data -join a b)
    data))

(def a-data "init" 
  (fn [n] ; empty connections map to start
    (atom {:n n :all-connections {} })))

(def a-maketestdata "e.g. 1000 25 5000 - not sparse"
  (fn [xs ys max]
    (let [test-data (a-data max) ; atom
          half-max (int (* 0.5 max))]
      (println half-max)
      (doseq [left (repeatedly xs #(rand-int half-max))
              right (repeatedly ys #(+ half-max (rand-int half-max)))]
         ; (println (str x "," y))
        (a-join test-data left right))
      (swap! test-data #(assoc % :all-connections (into (sorted-map) (:all-connections %))))
      (spit (str "data." max "_" (* xs ys) ".log-it") @test-data)
      test-data)))

(def a-check
  (fn [a-data f]
     (for [n (range (:n @a-data)) 
           :when (f (get-in @a-data [:all-connections n]))] 
       n)))

(def a-checksparse
  (fn [a-data]
    (a-check a-data some?)))

(def a-checkdense
  (fn [a-data]
    (a-check a-data nil?)))
