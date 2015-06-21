(ns demo-may.algo1
  )

; join

(def a-data "init" 
  (fn [n] ; empty connections map to start
    (atom {:n n :all-connections {} })))

(def -simple-connected
  (fn [all-conns left right]
    (let [left-conns (get all-conns left)]
      (and (some? left-conns) (contains? left-conns right)))))

(def stats (atom {:runs-ct 1 0 0 1 8}))
(def record-step (fn [s] 
                   (let [run (:runs-ct s)]
                     ; (print (str (inc (s run)) " "))
                     (assoc s run (inc (s run)))
                     )))
(def start-record (fn [s] 
                    (let [run (inc (:runs-ct s))] 
                      ; (println (str "run " run))
                      (assoc s :run-ct run run 0)
                      )))

; stack overflow @ 1000?
(def -recur-connected-mixedup
  (fn [all-conns left right]
    (swap! stats record-step)
    (if (-simple-connected all-conns left right)
      true
      (loop [left-conns (get all-conns left)]
        ; (swap! stats inc)
        (if (or (nil? left-conns) (empty? left-conns))
          false
          (if (-recur-connected-mixedup 
               all-conns (first left-conns) right)
            true
            (recur (rest left-conns))))))))

; trampoline...
(def -recur-connected-nostack
  (fn [all-conns left-set right]
    (swap! stats record-step)
    (if (or (= 0 (count left-set)))
      false
      (if (> (count left-set) 
             (count (for [left left-set 
                          :while 
                          (not (-simple-connected all-conns left right))] 
                      left)))
        true
        (let [left-conns (reduce 
                                       ; #(apply conj %1 (get all-conns %2)) 
                                        ; #(conj %1 (get all-conns %2))
                          #(clojure.set/union %1 (get all-conns %2))
                          #{} left-set)]
          (println left-conns)
          #(-recur-connected-nostack all-conns left-conns right))))))

(def a-connected
  (fn [data-atom a b]
    (swap! stats start-record)
    (let [all-conns (:all-connections @data-atom)
          ordered (sorted-set a b)
          left (first ordered)
          right (second ordered)                                
        ;  result (-simple-connected all-conns left right)
        ;  result (-recur-connected-mixedup all-conns left right)
          result (trampoline 
                  -recur-connected-nostack all-conns #{left} right)]
      (println (str "stats=" @stats))
      result)))

(def -join
  (fn [data a b]
    (let [ordered (sorted-set a b)
          left (first ordered)
          right (second ordered) ; always store connection with left
          connected-set (get-in data [:all-connections left]) 
          updated-set (if (nil? connected-set)
                        (hash-set right) (conj connected-set right))]
      (assoc-in data [:all-connections left] updated-set))))

(def a-join "join two objects/nodes"
  (fn [data a b]
    (swap! data -join a b)
    data))

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
