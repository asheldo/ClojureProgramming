(ns demo-may.algo1
  )

; join

(def stats (atom {:runs-ct 1 0 0 1 8}))
(def circuit-break (fn [s] 
                   (let [run (:runs-ct s)]
                     (if (> (s run) 40) (throw "Circuit break") s))))
(def record-step (fn [s] 
                   (let [run (:runs-ct s)]
                     (circuit-break s)
                     (assoc s run (inc (s run))))))
(def start-record (fn [s] 
                    (let [run (inc (:runs-ct s))] 
                      (assoc s :runs-ct run run 0 
                             :start-time (. System currentTimeMillis)))))
(def end-record (fn [s] 
                  (assoc s :end-time (. System currentTimeMillis))))

(def -simple-connected
  (fn [all-conns left right]
    (let [left-conns (get all-conns left)]
      (and (some? left-conns) (contains? left-conns right)))))

; trampoline for tail recursion...
(def -recur-connected-lazy "Not eager b/c stops building component if A-B join is found. "
  (fn [all left-set right component]
    (swap! stats record-step)
    (if (or (= 0 (count left-set))) ; no more conns we haven't checked
      (let [_ (swap! stats #(assoc % :seen (count component)))]
        false) ; will end trampoline jumping
      (let [not-conn-set (for [left left-set 
                               :when (not (-simple-connected all left right))] 
                           left)
            connection (clojure.set/difference left-set not-conn-set)]
        (if (< 0 (count connection)) 
          (let [_ (swap! stats #(assoc % :path (conj [] (first connection))
                                       :seen (count component)))] 
            true) ; will end trampoline jumping
          (let [connections (reduce #(clojure.set/union %1 (get all %2)) #{} left-set)
                check-set (clojure.set/difference connections component)
                grow-component (clojure.set/union component left-set)]
            (println check-set)
            #(-recur-connected-lazy all check-set right grow-component)))))))

(def a-connected-lazy "Not eager b/c stops building component once A-B join is found in data."
  (fn [data-atom a b]
    (swap! stats start-record)
    (let [all (:all-connections @data-atom)
          ordered (sorted-set a b)
          left (first ordered)
          right (second ordered)
          component #{}
          result (if (and (contains? all left) (contains? all right))
                   (trampoline 
                    -recur-connected-lazy all #{left} right component)
                   false)]
      (swap! stats end-record)
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

(def a-maketestdata "e.g. xs=1000 ys=25 max=5000 - not sparse"
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

(def a-checksparse ":when some?"
  (fn [a-data]
    (a-check a-data some?)))

(def a-checkdense ":when nil?"
  (fn [a-data]
    (a-check a-data nil?)))
