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

(def -recur-connected-mixedup
  (fn [all-conns left right]
    (println (str left "," right))
    (if (or (nil? left) (nil? right))
      (throw "Left or right is nil???")
      (if (-simple-connected all-conns left right)
        true
        (loop [left-conns (get all-conns left)]
          (if (or (nil? left-conns) (empty? left-conns))
            false
            (if (-recur-connected-mixedup 
                 all-conns (first left-conns) right)
              true
              (recur (rest left-conns)))))))))

(def -connected
  (fn [data-atom a b]
    (let [all-conns (:all-connections @data-atom)
          ordered (sorted-set a b)
          left (first ordered)
          right (second ordered)]
       ; (-simple-connected all-conns left right)
       (-recur-connected-mixedup all-conns left right))))

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

