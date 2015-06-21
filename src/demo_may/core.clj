(ns demo-may.core
  (:use [clojure.tools.cli :only (cli)])
  (:require [demo-may.algo1 :as a1])
  (:gen-class))

(defn echo-fn [line]
  (println line)
  (if (= "q" line)
    false
    true))

(defn- receive-fn [args]
  (let [[opts extra banner]
        (cli args
             ["-h" "--help" "Halp" :flag true :default false]
             )
        ]
    (when (:help opts)
      (println banner)
      (. System exit 0))
    (loop [lines (line-seq (java.io.BufferedReader. *in*))] 
      (if (echo-fn (first lines))
        (recur (drop 1 lines)))
      )
  ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (receive-fn args))

(comment
)
