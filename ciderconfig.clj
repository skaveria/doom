(require '[clojure.java.shell :refer [sh]])

(defn run [cmd]
  (println ">" cmd)
  (println (:out (sh "bash" "-c" cmd))))
