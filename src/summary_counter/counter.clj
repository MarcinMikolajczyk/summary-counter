(ns summary-counter.counter
  (:require [clojure.data.csv :refer :all]))

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(defn create-csv-seq [fileName]
  (seq (with-open [reader (io/reader fileName)]
         (doall (read-csv reader)))))

(defn extract-json [line]
  (try (subs line
             (inc (str/index-of line "{"))
             (str/index-of line "}"))
       (catch NullPointerException e (println "Cannot find json data in provided:" line))))

(defn get-comments-from-all-data [dataSeq]
  (def comments (atom (seq [])))
  (doseq [line dataSeq] (swap! comments (fn [x] (cons (last line) x))))
  comments)

(defn create-vector-list-with-task-hours [fileName]
  (map (fn [taskVector] (map str/trim taskVector))
       (map (fn [x] (str/split x #","))
            (map (fn [x] (str/replace x #"\n" ""))
                 (filter (fn [x] (not (nil? x)))
                         (map extract-json
                              @(get-comments-from-all-data (create-csv-seq fileName))))))))

(defn flat [sequence]
  (def flatted-seq (atom (seq [])))
  (doseq [arr sequence] (doseq [a arr]
                          (swap! flatted-seq (fn [x] (cons a x)))))
  @flatted-seq)

(defn create-task-list-from-file [fileName]
  (flat (map (fn [comment] (map (fn [x] (map str/trim (str/split x #":"))) comment))
             (create-vector-list-with-task-hours fileName))))

(defn sum-hours [sequence]
  (def sum (atom 0.0))
  (doseq [x sequence] (swap! sum (fn [s] (+ s (Float/parseFloat (last x))))))
  @sum)

(defn calculate-hours-task [taskList]
  (map (fn [x] (do
                 (def taskId (first x))
                 (def hours (sum-hours (last x)))
                 [taskId hours]))
       (group-by (fn [x] (first x)) taskList))
  )

(defn -main
  "Function to calculate task hours from summary file"
  [fileName]

  (def taskHours (calculate-hours-task
                   (create-task-list-from-file fileName)))

  (doseq [task taskHours] (println "Task id:" (first task) "work:" (last task)))

  (println "Hours sum:" (do (def sum (atom 0.0))
                            (doseq [task taskHours]
                              (swap! sum (fn [x] (+ x (last task))) ))
                            @sum)))
