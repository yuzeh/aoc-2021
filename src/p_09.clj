(ns aoc-2021
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :refer [union]]))

(defn parse-int [v] (Integer/parseInt v))

(defn parse-grid [lines]
  (into []
        (for [line lines]
          (as-> line v
            (.split v "")
            (mapv parse-int v)))))

(defn neighbor-indexes [[i j]]
  [[(inc i) j]
   [(dec i) j]
   [i (inc j)]
   [i (dec j)]])

(defn is-low-point [grid idx]
  (let [cur (get-in grid idx)
        n-idx (neighbor-indexes idx)
        neighbors (map #(get-in grid %) n-idx)]
    (every? #(or (nil? %) (< cur %)) neighbors)))

(defn get-basin-size [grid low-point]
  (loop [basin #{low-point}
         new-points #{low-point}]
    (let [new-neighbors (for [pt new-points
                              neighbor-index (neighbor-indexes pt)
                              :when (and
                                     (not (contains? basin neighbor-index))
                                     (get-in grid neighbor-index)
                                     (< (get-in grid neighbor-index) 9))]
                          neighbor-index)
          new-neighbors-set (into #{} new-neighbors)]
      (if (empty? new-neighbors-set)
        (count basin)
        (recur (union basin new-neighbors-set)
               new-neighbors-set)))))

(with-open [rdr (clojure.java.io/reader "./data/p_09.txt")]
  (let [lines (line-seq rdr)
        grid (parse-grid lines)
        coord-stream (combo/cartesian-product (range (count lines)) (range (count (first lines))))
        low-points (filter #(is-low-point grid %) coord-stream)

        ; part 1
        risk-levels (map #(inc (get-in grid %)) low-points)

        ; part 2
        basin-sizes (map #(get-basin-size grid %) low-points)
        ordered-basin-sizes (reverse (sort basin-sizes))]
    (println (apply * (take 3 ordered-basin-sizes)))))
