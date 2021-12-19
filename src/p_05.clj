(ns aoc-2021
  (:require [clojure.math.combinatorics :as combo]))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn fill-grid [[x1 y1 x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        d-gcd (gcd (Math/abs dx) (Math/abs dy))
        dx-step (/ dx d-gcd)
        dy-step (/ dy d-gcd)
        offsets (for [i (range d-gcd)]
                  [(+ x1 (* dx-step (inc i)))
                   (+ y1 (* dy-step (inc i)))])]
    (apply vector [x1 y1] offsets)))

(defn parse-line-segment [line]
  (as-> line v
    (.split v "->")
    (mapv #(vec (.split % ",")) v)
    (flatten v)
    (mapv #(.trim %) v)
    (mapv #(Integer/parseInt %) v)))

(defn axis-aligned? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(with-open [rdr (clojure.java.io/reader "./data/p_05.txt")]
  (let [str-lines (line-seq rdr)
        lines (mapv parse-line-segment str-lines)

        ; Remove this filter for part 1
        ; filtered-lines (filter axis-aligned? lines)
        filtered-lines lines

        grid-pts (apply concat (map fill-grid filtered-lines))
        intersections (->> grid-pts
                           (group-by identity)
                           (filter #(> (count (second %)) 1))
                           (map first)
                           (set))
        ; pairs (combo/combinations filtered-lines 2)
        ; intersections (->> pairs
        ;                    (map (partial apply line-segment-intersect))
        ;                    (#(do (println %) %))
        ;                    (filter some?)
        ;                    set)
        ]
    (println (count intersections))))
