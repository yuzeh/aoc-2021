(ns aoc-2021
  (:require [clojure.math.combinatorics :as combo]))

(defn median [nums]
  (let [sorted (vec (sort nums))
        half-length (/ (count sorted) 2)]
    (nth sorted half-length)))

(defn mean [nums]
  (/ (float (apply + nums)) (count nums)))

(with-open [rdr (clojure.java.io/reader "./data/p_07.txt")]
  (let [line (first (line-seq rdr))
        positions (mapv #(Integer/parseInt %) (.split line ","))

        ; part 1
        err-func median
        fuel-func (fn [target] (apply + (map #(Math/abs (- % target)) positions)))

        ; part 2
        err-func mean
        fuel-func (fn [target]
                    (apply + (->> positions
                                  (map #(Math/abs (- % target)))
                                  (map #(/ (* % (inc %)) 2)))))

        target (err-func positions)
        lower (int (Math/floor target))
        upper (int (Math/ceil target))
        fuel-lower (fuel-func lower)
        fuel-upper (fuel-func upper)]
    (println (min fuel-lower fuel-upper))))
