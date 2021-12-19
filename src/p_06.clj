(ns aoc-2021
  (:require [clojure.math.combinatorics :as combo]))

(defn evolve-lanternfish [school]
  (let [num-new-fish (get school 0 0)
        num-sevens (get school 7 0)]
    (as-> school v
      (dissoc v 0)
      (map (fn [[key value]] [(dec key) value]) v)
      (into {} v)
      (assoc v 6 (+ num-sevens num-new-fish))
      (assoc v 8 num-new-fish))))

(with-open [rdr (clojure.java.io/reader "./data/p_06.txt")]
  (let [line (first (line-seq rdr))
        ages (mapv #(Integer/parseInt %) (.split line ","))
        school (frequencies ages)
        evolutions (iterate evolve-lanternfish school)
        num-days 80
        num-days 256
        final-evolved-school (nth evolutions num-days)]
    (println (apply + (vals final-evolved-school)))))
