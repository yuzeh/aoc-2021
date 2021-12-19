(ns aoc-2021)

(with-open [rdr (clojure.java.io/reader "./data/p_01.txt")]
  (let [str-stream (line-seq rdr)
        int-stream (map #(Integer/parseInt %) str-stream)
        pair-stream (partition 2 1 int-stream)]
    (println (count (filter (fn [[a b]] (< a b)) pair-stream)))))