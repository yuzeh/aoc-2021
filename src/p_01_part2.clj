(ns aoc-2021)

(with-open [rdr (clojure.java.io/reader "./data/p_01.txt")]
  (let [str-stream (line-seq rdr)
        int-stream (map #(Integer/parseInt %) str-stream)
        triple-stream (partition 3 1 int-stream)
        sum-triple-stream (map #(reduce + %) triple-stream)
        pair-triple-stream (partition 2 1 sum-triple-stream)]
    (println (count (filter (fn [[a b]] (< a b)) pair-triple-stream)))))