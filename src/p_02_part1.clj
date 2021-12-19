(ns aoc-2021)

(defn line-to-coords [line]
  (let [[dir dist-str] (.split line " ")
        dist-int (Integer/parseInt dist-str)]
    (case dir
      "forward" [dist-int 0]
      "down" [0 dist-int]
      "up" [0 (- dist-int)]
      (throw (Exception. (concat "Unknown direction: " dir))))))

(defn sum-xy [[a b] [c d]] [(+ a c) (+ b d)])

(with-open [rdr (clojure.java.io/reader "./data/p_02.txt")]
  (let [str-stream (line-seq rdr)
        coord-stream (map line-to-coords str-stream)
        final-coords (reduce sum-xy coord-stream)]
    (println (reduce * final-coords))))