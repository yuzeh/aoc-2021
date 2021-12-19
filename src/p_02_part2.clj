(ns aoc-2021)

(defn next-coord [[hor dep aim] line]
  (let [[dir dist-str] (.split line " ")
        dist-int (bigint (Integer/parseInt dist-str))]
    (case dir
      "forward" [(+ hor dist-int) (+ dep (* aim dist-int)) aim]
      "down" [hor dep (+ aim dist-int)]
      "up" [hor dep (- aim dist-int)]
      (throw (Exception. (concat "Unknown direction: " dir))))))

(with-open [rdr (clojure.java.io/reader "./data/p_02.txt")]
  (let [str-stream (line-seq rdr)
        [hor dep aim] (reduce next-coord [0 0 0] str-stream)]
    (println (* hor dep))))