(ns aoc-2021)

(defn next-item [[_cur current-count] line]
  (let [str-bits (.split line "")
        int-bits (mapv #(Integer/parseInt %) str-bits)
        cur (if (nil? _cur) (vec (repeat (count int-bits) 0)) _cur)]
    [(mapv + cur int-bits) (inc current-count)]))

(defn bin-to-dec [bin-vec] (reduce #(+ %2 (* 2 %1)) (map #(if % 1 0) bin-vec)))

(with-open [rdr (clojure.java.io/reader "./data/p_03.txt")]
  (let [str-stream  (line-seq rdr)
        [cur current-count] (reduce next-item [nil 0] str-stream)
        half-count  (/ (float current-count) 2)
        epsilon     (mapv #(> % half-count) cur)
        gamma       (mapv #(< % half-count) cur)]
    (println (* (bin-to-dec epsilon) (bin-to-dec gamma)))))