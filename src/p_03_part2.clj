(ns aoc-2021)

(defn line-to-bit-vec [line]
  (as-> line v
    (.split v "")
    (mapv #(Integer/parseInt %) v)))

(defn recur-search-bit-vecs [bit-vecs find-least-common?]
  (loop [current-slice bit-vecs
         n             0]
    (if (= (count current-slice) 1)
      (first current-slice)
      (let [m                 (count current-slice)
            half-m            (/ m 2.)
            sorted            (sort current-slice)
            cut-idx           (- m (reduce + (map #(nth % n) current-slice)))
            more-common-value (condp apply [(float cut-idx) half-m]
                                > :zeros
                                < :ones
                                = :same)
            pick-zero?        (if find-least-common?
                                (#{:ones :same}  more-common-value)
                                (#{:zeros} more-common-value))
            lower             (if pick-zero? 0 cut-idx)
            upper             (if pick-zero? cut-idx m)
            new-slice         (subvec current-slice lower upper)]
        (recur new-slice (inc n))))))

(defn bin-to-dec [bin-vec] (reduce #(+ %2 (* 2 %1)) bin-vec))

(let [sorted-bit-vec-vec (with-open [rdr (clojure.java.io/reader "./data/p_03.txt")]
                           (->> rdr
                                line-seq
                                (map line-to-bit-vec)
                                sort
                                vec))
      o2  (recur-search-bit-vecs sorted-bit-vec-vec false)
      _ (println "o2" (bin-to-dec o2))
      co2 (recur-search-bit-vecs sorted-bit-vec-vec true)
      _ (println "co2" (bin-to-dec co2))
      value (* (bin-to-dec o2) (bin-to-dec co2))]
  (println value))