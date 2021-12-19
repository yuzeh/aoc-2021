(ns aoc-2021)

(defn gen-winning-indices [n]
  (let [idxes-1d (vec (range n))
        zip-combine (fn [as bs] (mapv #(+ (* n %1) %2) as bs))
        horizontals (map #(zip-combine %1 (repeat %2)) (repeat idxes-1d) idxes-1d)
        verticals (map #(zip-combine (repeat %2) %1) (repeat idxes-1d) idxes-1d)]
    (vec (concat horizontals verticals))))

(def winning-indices-5x5 (gen-winning-indices 5))

(defn check-bingo-board [board called-numbers]
  (let [called-numbers-set (set called-numbers)
        matches (mapv called-numbers-set board)
        all-set (fn [idx-set] (every? #(nth matches %) idx-set))]
    (some all-set winning-indices-5x5)))

(defn read-boards [lines]
  (let [boards-and-blanks (partition-by #(re-matches #"\s*" %) lines)
        boards-stream (filter #(> (count %) 1) boards-and-blanks)
        parse-board (fn [board-lines]
                      (as-> board-lines v
                        (clojure.string/join " " v)
                        (clojure.string/trim v)
                        (clojure.string/split v #"\s+")
                        (mapv #(Integer/parseInt %) v)))]
    (mapv parse-board boards-stream)))

(with-open [rdr (clojure.java.io/reader "./data/p_04.txt")]
  (let [lines (line-seq rdr)
        number-list (as-> (first lines) v
                      (.split v ",")
                      (mapv #(Integer/parseInt %) v))
        n (count number-list)
        boards (read-boards (rest lines))
        checks (for [i (range n)
                     :let [nums (take (inc i) number-list)
                           winner (->> boards
                                       (filter #(check-bingo-board % nums))
                                       first)]
                     :when (some? winner)]
                 [nums winner])
        [numbers winner] (first checks)
        numbers-set (set numbers)
        unmarked-numbers-sum (apply + (remove numbers-set winner))]
    (println (* unmarked-numbers-sum (last numbers)))))