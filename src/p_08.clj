(ns aoc-2021
  (:require [clojure.math.combinatorics :as combo]))

(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(def base (->>
           [#{:begin "a" "b" "c"     "e" "f" "g" :end}
            #{:begin         "c"         "f"     :end}
            #{:begin "a"     "c" "d" "e"     "g" :end}
            #{:begin "a"     "c" "d"     "f" "g" :end}
            #{:begin     "b" "c" "d"     "f"     :end}
            #{:begin "a" "b"     "d"     "f" "g" :end}
            #{:begin "a" "b"     "d" "e" "f" "g" :end}
            #{:begin "a"     "c"         "f"     :end}
            #{:begin "a" "b" "c" "d" "e" "f" "g" :end}
            #{:begin "a" "b" "c" "d"     "f" "g" :end}]
           (map #(disj % :begin :end))
           (map sort)
           (map #(clojure.string/join "" %))))

(defn get-digit [digit]
  (find-thing digit base))

(def base-as-set (set base))

(defn remap-digit [digit perm]
  (let [mapping (zipmap perm "abcdefg")]
    (->> digit
      (map mapping)
      (sort)
      (clojure.string/join ""))))

(defn parse-line [line]
  (let [[left right] (clojure.string/split line #"\|")
        proc (fn [segment]
               (as-> segment v
                 (.trim v)
                 (.split v " ")
                 (map set v)))]
    [(proc left) (proc right)]))

(defn solve [[left right]]
  (first
   (for [perm (combo/permutations "abcdefg")
         :let [remap-fn   #(remap-digit % perm)]
         :when (= (->> left (map remap-fn) (set))
                  base-as-set)]
     (->> right
          (map #(remap-digit % perm))
          (map get-digit)
          (map first)
          (reduce #(+ %2 (* 10 %1)))))))

(with-open [rdr (clojure.java.io/reader "./data/p_08.test.txt")]
  (let [lines (line-seq rdr)
        examples (map parse-line lines)
        rights (map second examples)

        ; part 1
        count-1478 (map #(->> % (map count) (filter #{2 4 3 7}) (count)) rights)

        ; part 2
        solution-stream (map solve examples)]
    (println (apply + solution-stream))))
