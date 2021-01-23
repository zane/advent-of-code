(ns io.zane.advent-of-code
  (:require [clojure.string :as string]))

(defn out-of-bounds?
  [v x y]
  (or (neg? x)
      (neg? y)
      (>= y (count v))
      (>= x (count (nth v y)))))

(defn adjacent-indexes
  [v x y]
  (->> (for [x (range (dec x) (+ 2 x))
             y (range (dec y) (+ 2 y))]
         [x y])
       (remove #{[x y]})
       (remove #(apply out-of-bounds? v %))))

(defn adjacent-values
  [v x y]
  (map (fn [[x y]]
         (get-in v [y x]))
       (adjacent-indexes v x y)))

(defn next-seat
  [v x y]
  (let [adjacent-counts (frequencies (adjacent-values v x y))]
    (case (get-in v [y x])
      :floor
      :floor

      :empty
      (if-not (:occupied adjacent-counts)
        :occupied
        :empty)

      :occupied
      (if (>= (get adjacent-counts :occupied 0)
              4)
        :empty
        :occupied))))

(defn next-plane
  [v]
  (vec (map-indexed (fn [y _]
                      (vec (map-indexed (fn [x _]
                                          (next-seat v x y))
                                        (nth v y))))
                    v)))

(defn parse
  [s]
  (let [parse-line (fn [s]
                     (mapv #(case %
                              \. :floor
                              \# :occupied
                              \L :empty)
                           s))]
    (mapv parse-line (string/split-lines s))))

(defn unparse
  [plane]
  (let [unparse-seat (fn [c]
                       (case c
                         :floor \.
                         :occupied \#
                         :empty \L))
        unparse-row (fn [v]
                      (apply str (map unparse-seat v)))]
    (->> (map unparse-row plane)
         (string/join "\n"))))

(defn y
  [f x]
  (let [x' (f x)]
    (if (= x x')
      x
      (y f x'))))

(defn last-plane
  [v]
  (y next-plane v))

(comment
  (->> (slurp "/Users/zane/Desktop/input.txt")
       (parse)
       (last-plane)
       (into [] cat)
       (frequencies))
  )
