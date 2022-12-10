(ns adv.y2022)

(defn ex-cmd [cycles i]
  (let [last-x ((first cycles) 1)
        new-cycles (case (i 0)
                     "addx" [[last-x last-x]
                             [last-x (+ last-x (Integer/parseInt (i 1)))]]
                     "noop" [[last-x last-x]]
                     :else [])]
    (reduce conj cycles new-cycles)))

(def start-cycles (list [1 1]))

(defn make-cmds [lines]
  (->> (clojure.string/split lines #"\n")
       (map #(clojure.string/split % #"\s"))))

(def input1 "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop")

(def input "noop\nnoop\nnoop\naddx 3\naddx 7\nnoop\nnoop\nnoop\nnoop\naddx 6\nnoop\naddx -1\nnoop\naddx 5\naddx 1\nnoop\naddx 4\nnoop\nnoop\nnoop\nnoop\naddx 6\naddx -1\nnoop\naddx 3\naddx -13\naddx -22\nnoop\nnoop\naddx 3\naddx 2\naddx 11\naddx -4\naddx 11\naddx -10\naddx 2\naddx 5\naddx 2\naddx -2\nnoop\naddx 7\naddx 3\naddx -2\naddx 2\naddx 5\naddx 2\naddx -2\naddx -8\naddx -27\naddx 5\naddx 2\naddx 21\naddx -21\naddx 3\naddx 5\naddx 2\naddx -3\naddx 4\naddx 3\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\naddx 3\naddx 1\naddx 6\naddx -31\nnoop\naddx -4\nnoop\nnoop\nnoop\nnoop\naddx 3\naddx 7\nnoop\naddx -1\naddx 1\naddx 5\nnoop\naddx 1\nnoop\naddx 2\naddx -8\naddx 15\naddx 3\nnoop\naddx 2\naddx 5\nnoop\nnoop\nnoop\naddx -28\naddx 11\naddx -20\nnoop\naddx 7\naddx -2\naddx 7\nnoop\naddx -2\nnoop\naddx -6\naddx 11\nnoop\naddx 3\naddx 2\nnoop\nnoop\naddx 7\naddx 3\naddx -2\naddx 2\naddx 5\naddx 2\naddx -16\naddx -10\naddx -11\naddx 27\naddx -20\nnoop\naddx 2\naddx 3\naddx 5\nnoop\nnoop\nnoop\naddx 3\naddx -2\naddx 2\nnoop\naddx -14\naddx 21\nnoop\naddx -6\naddx 12\nnoop\naddx -21\naddx 24\naddx 2\nnoop\nnoop\nnoop")

(defn part1 [input]
  (let [cycles (->> input
                    make-cmds
                    (reduce ex-cmd start-cycles)
                    reverse
                    (apply vector))]
    (reduce +
            (map #(* % ((cycles %) 0))
                 [20 60 100 140 180 220]))))

(part1 input1)
(part1 input)

(defn part2 [input]
  (let [cycles (->> input
                    make-cmds
                    (reduce ex-cmd start-cycles)
                    reverse
                    (apply vector))]
    (map (fn [r]
           (clojure.string/join
             ""
             (map-indexed
               (fn [pos cycle]
                 (let [x ((cycles cycle) 0)
                       rng [(dec x) x (inc x)]]
                   (if (some #(= pos %) rng)
                     "#"
                     ".")))
               (range r (+ 40 r)))))
         [1 41 81 121 161 201])))

(part2 input1)
(part2 input)
