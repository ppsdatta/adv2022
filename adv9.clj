(ns adv.y2022)

(def start-grid {:h       [0 0]
                 :t       [0 0]
                 :visited [[0 0]]})

(def start-grid-9 {:h       [0 0]
                   :ts      [[0 0] [0 0] [0 0]
                             [0 0] [0 0] [0 0]
                             [0 0] [0 0] [0 0]]
                   :visited [[0 0]]})

(defn move [grid dir]
  (let [[x y] (:h grid)]
    (case dir
      "R" (assoc grid :h [(inc x) y])
      "L" (assoc grid :h [(dec x) y])
      "U" (assoc grid :h [x (inc y)])
      "D" (assoc grid :h [x (dec y)]))))

(defn ht-adjacent?
  ([grid]
   (let [[hx hy] (:h grid)
         [tx ty] (:t grid)]
     (or
       (and (= hx tx) (= hy ty))
       (and (= hx tx) (= (Math/abs (- hy ty)) 1))
       (and (= hy ty) (= (Math/abs (- hx tx)) 1))
       (and (= (Math/abs (- hy ty)) 1) (= (Math/abs (- hx tx)) 1)))))
  ([[hx hy] [tx ty]]
   (or
     (and (= hx tx) (= hy ty))
     (and (= hx tx) (= (Math/abs (- hy ty)) 1))
     (and (= hy ty) (= (Math/abs (- hx tx)) 1))
     (and (= (Math/abs (- hy ty)) 1) (= (Math/abs (- hx tx)) 1)))))

(defn calculate-tail
  [[hx hy]
   [tx ty]]
  (cond
    (= hx tx) (if (> hy ty)
                [tx (inc ty)]
                [tx (dec ty)])
    (= hy ty) (if (> hx tx)
                [(inc tx) ty]
                [(dec tx) ty])
    (> hx tx) (if (> hy ty)
                [(inc tx) (inc ty)]
                [(inc tx) (dec ty)])
    (< hx tx) (if (> hy ty)
                [(dec tx) (inc ty)]
                [(dec tx) (dec ty)])
    :else [tx ty]))

(defn adjust-tail [grid]
  (if (ht-adjacent? grid)
    grid
    (let [head (:h grid)
          tail (:t grid)
          [ntx nty] (calculate-tail head tail)]
      (assoc grid :t [ntx nty]
                  :visited (conj (:visited grid) [ntx nty])))))

(defn adjust-9-tails [grid]
  (let [tails (:ts grid)
        head (:h grid)
        ntails (->> tails
                    (reduce (fn [tails point]
                              (if (ht-adjacent? (first tails) point)
                                (conj tails point)
                                (conj tails
                                      (calculate-tail (first tails) point))))
                            (list head))
                    (drop-last)
                    (reverse)
                    (apply vector))]
    (assoc
      grid
      :ts ntails
      :visited (conj (:visited grid)
                     (ntails 8)))))

(defn move-n [grid dir n]
  (->> (range 1 (inc n))
       (reduce (fn [g _] (let [g (adjust-tail (move g dir))]
                           ;(println g)
                           g))
               grid)))

(defn move-n-9 [grid dir n]
  (->> (range 1 (inc n))
       (reduce (fn [g _]
                 (let [g (adjust-9-tails (move g dir))]
                   ;(println g)
                   g))
               grid)))

(defn parse-apply [lines]
  (->> (clojure.string/split lines #"\n")
       (map #(clojure.string/split % #" "))
       (map (fn [[dir len]]
              [dir
               (Integer/parseInt len)]))
       (reduce (fn [grid [d n]]
                 (move-n grid d n))
               start-grid)))

(defn parse-apply-9 [lines]
  (->> (clojure.string/split lines #"\n")
       (map #(clojure.string/split % #" "))
       (map (fn [[dir len]]
              [dir
               (Integer/parseInt len)]))
       (reduce (fn [grid [d n]]
                 (move-n-9 grid d n))
               start-grid-9)))

(def input1
  "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(def input2
  "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20")

(def input
  "U 1\nD 1\nR 1\nL 2\nD 2\nU 1\nR 2\nD 2\nU 1\nR 1\nL 1\nD 2\nR 2\nU 2\nL 1\nD 1\nU 1\nL 1\nD 2\nU 1\nD 1\nL 1\nU 2\nR 1\nL 1\nR 1\nL 1\nU 2\nL 2\nU 2\nL 1\nU 2\nD 1\nU 2\nD 1\nL 2\nU 1\nD 1\nU 2\nR 2\nD 2\nR 1\nD 2\nL 1\nU 1\nL 2\nD 2\nL 2\nR 1\nU 1\nL 2\nD 1\nU 1\nR 2\nL 2\nD 1\nR 1\nL 1\nR 2\nU 1\nR 2\nD 1\nU 1\nL 2\nD 1\nR 2\nD 1\nU 2\nR 1\nD 1\nR 2\nU 1\nL 2\nD 2\nU 2\nR 2\nL 1\nD 2\nR 1\nD 1\nU 2\nD 2\nL 2\nR 2\nL 2\nD 1\nR 2\nD 2\nL 1\nU 1\nD 1\nL 2\nU 1\nR 1\nD 1\nU 2\nD 1\nL 2\nD 2\nR 2\nL 2\nR 2\nU 2\nL 1\nR 1\nL 1\nD 2\nR 2\nU 1\nR 1\nD 2\nR 1\nL 1\nR 3\nL 3\nD 1\nR 2\nD 2\nU 1\nD 1\nR 2\nD 1\nL 3\nU 3\nL 1\nU 3\nL 1\nD 2\nR 3\nL 2\nR 1\nU 3\nR 2\nD 3\nL 2\nD 1\nR 1\nL 2\nU 3\nD 2\nU 3\nR 1\nL 2\nD 2\nR 2\nU 3\nD 1\nL 2\nD 1\nR 1\nD 3\nU 1\nR 1\nD 3\nL 1\nD 3\nU 1\nL 3\nR 1\nD 1\nL 1\nU 3\nR 2\nD 1\nU 3\nL 1\nD 1\nL 3\nR 2\nD 1\nL 2\nU 1\nL 2\nU 1\nR 1\nL 2\nD 2\nR 2\nL 1\nR 2\nU 3\nD 3\nR 3\nL 3\nR 1\nL 2\nD 1\nU 1\nR 2\nD 2\nR 2\nD 1\nU 1\nD 3\nU 2\nR 3\nU 3\nR 3\nU 2\nR 1\nL 2\nD 1\nR 2\nU 1\nR 2\nD 2\nR 3\nL 2\nR 1\nD 2\nL 3\nU 3\nR 1\nD 1\nL 1\nR 1\nD 1\nR 3\nU 2\nD 3\nU 3\nR 2\nD 4\nL 3\nU 3\nL 2\nR 4\nU 1\nD 4\nR 1\nU 3\nL 1\nD 4\nL 3\nR 1\nD 3\nR 1\nL 2\nR 3\nL 3\nR 1\nD 1\nR 4\nL 3\nR 2\nL 1\nR 4\nU 3\nD 2\nR 3\nD 1\nR 1\nL 3\nR 4\nL 4\nU 3\nL 4\nR 4\nU 2\nD 2\nR 3\nL 4\nU 4\nL 3\nD 4\nR 1\nU 1\nL 3\nU 2\nL 2\nR 4\nD 3\nR 2\nU 2\nR 4\nD 1\nR 4\nU 3\nD 3\nR 1\nU 4\nL 1\nR 3\nD 3\nU 3\nL 4\nU 4\nD 4\nL 3\nD 4\nU 4\nR 2\nD 2\nU 3\nD 2\nU 4\nD 2\nL 2\nD 2\nR 2\nD 4\nL 2\nU 3\nD 1\nU 1\nR 2\nU 3\nL 4\nU 1\nR 3\nU 3\nD 3\nU 4\nD 2\nR 3\nL 4\nR 4\nD 3\nU 2\nR 4\nD 1\nR 4\nU 3\nR 4\nL 4\nD 3\nL 3\nD 3\nU 4\nR 4\nD 2\nR 2\nD 1\nR 4\nL 3\nD 5\nL 1\nD 3\nL 4\nR 2\nU 2\nL 3\nR 3\nU 5\nD 1\nU 1\nR 5\nD 5\nL 1\nR 4\nL 4\nR 4\nD 4\nR 4\nD 3\nL 4\nU 2\nL 2\nD 5\nU 5\nL 1\nR 2\nD 2\nR 2\nD 2\nU 2\nR 5\nU 2\nR 2\nU 4\nL 2\nU 3\nR 5\nU 2\nR 2\nD 2\nR 4\nU 5\nD 5\nR 3\nU 4\nL 5\nU 3\nD 2\nR 5\nU 3\nL 5\nU 2\nR 1\nL 1\nR 3\nL 2\nR 5\nL 5\nU 1\nR 2\nL 2\nD 1\nR 3\nL 5\nD 5\nU 4\nR 4\nL 3\nR 2\nD 2\nL 3\nD 3\nR 4\nU 2\nR 5\nD 1\nR 1\nD 3\nL 4\nR 3\nD 4\nL 4\nD 5\nR 3\nL 1\nU 1\nD 5\nU 2\nL 5\nD 3\nU 4\nD 5\nU 1\nR 2\nD 5\nL 2\nU 1\nD 4\nR 3\nD 2\nU 5\nL 4\nR 2\nD 4\nU 4\nR 4\nD 5\nR 2\nL 2\nR 5\nU 5\nR 6\nD 6\nU 4\nL 6\nD 4\nR 1\nL 1\nD 6\nR 4\nL 1\nU 4\nD 3\nU 3\nD 2\nU 6\nR 1\nU 4\nL 6\nR 5\nD 4\nR 6\nD 3\nR 1\nL 5\nD 3\nR 3\nD 6\nR 6\nD 2\nR 6\nD 1\nL 1\nR 5\nU 1\nD 5\nL 6\nR 4\nU 3\nL 2\nD 4\nU 1\nD 2\nR 4\nL 6\nD 6\nU 5\nD 2\nR 4\nL 4\nU 3\nD 1\nR 3\nL 1\nD 1\nL 1\nD 4\nL 6\nU 1\nL 2\nD 6\nR 1\nL 5\nU 6\nL 1\nD 2\nL 4\nR 3\nD 3\nR 6\nL 6\nR 5\nU 2\nR 4\nD 4\nU 5\nD 1\nU 5\nR 4\nL 3\nR 6\nU 4\nL 3\nR 3\nL 3\nU 6\nD 3\nL 3\nR 1\nL 5\nR 1\nD 2\nL 6\nR 6\nD 5\nR 6\nU 3\nR 3\nL 5\nD 6\nR 2\nD 3\nR 4\nD 1\nR 4\nD 5\nU 4\nD 1\nL 2\nD 7\nL 7\nD 3\nU 4\nR 1\nD 4\nR 3\nL 1\nU 2\nL 1\nD 1\nR 4\nD 7\nU 5\nR 5\nU 1\nL 3\nR 3\nL 4\nR 2\nU 3\nR 3\nU 2\nL 4\nR 3\nU 2\nL 1\nD 4\nU 7\nL 5\nD 7\nL 2\nD 6\nR 1\nD 4\nR 2\nD 7\nL 5\nD 4\nR 3\nL 1\nU 5\nD 1\nU 7\nL 3\nU 5\nL 3\nU 2\nR 2\nL 7\nU 1\nR 5\nU 3\nL 2\nD 7\nL 2\nR 5\nD 3\nL 7\nD 7\nU 6\nR 6\nL 5\nU 2\nD 7\nL 5\nR 5\nD 4\nR 5\nL 7\nD 1\nU 7\nR 2\nL 3\nR 6\nL 3\nR 6\nL 2\nR 5\nL 2\nR 2\nD 7\nU 4\nR 7\nU 2\nD 5\nR 4\nU 4\nR 1\nL 4\nR 1\nU 1\nL 1\nU 7\nR 1\nL 7\nD 3\nL 6\nU 6\nL 6\nU 2\nD 7\nL 7\nR 1\nD 5\nU 4\nL 5\nU 7\nL 6\nU 3\nR 5\nL 5\nD 5\nL 8\nR 8\nL 5\nD 2\nL 2\nR 7\nD 6\nL 2\nU 7\nR 6\nD 3\nU 2\nL 4\nD 5\nR 4\nD 6\nR 4\nD 3\nR 3\nL 5\nR 5\nL 7\nR 6\nD 5\nU 7\nR 1\nU 1\nR 1\nL 6\nR 6\nL 3\nU 2\nD 6\nU 4\nR 5\nU 4\nR 4\nD 4\nU 1\nL 1\nD 4\nU 4\nL 3\nR 1\nU 1\nR 8\nU 8\nD 2\nR 7\nU 8\nR 8\nU 3\nD 5\nU 2\nR 3\nD 5\nR 4\nL 6\nU 3\nL 4\nD 6\nU 2\nL 2\nD 5\nU 1\nR 2\nD 7\nL 4\nR 6\nL 4\nD 5\nL 3\nR 7\nU 5\nD 3\nL 5\nD 8\nL 8\nU 7\nD 8\nR 6\nU 5\nR 2\nU 5\nD 5\nU 7\nR 3\nL 6\nD 1\nU 6\nL 3\nR 5\nU 5\nR 2\nL 4\nU 8\nD 8\nL 7\nR 1\nU 8\nL 4\nU 1\nD 1\nR 3\nD 8\nL 4\nU 7\nL 5\nD 2\nU 6\nD 1\nL 5\nR 1\nU 6\nR 1\nD 7\nU 3\nL 7\nR 9\nD 5\nL 5\nU 5\nR 7\nU 3\nL 7\nR 3\nU 3\nL 3\nU 5\nD 8\nR 4\nL 1\nU 7\nD 3\nL 7\nU 6\nD 4\nU 7\nD 6\nR 9\nL 8\nR 4\nL 7\nU 7\nR 7\nL 4\nU 1\nL 5\nD 5\nL 6\nU 6\nD 6\nR 7\nL 3\nR 9\nU 2\nR 6\nD 8\nL 3\nR 6\nL 7\nD 2\nU 9\nD 2\nL 6\nD 9\nR 6\nD 1\nU 7\nL 8\nU 5\nL 9\nR 7\nD 9\nL 7\nD 9\nU 6\nR 5\nL 7\nD 8\nR 4\nL 4\nU 5\nD 3\nR 2\nD 3\nR 1\nL 3\nR 8\nD 6\nU 3\nD 3\nU 4\nR 6\nU 2\nR 3\nU 5\nR 7\nD 1\nU 9\nD 4\nU 5\nD 9\nR 4\nU 2\nD 5\nU 2\nR 8\nU 9\nL 9\nR 3\nL 6\nD 4\nL 2\nD 6\nU 8\nR 3\nL 2\nU 1\nL 3\nR 4\nL 1\nD 1\nU 5\nR 9\nD 10\nU 4\nL 1\nU 10\nD 2\nL 9\nR 10\nU 8\nL 2\nU 10\nR 2\nL 2\nD 10\nR 10\nU 9\nR 6\nL 9\nD 2\nU 6\nR 8\nU 9\nL 7\nR 5\nU 2\nD 8\nR 4\nD 5\nU 7\nD 2\nR 2\nU 2\nD 1\nR 1\nD 9\nL 10\nR 3\nD 5\nR 8\nU 5\nR 1\nU 3\nL 3\nU 7\nR 10\nU 4\nD 7\nU 3\nL 3\nR 6\nU 6\nD 7\nR 3\nU 2\nD 9\nR 7\nD 1\nL 3\nD 6\nL 4\nD 5\nL 3\nU 7\nR 2\nD 8\nU 1\nL 8\nR 4\nD 8\nL 2\nD 6\nL 9\nR 9\nU 1\nR 10\nL 3\nU 8\nD 9\nR 8\nD 8\nU 2\nD 8\nL 4\nU 2\nD 3\nL 4\nR 6\nL 4\nD 4\nL 6\nU 1\nR 1\nD 4\nL 7\nR 8\nU 8\nL 9\nD 5\nR 1\nU 2\nD 8\nU 8\nL 10\nD 9\nL 1\nR 4\nU 10\nR 7\nD 1\nU 9\nL 5\nR 3\nU 2\nR 3\nL 3\nU 7\nR 8\nL 10\nR 4\nD 5\nU 3\nL 10\nR 4\nU 7\nL 8\nU 4\nL 2\nR 10\nD 11\nL 10\nD 10\nL 11\nR 9\nD 9\nR 5\nL 5\nD 10\nR 11\nU 1\nR 11\nU 5\nR 9\nU 8\nL 1\nR 7\nU 7\nD 5\nR 5\nL 6\nR 8\nL 10\nR 9\nU 6\nD 9\nL 7\nU 7\nL 9\nR 5\nD 8\nR 8\nU 6\nR 11\nL 7\nD 9\nR 6\nL 5\nR 9\nU 8\nL 3\nU 9\nL 2\nR 9\nL 8\nR 1\nL 9\nR 4\nL 2\nU 1\nR 8\nL 8\nD 2\nL 8\nR 3\nD 8\nL 1\nR 11\nL 3\nR 8\nD 3\nL 10\nR 10\nD 5\nR 9\nD 4\nU 3\nL 6\nR 2\nU 10\nD 7\nU 8\nL 4\nD 7\nU 4\nD 7\nL 7\nR 9\nL 1\nR 6\nD 10\nU 4\nD 11\nU 7\nR 4\nU 10\nL 7\nR 9\nD 11\nU 11\nR 6\nU 9\nL 5\nD 10\nR 9\nL 10\nR 1\nD 4\nR 2\nL 4\nU 5\nL 4\nD 11\nU 8\nR 12\nL 10\nD 7\nR 4\nU 9\nR 5\nD 8\nU 9\nL 3\nR 3\nL 3\nU 11\nL 3\nD 9\nU 2\nR 3\nD 2\nL 4\nU 7\nR 10\nU 2\nD 10\nU 7\nL 1\nU 10\nL 1\nD 10\nU 12\nR 5\nL 4\nR 10\nU 6\nD 2\nR 6\nU 11\nD 1\nU 2\nL 1\nD 12\nR 2\nU 3\nD 7\nU 2\nL 9\nD 7\nR 3\nU 5\nR 10\nU 7\nD 5\nL 6\nU 11\nD 11\nL 1\nD 10\nR 1\nL 5\nU 10\nL 11\nR 10\nU 2\nD 7\nU 10\nL 10\nU 11\nD 7\nR 6\nD 3\nL 11\nU 4\nR 8\nU 6\nD 2\nU 8\nR 3\nL 7\nR 5\nU 8\nD 3\nL 12\nU 12\nD 10\nU 7\nL 10\nR 8\nD 8\nU 11\nR 3\nU 10\nR 6\nD 2\nU 3\nD 12\nL 5\nU 11\nR 9\nL 4\nU 6\nR 11\nD 1\nL 6\nU 5\nD 12\nL 9\nU 6\nL 13\nD 7\nL 10\nU 13\nL 11\nU 6\nD 11\nU 11\nL 6\nU 5\nD 2\nL 10\nD 5\nR 6\nD 13\nR 9\nD 4\nL 2\nR 11\nU 13\nL 3\nR 10\nU 2\nL 13\nU 10\nD 4\nL 2\nR 5\nL 8\nU 4\nD 1\nU 3\nD 10\nU 13\nD 2\nL 11\nR 7\nL 5\nR 7\nD 10\nR 13\nD 6\nR 11\nL 11\nR 9\nL 11\nD 13\nU 3\nL 6\nU 7\nL 12\nD 1\nR 7\nL 7\nU 1\nD 6\nR 6\nU 11\nD 2\nL 10\nR 5\nL 6\nR 6\nU 2\nR 11\nD 10\nU 9\nL 4\nU 7\nR 3\nU 9\nR 11\nU 6\nL 5\nD 1\nU 13\nL 11\nD 1\nL 9\nR 8\nD 4\nU 4\nL 1\nR 1\nL 6\nR 11\nD 10\nR 3\nU 2\nL 3\nR 1\nU 10\nR 3\nU 2\nL 1\nD 5\nU 11\nR 11\nU 11\nL 3\nU 3\nR 3\nL 3\nU 10\nL 14\nU 14\nD 6\nL 11\nR 6\nU 8\nL 12\nR 11\nU 1\nL 9\nD 11\nU 8\nD 6\nR 2\nL 12\nR 11\nD 2\nR 3\nD 5\nL 4\nU 2\nD 12\nU 12\nL 5\nR 2\nU 13\nR 1\nL 2\nD 13\nR 8\nL 14\nU 14\nL 9\nR 10\nU 5\nD 3\nU 12\nD 13\nR 2\nD 8\nR 10\nL 6\nU 12\nD 12\nR 6\nD 12\nR 13\nL 5\nR 10\nD 10\nL 8\nR 5\nL 2\nU 6\nD 2\nU 9\nL 1\nD 8\nL 7\nD 10\nL 6\nR 1\nL 6\nD 14\nU 6\nR 10\nD 4\nR 2\nD 13\nU 9\nR 11\nU 8\nR 4\nL 6\nR 14\nL 10\nU 14\nD 12\nR 3\nD 12\nL 8\nU 13\nR 4\nL 3\nR 14\nU 11\nD 13\nL 8\nU 1\nD 2\nL 11\nR 7\nD 5\nR 11\nL 1\nD 13\nL 7\nR 4\nU 7\nD 13\nL 2\nR 8\nD 1\nR 13\nL 9\nD 4\nR 12\nD 10\nU 12\nD 7\nU 5\nL 3\nU 9\nD 12\nR 8\nD 13\nR 8\nD 5\nL 14\nU 7\nD 4\nR 14\nD 14\nR 2\nD 5\nL 2\nD 13\nU 7\nL 8\nR 10\nD 9\nU 11\nD 3\nU 3\nD 2\nL 9\nD 2\nU 10\nD 7\nL 5\nR 10\nD 15\nR 1\nU 7\nD 10\nU 14\nR 12\nL 14\nU 7\nR 5\nU 2\nL 3\nR 13\nU 12\nD 1\nL 15\nD 10\nR 6\nU 9\nR 13\nU 3\nD 13\nU 1\nR 4\nL 1\nR 8\nU 14\nL 1\nR 10\nL 12\nD 15\nR 6\nU 13\nL 2\nR 13\nD 9\nU 3\nR 4\nD 11\nU 6\nD 9\nL 12\nR 1\nD 11\nR 11\nU 11\nR 5\nD 2\nL 3\nR 12\nL 8\nR 1\nL 12\nU 9\nL 3\nU 7\nD 2\nU 9\nL 10\nD 1\nU 7\nR 9\nD 11\nL 8\nD 4\nR 2\nU 12\nD 12\nU 6\nR 5\nD 2\nR 1\nD 5\nU 6\nL 10\nD 13\nR 5\nU 12\nL 4\nU 8\nL 12\nU 13\nL 1\nD 4\nL 9\nU 3\nD 15\nU 3\nD 15\nR 1\nD 10\nU 11\nR 7\nU 3\nR 13\nD 3\nU 3\nR 9\nL 12\nD 1\nU 5\nD 1\nR 9\nD 5\nU 15\nD 9\nL 10\nR 6\nL 4\nD 6\nR 5\nU 15\nR 9\nU 16\nD 3\nR 9\nU 11\nD 16\nU 7\nL 1\nU 8\nR 12\nL 1\nR 6\nD 9\nU 11\nL 5\nD 5\nR 14\nD 1\nL 3\nR 8\nL 3\nD 8\nU 11\nR 12\nU 10\nL 3\nU 10\nL 1\nR 9\nL 8\nD 2\nR 5\nU 3\nR 2\nD 9\nR 3\nD 12\nU 5\nD 13\nL 7\nR 15\nL 9\nU 9\nD 7\nR 6\nD 9\nL 7\nR 11\nL 10\nR 9\nU 13\nL 11\nU 10\nL 2\nD 13\nL 13\nR 15\nD 6\nR 15\nU 10\nR 12\nD 8\nU 16\nL 6\nR 9\nU 12\nD 4\nR 6\nU 7\nL 11\nU 12\nL 5\nR 11\nL 16\nR 3\nU 5\nL 14\nR 12\nL 13\nD 1\nL 9\nD 8\nL 6\nR 6\nD 15\nL 14\nR 17\nU 12\nD 12\nR 15\nD 2\nL 11\nR 9\nU 1\nR 5\nU 17\nR 3\nD 16\nU 6\nD 16\nR 8\nL 10\nU 13\nL 9\nD 12\nR 1\nU 4\nR 8\nD 11\nR 15\nD 15\nL 4\nD 10\nL 6\nU 15\nL 1\nR 1\nD 16\nU 7\nR 13\nD 6\nL 14\nR 15\nU 5\nR 4\nU 7\nR 6\nU 17\nL 12\nU 6\nL 5\nR 16\nL 10\nD 6\nR 7\nU 15\nR 3\nU 3\nD 16\nL 8\nR 13\nL 1\nU 2\nR 5\nD 17\nU 10\nL 16\nU 2\nL 3\nD 2\nR 10\nL 6\nR 9\nU 3\nL 5\nU 2\nD 2\nU 4\nD 10\nR 9\nU 8\nL 9\nD 16\nR 3\nU 3\nL 15\nD 11\nR 1\nU 11\nR 11\nL 12\nR 11\nD 17\nR 4\nD 14\nR 10\nU 4\nR 5\nD 12\nU 16\nD 3\nR 1\nL 15\nU 11\nR 1\nD 3\nL 9\nD 3\nL 7\nU 5\nR 12\nU 10\nR 16\nU 12\nD 9\nR 10\nU 2\nR 3\nD 15\nR 14\nL 11\nD 18\nR 12\nD 6\nU 5\nL 8\nU 7\nD 15\nR 8\nD 15\nL 7\nR 18\nL 5\nR 8\nL 10\nD 12\nR 14\nU 2\nL 7\nR 17\nU 4\nL 1\nU 10\nD 18\nL 3\nU 5\nD 12\nL 4\nR 14\nU 5\nL 18\nU 18\nD 14\nR 11\nU 6\nL 4\nR 13\nD 18\nU 18\nR 18\nU 7\nD 4\nL 17\nD 2\nU 11\nL 13\nD 13\nL 14\nU 2\nR 6\nL 3\nD 1\nL 8\nU 3\nR 5\nD 7\nL 2\nD 4\nU 1\nR 1\nU 8\nD 2\nU 3\nR 2\nD 15\nU 1\nR 9\nL 2\nR 2\nU 3\nL 16\nU 16\nR 16\nL 11\nU 9\nD 10\nR 18\nU 14\nD 7\nR 16\nD 7\nR 7\nL 3\nU 17\nR 18\nU 10\nL 15\nR 3\nU 6\nR 1\nU 1\nR 14\nD 12\nR 10\nL 7\nU 11\nD 15\nU 9\nL 11\nU 13\nD 16\nR 8\nU 5\nL 5\nU 10\nR 5\nL 15\nD 6\nL 16\nD 4\nL 19\nR 8\nU 18\nL 7\nD 15\nR 5\nU 9\nL 2\nU 17\nL 2\nU 17\nR 2\nL 13\nD 19\nL 16\nU 6\nD 13\nR 15\nD 13\nU 7\nL 7\nR 8\nD 10\nL 13\nD 4\nL 5\nU 8\nL 12\nR 13\nU 5\nR 9\nU 16\nL 5\nD 14\nL 4\nR 8\nU 4\nL 10\nR 17\nD 12\nL 14\nU 18\nD 3\nU 9\nR 12\nL 3\nR 15\nL 17\nD 4\nR 6\nL 5\nR 8\nD 19\nL 15\nD 5\nL 8\nR 7\nL 13\nD 3\nL 17\nD 12\nR 5\nL 9\nU 3\nL 17\nD 10\nR 17\nL 12\nR 16\nL 7\nD 4\nR 18\nU 10\nR 19\nL 19\nR 1\nU 10\nD 2\nL 8\nU 9\nL 15\nD 1\nR 15\nU 10\nR 18\nU 16\nR 17\nL 16\nD 9\nR 18\nU 16\nL 9\nR 14\nU 7\nL 8\nD 15\nL 7\nU 10\nD 18\nL 13\nR 1\nD 8\nR 14\nL 12\nU 2")

(defn part1 [input]
  (-> input
      (parse-apply)
      (:visited)
      (distinct)
      (count)))

(defn part2 [input]
  (-> input
      (parse-apply-9)
      (:visited)
      (distinct)
      (count)))

(part1 input1)

(part2 input1)
(part2 input2)

;(part1 input)
(part2 input)
