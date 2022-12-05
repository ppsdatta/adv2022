(ns adv.y2022)

(defn convert-ws [c count accm]
  (cond
    (empty? c) accm
    (= (first c) "") (convert-ws
                       (rest c)
                       (if (= count 3) 0 (inc count))
                       (if (= count 3) (conj accm nil) accm))
    :else (convert-ws (rest c) 0 (conj accm (first c)))))

(defn make-line-coll [line]
  (let [cs (clojure.string/split line #"\s")]
    (convert-ws cs 0 [])))

(defn make-matrix [lines]
  (->> (apply map list
              (map make-line-coll lines))
       (map reverse)
       (map (fn [x] (filter (fn [y] (not (nil? y))) x)))))

(defn take-from [coll n]
  [(apply vector (drop-last n coll))
   (apply vector (take-last n coll))])

(defn add-to [coll1 coll2]
  (concat coll1 coll2))

(defn read-and-make-matrix [lines]
  (apply vector
         (map #(apply vector %)
              (make-matrix (clojure.string/split lines #"\n")))))

(defn move [mat n from-index to-index]
  (let [fi (dec from-index)
        ti (dec to-index)
        [x y] (take-from (nth mat fi) n)]
    (-> mat
        (assoc fi x)
        (assoc ti
               (add-to (nth mat ti) (reverse y))))))

(defn move-9001 [mat n from-index to-index]
  (let [fi (dec from-index)
        ti (dec to-index)
        [x y] (take-from (nth mat fi) n)]
    (-> mat
        (assoc fi x)
        (assoc ti
               (add-to (nth mat ti) y)))))

(defn parse-move
  [s]
  (apply vector
         (map #(Integer/parseInt %)
              (mapv
                (clojure.string/split s #"\s")
                [1 3 5]))))

(defn solve-1
  [m move-lines]
  (->> (clojure.string/split move-lines #"\n")
       (map parse-move)
       (reduce (fn [mat x] (apply (partial move mat) x)) m)
       (map (fn [x] (nth x (dec (count x)))))
       (map #(.charAt % 1))
       (apply str)))

(defn solve-2
  [m move-lines]
  (->> (clojure.string/split move-lines #"\n")
       (map parse-move)
       (reduce (fn [mat x] (apply (partial move-9001 mat) x)) m)
       (map (fn [x] (nth x (dec (count x)))))
       (map #(.charAt % 1))
       (apply str)))

(def m2 [["[Z]" "[N]"] ["[M]" "[C]" "[D]"] ["[P]"]])
(solve m2 "move 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")
(def m
  (read-and-make-matrix "                        [R] [J] [W]\n            [R] [N]     [T] [T] [C]\n[R]         [P] [G]     [J] [P] [T]\n[Q]     [C] [M] [V]     [F] [F] [H]\n[G] [P] [M] [S] [Z]     [Z] [C] [Q]\n[P] [C] [P] [Q] [J] [J] [P] [H] [Z]\n[C] [T] [H] [T] [H] [P] [G] [L] [V]\n[F] [W] [B] [L] [P] [D] [L] [N] [G]"))

(solve-1 m "move 2 from 2 to 8\nmove 2 from 1 to 6\nmove 8 from 7 to 1\nmove 7 from 5 to 4\nmove 1 from 6 to 4\nmove 1 from 6 to 3\nmove 6 from 3 to 5\nmove 9 from 8 to 1\nmove 3 from 6 to 7\nmove 14 from 4 to 1\nmove 6 from 1 to 7\nmove 16 from 1 to 9\nmove 6 from 1 to 4\nmove 1 from 8 to 6\nmove 4 from 1 to 5\nmove 11 from 9 to 7\nmove 2 from 1 to 8\nmove 1 from 6 to 7\nmove 1 from 8 to 7\nmove 1 from 8 to 3\nmove 7 from 4 to 3\nmove 14 from 7 to 6\nmove 8 from 6 to 9\nmove 19 from 9 to 2\nmove 1 from 1 to 2\nmove 2 from 9 to 7\nmove 9 from 7 to 8\nmove 2 from 2 to 8\nmove 16 from 2 to 9\nmove 4 from 8 to 2\nmove 1 from 7 to 9\nmove 3 from 9 to 6\nmove 3 from 3 to 6\nmove 11 from 9 to 2\nmove 7 from 5 to 3\nmove 2 from 5 to 9\nmove 6 from 6 to 4\nmove 1 from 6 to 4\nmove 4 from 6 to 8\nmove 5 from 9 to 1\nmove 4 from 1 to 7\nmove 3 from 2 to 6\nmove 3 from 4 to 1\nmove 1 from 4 to 1\nmove 2 from 1 to 3\nmove 4 from 3 to 7\nmove 1 from 5 to 2\nmove 3 from 1 to 6\nmove 15 from 2 to 5\nmove 3 from 6 to 3\nmove 13 from 3 to 8\nmove 2 from 4 to 2\nmove 9 from 5 to 4\nmove 2 from 2 to 5\nmove 5 from 7 to 5\nmove 10 from 8 to 6\nmove 1 from 2 to 5\nmove 10 from 4 to 6\nmove 4 from 8 to 6\nmove 3 from 7 to 1\nmove 3 from 1 to 9\nmove 1 from 2 to 1\nmove 8 from 5 to 2\nmove 3 from 6 to 9\nmove 6 from 8 to 5\nmove 6 from 9 to 2\nmove 1 from 1 to 9\nmove 10 from 2 to 1\nmove 4 from 8 to 5\nmove 10 from 5 to 9\nmove 11 from 9 to 7\nmove 5 from 7 to 9\nmove 1 from 9 to 2\nmove 3 from 2 to 9\nmove 2 from 2 to 8\nmove 4 from 9 to 5\nmove 4 from 1 to 9\nmove 5 from 5 to 2\nmove 5 from 1 to 4\nmove 21 from 6 to 9\nmove 3 from 2 to 9\nmove 2 from 8 to 1\nmove 25 from 9 to 6\nmove 4 from 5 to 7\nmove 1 from 4 to 6\nmove 6 from 6 to 4\nmove 3 from 4 to 6\nmove 7 from 7 to 3\nmove 4 from 9 to 1\nmove 3 from 7 to 8\nmove 2 from 9 to 8\nmove 2 from 2 to 8\nmove 4 from 1 to 3\nmove 9 from 6 to 2\nmove 13 from 6 to 4\nmove 13 from 4 to 5\nmove 1 from 5 to 8\nmove 2 from 2 to 3\nmove 6 from 5 to 3\nmove 19 from 3 to 6\nmove 1 from 4 to 9\nmove 2 from 8 to 1\nmove 5 from 2 to 3\nmove 5 from 1 to 9\nmove 7 from 5 to 4\nmove 1 from 8 to 3\nmove 1 from 2 to 6\nmove 8 from 6 to 3\nmove 1 from 9 to 8\nmove 11 from 4 to 2\nmove 1 from 4 to 6\nmove 1 from 2 to 8\nmove 5 from 3 to 4\nmove 4 from 9 to 6\nmove 1 from 6 to 8\nmove 9 from 3 to 1\nmove 7 from 2 to 9\nmove 1 from 2 to 6\nmove 3 from 1 to 8\nmove 2 from 2 to 3\nmove 3 from 9 to 7\nmove 3 from 4 to 7\nmove 2 from 4 to 3\nmove 2 from 3 to 5\nmove 8 from 6 to 4\nmove 6 from 8 to 6\nmove 2 from 9 to 4\nmove 5 from 8 to 6\nmove 3 from 7 to 5\nmove 1 from 5 to 8\nmove 1 from 8 to 2\nmove 1 from 5 to 1\nmove 11 from 4 to 9\nmove 2 from 6 to 3\nmove 2 from 2 to 4\nmove 6 from 1 to 2\nmove 6 from 2 to 1\nmove 3 from 7 to 3\nmove 2 from 4 to 7\nmove 4 from 6 to 5\nmove 7 from 3 to 7\nmove 5 from 9 to 6\nmove 22 from 6 to 8\nmove 2 from 6 to 5\nmove 2 from 8 to 4\nmove 14 from 8 to 7\nmove 11 from 7 to 4\nmove 3 from 8 to 1\nmove 9 from 7 to 8\nmove 10 from 1 to 4\nmove 1 from 7 to 4\nmove 4 from 8 to 7\nmove 6 from 4 to 9\nmove 7 from 4 to 1\nmove 3 from 4 to 8\nmove 1 from 5 to 8\nmove 8 from 5 to 3\nmove 4 from 3 to 9\nmove 7 from 8 to 9\nmove 3 from 8 to 3\nmove 2 from 8 to 2\nmove 7 from 9 to 1\nmove 2 from 2 to 8\nmove 8 from 9 to 1\nmove 8 from 1 to 7\nmove 7 from 1 to 5\nmove 7 from 7 to 1\nmove 11 from 9 to 8\nmove 9 from 8 to 5\nmove 2 from 8 to 5\nmove 3 from 1 to 8\nmove 2 from 3 to 7\nmove 6 from 4 to 1\nmove 6 from 1 to 6\nmove 5 from 7 to 1\nmove 2 from 4 to 6\nmove 1 from 3 to 5\nmove 4 from 7 to 4\nmove 2 from 8 to 7\nmove 10 from 5 to 6\nmove 9 from 6 to 1\nmove 8 from 1 to 6\nmove 1 from 7 to 2\nmove 9 from 6 to 4\nmove 2 from 4 to 3\nmove 3 from 8 to 1\nmove 1 from 2 to 4\nmove 4 from 4 to 1\nmove 7 from 4 to 3\nmove 3 from 3 to 2\nmove 1 from 7 to 6\nmove 9 from 6 to 7\nmove 6 from 7 to 4\nmove 2 from 7 to 2\nmove 6 from 4 to 7\nmove 2 from 2 to 9\nmove 1 from 2 to 4\nmove 1 from 7 to 4\nmove 4 from 7 to 6\nmove 4 from 5 to 4\nmove 1 from 2 to 5\nmove 1 from 7 to 5\nmove 1 from 2 to 6\nmove 6 from 4 to 3\nmove 9 from 3 to 9\nmove 4 from 6 to 2\nmove 7 from 3 to 8\nmove 22 from 1 to 7\nmove 1 from 1 to 7\nmove 2 from 8 to 3\nmove 4 from 5 to 6\nmove 2 from 3 to 2\nmove 6 from 2 to 8\nmove 3 from 8 to 6\nmove 1 from 4 to 8\nmove 1 from 1 to 8\nmove 8 from 6 to 7\nmove 7 from 8 to 9\nmove 22 from 7 to 4\nmove 3 from 5 to 6\nmove 1 from 8 to 1\nmove 2 from 8 to 2\nmove 3 from 6 to 4\nmove 1 from 1 to 3\nmove 15 from 9 to 1\nmove 5 from 1 to 5\nmove 3 from 7 to 6\nmove 5 from 5 to 6\nmove 4 from 4 to 3\nmove 6 from 6 to 9\nmove 7 from 7 to 6\nmove 5 from 6 to 7\nmove 4 from 1 to 9\nmove 3 from 7 to 4\nmove 2 from 9 to 7\nmove 5 from 3 to 5\nmove 3 from 6 to 3\nmove 5 from 4 to 6\nmove 10 from 9 to 5\nmove 1 from 2 to 9\nmove 1 from 3 to 5\nmove 1 from 2 to 9\nmove 3 from 1 to 6\nmove 2 from 9 to 2\nmove 7 from 6 to 5\nmove 15 from 4 to 9\nmove 2 from 4 to 5\nmove 1 from 3 to 4\nmove 9 from 9 to 1\nmove 1 from 9 to 2\nmove 2 from 9 to 4\nmove 11 from 5 to 4\nmove 1 from 9 to 3\nmove 1 from 6 to 8\nmove 4 from 7 to 8\nmove 4 from 8 to 9\nmove 15 from 4 to 7\nmove 1 from 6 to 7\nmove 1 from 3 to 7\nmove 6 from 9 to 6\nmove 1 from 3 to 7\nmove 1 from 2 to 1\nmove 1 from 9 to 5\nmove 3 from 6 to 1\nmove 11 from 1 to 4\nmove 6 from 5 to 1\nmove 2 from 2 to 5\nmove 1 from 5 to 7\nmove 2 from 6 to 1\nmove 7 from 5 to 7\nmove 3 from 5 to 6\nmove 4 from 6 to 1\nmove 11 from 4 to 3\nmove 1 from 8 to 5\nmove 23 from 7 to 6\nmove 18 from 6 to 9\nmove 1 from 5 to 9\nmove 1 from 4 to 2\nmove 3 from 3 to 7\nmove 3 from 3 to 8\nmove 17 from 1 to 8\nmove 5 from 6 to 5\nmove 2 from 7 to 1\nmove 20 from 8 to 2\nmove 4 from 7 to 2\nmove 3 from 9 to 5\nmove 7 from 9 to 7\nmove 6 from 9 to 2\nmove 1 from 1 to 8\nmove 3 from 9 to 4\nmove 7 from 5 to 2\nmove 6 from 7 to 1\nmove 1 from 1 to 8\nmove 3 from 2 to 6\nmove 1 from 7 to 6\nmove 2 from 8 to 9\nmove 35 from 2 to 4\nmove 3 from 3 to 2\nmove 1 from 5 to 7\nmove 2 from 3 to 9\nmove 3 from 1 to 6\nmove 2 from 2 to 1\nmove 32 from 4 to 7\nmove 3 from 4 to 8\nmove 3 from 9 to 5\nmove 1 from 1 to 2\nmove 21 from 7 to 5\nmove 2 from 2 to 1\nmove 3 from 1 to 2\nmove 15 from 5 to 1\nmove 3 from 6 to 7\nmove 3 from 4 to 6\nmove 3 from 8 to 5\nmove 1 from 9 to 3\nmove 8 from 7 to 2\nmove 6 from 5 to 2\nmove 9 from 1 to 6\nmove 4 from 7 to 1\nmove 2 from 5 to 4\nmove 2 from 4 to 3\nmove 3 from 5 to 4\nmove 17 from 2 to 7\nmove 3 from 3 to 5\nmove 2 from 4 to 8\nmove 1 from 4 to 3\nmove 5 from 7 to 9\nmove 1 from 3 to 6\nmove 4 from 1 to 7\nmove 4 from 6 to 7\nmove 2 from 5 to 2\nmove 1 from 1 to 3\nmove 10 from 6 to 4\nmove 1 from 3 to 7\nmove 20 from 7 to 8\nmove 8 from 4 to 8\nmove 1 from 2 to 8\nmove 4 from 9 to 1\nmove 3 from 7 to 4\nmove 2 from 4 to 9\nmove 2 from 6 to 3\nmove 1 from 2 to 8\nmove 1 from 7 to 6\nmove 1 from 9 to 5\nmove 3 from 5 to 9\nmove 4 from 9 to 2\nmove 1 from 4 to 5\nmove 1 from 5 to 3\nmove 3 from 2 to 4\nmove 1 from 9 to 7\nmove 1 from 2 to 1\nmove 1 from 7 to 1\nmove 11 from 1 to 2\nmove 3 from 1 to 7\nmove 25 from 8 to 5\nmove 1 from 6 to 3\nmove 1 from 6 to 2\nmove 7 from 8 to 2\nmove 9 from 2 to 8\nmove 2 from 4 to 7\nmove 2 from 5 to 7\nmove 2 from 5 to 2\nmove 5 from 5 to 1\nmove 7 from 5 to 1\nmove 2 from 4 to 9\nmove 3 from 5 to 6\nmove 1 from 1 to 8\nmove 1 from 5 to 6\nmove 1 from 4 to 7\nmove 1 from 9 to 2\nmove 3 from 5 to 2\nmove 2 from 6 to 9\nmove 3 from 9 to 8\nmove 1 from 5 to 4\nmove 3 from 3 to 9\nmove 10 from 1 to 5\nmove 4 from 2 to 8\nmove 2 from 6 to 1\nmove 3 from 9 to 7\nmove 1 from 1 to 9\nmove 1 from 4 to 3\nmove 1 from 9 to 2\nmove 9 from 8 to 2\nmove 2 from 3 to 7\nmove 2 from 7 to 6\nmove 3 from 5 to 6\nmove 4 from 8 to 6\nmove 4 from 8 to 3\nmove 4 from 3 to 2\nmove 4 from 6 to 8\nmove 1 from 7 to 9\nmove 2 from 1 to 8\nmove 2 from 8 to 3\nmove 1 from 9 to 2\nmove 13 from 2 to 4\nmove 6 from 5 to 7\nmove 2 from 5 to 7\nmove 10 from 2 to 4\nmove 11 from 7 to 8\nmove 1 from 6 to 4\nmove 4 from 6 to 7\nmove 24 from 4 to 9\nmove 11 from 7 to 4\nmove 1 from 3 to 8\nmove 1 from 3 to 5\nmove 4 from 4 to 2\nmove 5 from 4 to 2\nmove 9 from 2 to 5\nmove 4 from 9 to 5\nmove 1 from 5 to 1\nmove 2 from 5 to 7\nmove 2 from 2 to 5\nmove 1 from 1 to 7\nmove 2 from 2 to 3\nmove 18 from 9 to 6\nmove 9 from 8 to 1\nmove 2 from 9 to 5\nmove 5 from 1 to 8\nmove 2 from 8 to 7\nmove 4 from 8 to 4\nmove 5 from 8 to 7\nmove 10 from 5 to 1\nmove 10 from 7 to 4\nmove 4 from 5 to 8\nmove 14 from 1 to 9\nmove 6 from 9 to 8\nmove 1 from 5 to 1\nmove 12 from 6 to 9\nmove 4 from 6 to 8\nmove 11 from 8 to 5\nmove 1 from 6 to 1\nmove 19 from 9 to 7\nmove 2 from 3 to 5\nmove 13 from 7 to 5\nmove 3 from 7 to 1\nmove 4 from 8 to 9\nmove 2 from 7 to 6\nmove 7 from 4 to 8\nmove 5 from 8 to 1\nmove 1 from 1 to 3\nmove 1 from 7 to 2\nmove 6 from 1 to 6\nmove 1 from 2 to 5\nmove 1 from 8 to 1\nmove 1 from 8 to 2\nmove 2 from 4 to 8\nmove 5 from 6 to 1\nmove 2 from 4 to 7\nmove 2 from 9 to 6\nmove 1 from 6 to 5\nmove 4 from 6 to 2\nmove 1 from 9 to 5\nmove 2 from 4 to 5\nmove 4 from 2 to 4\nmove 2 from 8 to 3\nmove 3 from 3 to 2\nmove 4 from 1 to 2\nmove 2 from 4 to 7\nmove 4 from 2 to 3\nmove 4 from 1 to 2\nmove 13 from 5 to 1\nmove 1 from 6 to 2\nmove 1 from 1 to 8\nmove 15 from 5 to 2\nmove 4 from 3 to 1\nmove 5 from 4 to 3\nmove 1 from 3 to 6\nmove 1 from 8 to 7\nmove 1 from 9 to 8\nmove 1 from 7 to 8\nmove 3 from 3 to 2\nmove 1 from 8 to 2\nmove 1 from 3 to 7\nmove 13 from 1 to 4\nmove 3 from 5 to 3\nmove 1 from 1 to 2\nmove 1 from 8 to 5\nmove 5 from 7 to 2\nmove 1 from 6 to 5\nmove 2 from 3 to 4\nmove 10 from 2 to 5\nmove 1 from 9 to 5\nmove 3 from 1 to 8\nmove 3 from 8 to 3\nmove 11 from 4 to 5\nmove 12 from 2 to 8\nmove 4 from 4 to 7\nmove 10 from 8 to 5\nmove 2 from 8 to 1\nmove 1 from 7 to 3\nmove 1 from 7 to 9\nmove 5 from 3 to 7\nmove 1 from 9 to 4\nmove 7 from 7 to 6\nmove 13 from 5 to 8\nmove 6 from 6 to 7\nmove 5 from 7 to 4\nmove 1 from 6 to 4\nmove 2 from 4 to 9\nmove 1 from 7 to 9\nmove 3 from 4 to 3\nmove 1 from 3 to 6\nmove 4 from 5 to 7")
(solve-2 m "move 2 from 2 to 8\nmove 2 from 1 to 6\nmove 8 from 7 to 1\nmove 7 from 5 to 4\nmove 1 from 6 to 4\nmove 1 from 6 to 3\nmove 6 from 3 to 5\nmove 9 from 8 to 1\nmove 3 from 6 to 7\nmove 14 from 4 to 1\nmove 6 from 1 to 7\nmove 16 from 1 to 9\nmove 6 from 1 to 4\nmove 1 from 8 to 6\nmove 4 from 1 to 5\nmove 11 from 9 to 7\nmove 2 from 1 to 8\nmove 1 from 6 to 7\nmove 1 from 8 to 7\nmove 1 from 8 to 3\nmove 7 from 4 to 3\nmove 14 from 7 to 6\nmove 8 from 6 to 9\nmove 19 from 9 to 2\nmove 1 from 1 to 2\nmove 2 from 9 to 7\nmove 9 from 7 to 8\nmove 2 from 2 to 8\nmove 16 from 2 to 9\nmove 4 from 8 to 2\nmove 1 from 7 to 9\nmove 3 from 9 to 6\nmove 3 from 3 to 6\nmove 11 from 9 to 2\nmove 7 from 5 to 3\nmove 2 from 5 to 9\nmove 6 from 6 to 4\nmove 1 from 6 to 4\nmove 4 from 6 to 8\nmove 5 from 9 to 1\nmove 4 from 1 to 7\nmove 3 from 2 to 6\nmove 3 from 4 to 1\nmove 1 from 4 to 1\nmove 2 from 1 to 3\nmove 4 from 3 to 7\nmove 1 from 5 to 2\nmove 3 from 1 to 6\nmove 15 from 2 to 5\nmove 3 from 6 to 3\nmove 13 from 3 to 8\nmove 2 from 4 to 2\nmove 9 from 5 to 4\nmove 2 from 2 to 5\nmove 5 from 7 to 5\nmove 10 from 8 to 6\nmove 1 from 2 to 5\nmove 10 from 4 to 6\nmove 4 from 8 to 6\nmove 3 from 7 to 1\nmove 3 from 1 to 9\nmove 1 from 2 to 1\nmove 8 from 5 to 2\nmove 3 from 6 to 9\nmove 6 from 8 to 5\nmove 6 from 9 to 2\nmove 1 from 1 to 9\nmove 10 from 2 to 1\nmove 4 from 8 to 5\nmove 10 from 5 to 9\nmove 11 from 9 to 7\nmove 5 from 7 to 9\nmove 1 from 9 to 2\nmove 3 from 2 to 9\nmove 2 from 2 to 8\nmove 4 from 9 to 5\nmove 4 from 1 to 9\nmove 5 from 5 to 2\nmove 5 from 1 to 4\nmove 21 from 6 to 9\nmove 3 from 2 to 9\nmove 2 from 8 to 1\nmove 25 from 9 to 6\nmove 4 from 5 to 7\nmove 1 from 4 to 6\nmove 6 from 6 to 4\nmove 3 from 4 to 6\nmove 7 from 7 to 3\nmove 4 from 9 to 1\nmove 3 from 7 to 8\nmove 2 from 9 to 8\nmove 2 from 2 to 8\nmove 4 from 1 to 3\nmove 9 from 6 to 2\nmove 13 from 6 to 4\nmove 13 from 4 to 5\nmove 1 from 5 to 8\nmove 2 from 2 to 3\nmove 6 from 5 to 3\nmove 19 from 3 to 6\nmove 1 from 4 to 9\nmove 2 from 8 to 1\nmove 5 from 2 to 3\nmove 5 from 1 to 9\nmove 7 from 5 to 4\nmove 1 from 8 to 3\nmove 1 from 2 to 6\nmove 8 from 6 to 3\nmove 1 from 9 to 8\nmove 11 from 4 to 2\nmove 1 from 4 to 6\nmove 1 from 2 to 8\nmove 5 from 3 to 4\nmove 4 from 9 to 6\nmove 1 from 6 to 8\nmove 9 from 3 to 1\nmove 7 from 2 to 9\nmove 1 from 2 to 6\nmove 3 from 1 to 8\nmove 2 from 2 to 3\nmove 3 from 9 to 7\nmove 3 from 4 to 7\nmove 2 from 4 to 3\nmove 2 from 3 to 5\nmove 8 from 6 to 4\nmove 6 from 8 to 6\nmove 2 from 9 to 4\nmove 5 from 8 to 6\nmove 3 from 7 to 5\nmove 1 from 5 to 8\nmove 1 from 8 to 2\nmove 1 from 5 to 1\nmove 11 from 4 to 9\nmove 2 from 6 to 3\nmove 2 from 2 to 4\nmove 6 from 1 to 2\nmove 6 from 2 to 1\nmove 3 from 7 to 3\nmove 2 from 4 to 7\nmove 4 from 6 to 5\nmove 7 from 3 to 7\nmove 5 from 9 to 6\nmove 22 from 6 to 8\nmove 2 from 6 to 5\nmove 2 from 8 to 4\nmove 14 from 8 to 7\nmove 11 from 7 to 4\nmove 3 from 8 to 1\nmove 9 from 7 to 8\nmove 10 from 1 to 4\nmove 1 from 7 to 4\nmove 4 from 8 to 7\nmove 6 from 4 to 9\nmove 7 from 4 to 1\nmove 3 from 4 to 8\nmove 1 from 5 to 8\nmove 8 from 5 to 3\nmove 4 from 3 to 9\nmove 7 from 8 to 9\nmove 3 from 8 to 3\nmove 2 from 8 to 2\nmove 7 from 9 to 1\nmove 2 from 2 to 8\nmove 8 from 9 to 1\nmove 8 from 1 to 7\nmove 7 from 1 to 5\nmove 7 from 7 to 1\nmove 11 from 9 to 8\nmove 9 from 8 to 5\nmove 2 from 8 to 5\nmove 3 from 1 to 8\nmove 2 from 3 to 7\nmove 6 from 4 to 1\nmove 6 from 1 to 6\nmove 5 from 7 to 1\nmove 2 from 4 to 6\nmove 1 from 3 to 5\nmove 4 from 7 to 4\nmove 2 from 8 to 7\nmove 10 from 5 to 6\nmove 9 from 6 to 1\nmove 8 from 1 to 6\nmove 1 from 7 to 2\nmove 9 from 6 to 4\nmove 2 from 4 to 3\nmove 3 from 8 to 1\nmove 1 from 2 to 4\nmove 4 from 4 to 1\nmove 7 from 4 to 3\nmove 3 from 3 to 2\nmove 1 from 7 to 6\nmove 9 from 6 to 7\nmove 6 from 7 to 4\nmove 2 from 7 to 2\nmove 6 from 4 to 7\nmove 2 from 2 to 9\nmove 1 from 2 to 4\nmove 1 from 7 to 4\nmove 4 from 7 to 6\nmove 4 from 5 to 4\nmove 1 from 2 to 5\nmove 1 from 7 to 5\nmove 1 from 2 to 6\nmove 6 from 4 to 3\nmove 9 from 3 to 9\nmove 4 from 6 to 2\nmove 7 from 3 to 8\nmove 22 from 1 to 7\nmove 1 from 1 to 7\nmove 2 from 8 to 3\nmove 4 from 5 to 6\nmove 2 from 3 to 2\nmove 6 from 2 to 8\nmove 3 from 8 to 6\nmove 1 from 4 to 8\nmove 1 from 1 to 8\nmove 8 from 6 to 7\nmove 7 from 8 to 9\nmove 22 from 7 to 4\nmove 3 from 5 to 6\nmove 1 from 8 to 1\nmove 2 from 8 to 2\nmove 3 from 6 to 4\nmove 1 from 1 to 3\nmove 15 from 9 to 1\nmove 5 from 1 to 5\nmove 3 from 7 to 6\nmove 5 from 5 to 6\nmove 4 from 4 to 3\nmove 6 from 6 to 9\nmove 7 from 7 to 6\nmove 5 from 6 to 7\nmove 4 from 1 to 9\nmove 3 from 7 to 4\nmove 2 from 9 to 7\nmove 5 from 3 to 5\nmove 3 from 6 to 3\nmove 5 from 4 to 6\nmove 10 from 9 to 5\nmove 1 from 2 to 9\nmove 1 from 3 to 5\nmove 1 from 2 to 9\nmove 3 from 1 to 6\nmove 2 from 9 to 2\nmove 7 from 6 to 5\nmove 15 from 4 to 9\nmove 2 from 4 to 5\nmove 1 from 3 to 4\nmove 9 from 9 to 1\nmove 1 from 9 to 2\nmove 2 from 9 to 4\nmove 11 from 5 to 4\nmove 1 from 9 to 3\nmove 1 from 6 to 8\nmove 4 from 7 to 8\nmove 4 from 8 to 9\nmove 15 from 4 to 7\nmove 1 from 6 to 7\nmove 1 from 3 to 7\nmove 6 from 9 to 6\nmove 1 from 3 to 7\nmove 1 from 2 to 1\nmove 1 from 9 to 5\nmove 3 from 6 to 1\nmove 11 from 1 to 4\nmove 6 from 5 to 1\nmove 2 from 2 to 5\nmove 1 from 5 to 7\nmove 2 from 6 to 1\nmove 7 from 5 to 7\nmove 3 from 5 to 6\nmove 4 from 6 to 1\nmove 11 from 4 to 3\nmove 1 from 8 to 5\nmove 23 from 7 to 6\nmove 18 from 6 to 9\nmove 1 from 5 to 9\nmove 1 from 4 to 2\nmove 3 from 3 to 7\nmove 3 from 3 to 8\nmove 17 from 1 to 8\nmove 5 from 6 to 5\nmove 2 from 7 to 1\nmove 20 from 8 to 2\nmove 4 from 7 to 2\nmove 3 from 9 to 5\nmove 7 from 9 to 7\nmove 6 from 9 to 2\nmove 1 from 1 to 8\nmove 3 from 9 to 4\nmove 7 from 5 to 2\nmove 6 from 7 to 1\nmove 1 from 1 to 8\nmove 3 from 2 to 6\nmove 1 from 7 to 6\nmove 2 from 8 to 9\nmove 35 from 2 to 4\nmove 3 from 3 to 2\nmove 1 from 5 to 7\nmove 2 from 3 to 9\nmove 3 from 1 to 6\nmove 2 from 2 to 1\nmove 32 from 4 to 7\nmove 3 from 4 to 8\nmove 3 from 9 to 5\nmove 1 from 1 to 2\nmove 21 from 7 to 5\nmove 2 from 2 to 1\nmove 3 from 1 to 2\nmove 15 from 5 to 1\nmove 3 from 6 to 7\nmove 3 from 4 to 6\nmove 3 from 8 to 5\nmove 1 from 9 to 3\nmove 8 from 7 to 2\nmove 6 from 5 to 2\nmove 9 from 1 to 6\nmove 4 from 7 to 1\nmove 2 from 5 to 4\nmove 2 from 4 to 3\nmove 3 from 5 to 4\nmove 17 from 2 to 7\nmove 3 from 3 to 5\nmove 2 from 4 to 8\nmove 1 from 4 to 3\nmove 5 from 7 to 9\nmove 1 from 3 to 6\nmove 4 from 1 to 7\nmove 4 from 6 to 7\nmove 2 from 5 to 2\nmove 1 from 1 to 3\nmove 10 from 6 to 4\nmove 1 from 3 to 7\nmove 20 from 7 to 8\nmove 8 from 4 to 8\nmove 1 from 2 to 8\nmove 4 from 9 to 1\nmove 3 from 7 to 4\nmove 2 from 4 to 9\nmove 2 from 6 to 3\nmove 1 from 2 to 8\nmove 1 from 7 to 6\nmove 1 from 9 to 5\nmove 3 from 5 to 9\nmove 4 from 9 to 2\nmove 1 from 4 to 5\nmove 1 from 5 to 3\nmove 3 from 2 to 4\nmove 1 from 9 to 7\nmove 1 from 2 to 1\nmove 1 from 7 to 1\nmove 11 from 1 to 2\nmove 3 from 1 to 7\nmove 25 from 8 to 5\nmove 1 from 6 to 3\nmove 1 from 6 to 2\nmove 7 from 8 to 2\nmove 9 from 2 to 8\nmove 2 from 4 to 7\nmove 2 from 5 to 7\nmove 2 from 5 to 2\nmove 5 from 5 to 1\nmove 7 from 5 to 1\nmove 2 from 4 to 9\nmove 3 from 5 to 6\nmove 1 from 1 to 8\nmove 1 from 5 to 6\nmove 1 from 4 to 7\nmove 1 from 9 to 2\nmove 3 from 5 to 2\nmove 2 from 6 to 9\nmove 3 from 9 to 8\nmove 1 from 5 to 4\nmove 3 from 3 to 9\nmove 10 from 1 to 5\nmove 4 from 2 to 8\nmove 2 from 6 to 1\nmove 3 from 9 to 7\nmove 1 from 1 to 9\nmove 1 from 4 to 3\nmove 1 from 9 to 2\nmove 9 from 8 to 2\nmove 2 from 3 to 7\nmove 2 from 7 to 6\nmove 3 from 5 to 6\nmove 4 from 8 to 6\nmove 4 from 8 to 3\nmove 4 from 3 to 2\nmove 4 from 6 to 8\nmove 1 from 7 to 9\nmove 2 from 1 to 8\nmove 2 from 8 to 3\nmove 1 from 9 to 2\nmove 13 from 2 to 4\nmove 6 from 5 to 7\nmove 2 from 5 to 7\nmove 10 from 2 to 4\nmove 11 from 7 to 8\nmove 1 from 6 to 4\nmove 4 from 6 to 7\nmove 24 from 4 to 9\nmove 11 from 7 to 4\nmove 1 from 3 to 8\nmove 1 from 3 to 5\nmove 4 from 4 to 2\nmove 5 from 4 to 2\nmove 9 from 2 to 5\nmove 4 from 9 to 5\nmove 1 from 5 to 1\nmove 2 from 5 to 7\nmove 2 from 2 to 5\nmove 1 from 1 to 7\nmove 2 from 2 to 3\nmove 18 from 9 to 6\nmove 9 from 8 to 1\nmove 2 from 9 to 5\nmove 5 from 1 to 8\nmove 2 from 8 to 7\nmove 4 from 8 to 4\nmove 5 from 8 to 7\nmove 10 from 5 to 1\nmove 10 from 7 to 4\nmove 4 from 5 to 8\nmove 14 from 1 to 9\nmove 6 from 9 to 8\nmove 1 from 5 to 1\nmove 12 from 6 to 9\nmove 4 from 6 to 8\nmove 11 from 8 to 5\nmove 1 from 6 to 1\nmove 19 from 9 to 7\nmove 2 from 3 to 5\nmove 13 from 7 to 5\nmove 3 from 7 to 1\nmove 4 from 8 to 9\nmove 2 from 7 to 6\nmove 7 from 4 to 8\nmove 5 from 8 to 1\nmove 1 from 1 to 3\nmove 1 from 7 to 2\nmove 6 from 1 to 6\nmove 1 from 2 to 5\nmove 1 from 8 to 1\nmove 1 from 8 to 2\nmove 2 from 4 to 8\nmove 5 from 6 to 1\nmove 2 from 4 to 7\nmove 2 from 9 to 6\nmove 1 from 6 to 5\nmove 4 from 6 to 2\nmove 1 from 9 to 5\nmove 2 from 4 to 5\nmove 4 from 2 to 4\nmove 2 from 8 to 3\nmove 3 from 3 to 2\nmove 4 from 1 to 2\nmove 2 from 4 to 7\nmove 4 from 2 to 3\nmove 4 from 1 to 2\nmove 13 from 5 to 1\nmove 1 from 6 to 2\nmove 1 from 1 to 8\nmove 15 from 5 to 2\nmove 4 from 3 to 1\nmove 5 from 4 to 3\nmove 1 from 3 to 6\nmove 1 from 8 to 7\nmove 1 from 9 to 8\nmove 1 from 7 to 8\nmove 3 from 3 to 2\nmove 1 from 8 to 2\nmove 1 from 3 to 7\nmove 13 from 1 to 4\nmove 3 from 5 to 3\nmove 1 from 1 to 2\nmove 1 from 8 to 5\nmove 5 from 7 to 2\nmove 1 from 6 to 5\nmove 2 from 3 to 4\nmove 10 from 2 to 5\nmove 1 from 9 to 5\nmove 3 from 1 to 8\nmove 3 from 8 to 3\nmove 11 from 4 to 5\nmove 12 from 2 to 8\nmove 4 from 4 to 7\nmove 10 from 8 to 5\nmove 2 from 8 to 1\nmove 1 from 7 to 3\nmove 1 from 7 to 9\nmove 5 from 3 to 7\nmove 1 from 9 to 4\nmove 7 from 7 to 6\nmove 13 from 5 to 8\nmove 6 from 6 to 7\nmove 5 from 7 to 4\nmove 1 from 6 to 4\nmove 2 from 4 to 9\nmove 1 from 7 to 9\nmove 3 from 4 to 3\nmove 1 from 3 to 6\nmove 4 from 5 to 7")
