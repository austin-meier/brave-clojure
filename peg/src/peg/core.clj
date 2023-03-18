(ns peg.core
  (:require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(def new-board
  {1 {:pegged true :connections {6 3, 4 2}}
   2 {:pegged true :connections {9 5, 7 4}}
   3 {:pegged true :connections {10 6, 8 5}}
   4 {:pegged true :connections {13 8, 11 7, 6 5, 1 2}}
   5 {:pegged true :connections {14 9, 12 8}}
   6 {:pegged true :connections {15 10, 13 9, 4 5, 1 3}}
   7 {:pegged true :connections {9 8, 2 4}}
   8 {:pegged true :connections {10 9, 3 5}}
   9 {:pegged true :connections {7 8, 2 5}}
   10 {:pegged true :connections {8 9, 3 6}}
   11 {:pegged true :connections {13 12, 4 7}}
   12 {:pegged true :connections {14 13, 5 8}}
   13 {:pegged true :connections {15 14, 11 12, 6 9, 4 8}}
   14 {:pegged true :connections {12 13, 5 9}}
   15 {:pegged true :connections {13 14, 6 10}}
   :rows 5})

(defn tri*
  "Generates a lazy sequence of triangle numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
    (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular? [n]
  "Is the number triangular?"
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangle number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(new-board 5)
