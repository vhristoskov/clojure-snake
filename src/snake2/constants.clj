(ns snake2.constants)

(def border-size 3)
(def board-width 50)
(def board-height 25)
(def point-size 15)

(def board-real-width (* (inc board-width) point-size))
(def board-real-height (* (inc board-height) point-size))

(def game-speed-millis 75)
(def win-length 20)

(def points-per-apple 1)
(def lv2-score (* 5 points-per-apple))
(def lv3-score (* 8 points-per-apple))
(def lv4-score (* 10 points-per-apple))

(def levels {lv2-score "snake_lv2.txt" lv3-score "snake_lv3.txt" lv4-score "newLevel.txt"})
(def highscore-filename "snake_highscore.txt")

(def dirs {:left [-1 0]
           :right [1 0]
           :up [0 -1]
           :down [0 1]})