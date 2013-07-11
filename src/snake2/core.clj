(ns snake2.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane JLabel)
           (java.awt.event ActionListener KeyListener KeyEvent))
  (:use clojure.java.io))


;Constants

(def highscore-filename "snake_highscore.txt")

(def board-width 50)
(def board-height 25)
(def point-size 15)
(def game-speed-millis 75)
(def win-length 10)
(def dirs {KeyEvent/VK_LEFT [-1 0]
           KeyEvent/VK_RIGHT [1 0]
           KeyEvent/VK_UP [0 -1]
           KeyEvent/VK_DOWN [0 1]})

(def points-per-apple 1)
(def lv2-score (* 10 points-per-apple))
(def lv3-score (* 10 points-per-apple))
(def current-score (ref 0))

(defn create-board-coords []
  (for [x (range board-width)
        y (range board-height)]
    [x y]))

(def board-coords (create-board-coords))

;Work with files

;Here '#^String is maybe type hinting'
(defn serialize [data-structure #^String filename]
  ;'with-open' ensures that the reader is closed at the end of the form
  (with-open [w (writer (file filename))]
    (print-dup data-structure w)))

(defn deserialize [filename]
  (with-open [r (java.io.PushbackReader. (reader filename))]
    (read r)))

(defn save-highscore [frame]
  (let [highscore (deserialize highscore-filename)]
    (when (> @current-score highscore)
      (serialize @current-score highscore-filename)
      (JOptionPane/showMessageDialog frame (str "New Highscore: " @current-score)))))

;Used to calculate the new position of a moving game obj
(defn add-points [& pts]
  (vec (apply map + pts)))

(defn convert-point-to-screen-rect
  "Converts a point in game space to a rect on the screen"
  [pt]
  (map #(* point-size % )
       [(pt 0) (pt 1) 1 1]))

(defn create-apple [{walls-coords :coordinates}]
  (let [availabe-coords (remove (set walls-coords) board-coords)]
    {:location (rand-nth availabe-coords)
     :color (Color. 210 50 80)
     :type :apple}))

(defn create-snake []
  {:body (list [1 1])
   :dir [1 0]
   :color (Color. 15 160 70)
   :type :snake})

(defn create-walls []
  {:coordinates (list [20 1] [20 2] [20 3] [20 4])
   :color (Color. 139 69 19)
   :type :walls})

;Constantly movement without our navigation
(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir)
                            (if grow
                              body
                              (butlast body)))))

(defn win? [{body :body}]
  (>= (count body) win-length))

(defn head-crashed-body? [{[head & body] :body}]
  (contains? (set body) head))

(defn head-crashed-wall? [{[head] :body}]
  (or (not (and
         (<= 0 (head 0) board-width)
         (<= 0 (head 1) board-height)))
      (contains? (set (:coordinates (create-walls))) head)))

(defn lose-game? [snake]
  (or (head-crashed-body? snake)
    (head-crashed-wall? snake)))

(defn eat-apple? [{[head] :body} {apple-location :location}]
  (= head apple-location))

(defn turn-snake [snake newdir]
  (assoc snake :dir newdir))

;The mutable model with STM
(defn reset-game [snake apple walls]
;TODO read more about the refs
  (dosync (ref-set apple (create-apple walls))
          (ref-set snake (create-snake))
          (ref-set current-score 0))
  nil)

(defn update-dir [snake newdir]
  (when newdir
    (dosync (alter snake turn-snake newdir))))

(defn update-positions [snake apple walls]
  (dosync
    (if (eat-apple? @snake @apple)
      (do (ref-set apple (create-apple walls))
          (alter snake move :growfat)
          (alter current-score + points-per-apple))
      (alter snake move)))
   nil)

;GUI

(def border-size 3)
(defn fill-point [g pt color]
  (let [[x y width height] (convert-point-to-screen-rect pt)]
        (.setColor g color)
        (.fillRect g x y width height)
;  Draw Shadows for more realistic 3-D effect
        (.setColor g (.brighter color))
        (.fillRect g x y width border-size)
        (.fillRect g x y border-size height)
        (.setColor g (.darker color))
        (.fillRect g x (- y border-size) width border-size)
        (.fillRect g (- x border-size) y border-size height)
        (.fillRect g x (+ y height) width border-size)
        (.fillRect g (+ x width) y border-size height)))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defmethod paint :walls [g {:keys [coordinates color]}]
  (doseq [point coordinates]
    (fill-point g point color)))


(defn game-board [frame snake apple walls scorelabel]
;TODO read more about "proxy"
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @snake)
      (paint g @apple)
      (paint g @walls))
    (actionPerformed [e]
      ;actionPerformed is called on every timer tick
      (update-positions snake apple walls)
      (prn @current-score)
;      (.setText scorelabel (str "Score: " @current-score))
      (when (lose-game? @snake)
        (JOptionPane/showMessageDialog frame "You lose! Try your skills again.")
        (save-highscore frame)
        (reset-game snake apple walls))
      (when (win? @snake)
        (JOptionPane/showMessageDialog frame "You win!")
        (save-highscore frame)
        (reset-game snake apple walls))
      (.repaint this))
    (keyPressed [e]
      (update-dir snake (dirs (.getKeyCode e))))
    (getPreferredSize []
      (Dimension. (* (inc board-width) point-size)
                  (* (inc board-height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [walls (atom (create-walls))
        snake (ref (create-snake))
        apple (ref (create-apple walls))
        frame (JFrame. "Clojure Snake")
        scorelabel (JLabel. "Score: 0")
        board (game-board frame snake apple walls scorelabel)
        timer (Timer. game-speed-millis board)]
    (doto board
      (.setFocusable true)
      (.addKeyListener board))
    (doto frame
      (.add board)
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (.start timer)
))

(defn -main [& args]
  (game))