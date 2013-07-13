(ns snake2.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane JLabel)
           (java.awt.event ActionListener KeyListener KeyEvent))
  (:use (snake2 [edit-level-gui :only (point-to-screen start-level-editor)]
                constants
                data-manager)))

(def dirs {KeyEvent/VK_LEFT [-1 0]
           KeyEvent/VK_RIGHT [1 0]
           KeyEvent/VK_UP [0 -1]
           KeyEvent/VK_DOWN [0 1]})

(def current-score (ref 0))
(def current-level (atom 1))

(defn create-board-coords []
  (for [x (range board-width)
        y (range board-height)]
    [x y]))

(def board-coords (create-board-coords))

(defn default-snake-pos [{body :body}]
  (for [x (range 1 (count body))]
    [x 10]))

;Work with files
(defn save-highscore [frame]
  (let [highscore (deserialize highscore-filename)]
    (when (> @current-score highscore)
      (serialize @current-score highscore-filename)
      (JOptionPane/showMessageDialog frame (str "New Highscore: " @current-score)))))


(defn update-wall-coordinates [walls new-coordinates]
  (assoc walls :coordinates new-coordinates))

(defn reset-snake-position [snake]
  (do (assoc snake :body (default-snake-pos snake))
      (assoc snake :dir [1 0])))

(defn load-new-level [snake apple walls frame]
  (when-let [level-filename (levels @current-score)]
    (dosync (alter walls update-wall-coordinates (deserialize level-filename))
;            (alter snake reset-snake-position)
            (swap! current-level inc))
    (JOptionPane/showMessageDialog frame (str "Level: " @current-level))))


(defn add-points
  "Used to calculate the new position of
  a moving game obj (ex. point from the snake body)"
  [& pts]
  (vec (apply map + pts)))


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
  {:coordinates []  ;(list [20 1] [20 2] [20 3] [20 4])
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

(defn head-crashed-wall? [{[head] :body} {walls :coordinates}]
  (or (not (and
         (<= 0 (head 0) board-width)
         (<= 0 (head 1) board-height)))
      (contains? (set walls) head)))

(defn lose-game? [snake walls]
  (or (head-crashed-body? snake)
    (head-crashed-wall? snake walls)))

(defn eat-apple? [{[head] :body} {apple-location :location}]
  (= head apple-location))

(defn turn-snake [snake newdir]
  (assoc snake :dir newdir))

;The mutable model with STM
(defn reset-game [snake apple walls]
;TODO read more about the refs
  (dosync (ref-set apple (create-apple walls))
          (ref-set snake (create-snake))
          (ref-set current-score 0)
          (ref-set walls (create-walls))
          (reset! current-level 1))
  nil)

(defn update-dir [snake newdir]
  (when newdir
    (dosync (alter snake turn-snake newdir))))

(defn update-positions [snake apple walls frame]
  (dosync
    (if (eat-apple? @snake @apple)
      (do (ref-set apple (create-apple walls))
          (alter snake move :growfat)
          (alter current-score + points-per-apple)
          (load-new-level snake apple walls frame))
      (alter snake move)))
   nil)

;GUI

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen pt)]
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
      (update-positions snake apple walls frame)
;      (load-new-level snake apple walls frame)
      (prn @current-score)
;      (.setText scorelabel (str "Score: " @current-score))
      (when (lose-game? @snake @walls)
        (JOptionPane/showMessageDialog frame "You lose! Try your skills again.")
        (prn "After pushing the button")
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
  (let [walls (ref (create-walls))
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
;  (game))
  (start-level-editor))