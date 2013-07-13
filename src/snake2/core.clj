(ns snake2.core
  (:import java.awt.Color)
  (:use (snake2 [edit-level-gui :only (point-to-screen start-level-editor)]
                constants
                data-manager)
        seesaw.core))

(def current-score (ref 0))
(def current-level (atom 1))

(defn create-board-coords []
  (for [x (range board-width)
        y (range board-height)]
    [x y]))

(def board-coords (create-board-coords))

;Work with files
(defn save-highscore []
  (let [highscore (deserialize highscore-filename)]
    (when (> @current-score highscore)
      (serialize @current-score highscore-filename)
      (alert (str "New Highscore: " @current-score))))
  nil)


(defn update-wall-coordinates [walls new-coordinates]
  (assoc walls :coordinates new-coordinates))

(defn load-new-level [snake apple walls]
  (when-let [level-filename (levels @current-score)]
    (dosync (alter walls update-wall-coordinates (deserialize level-filename))
            (swap! current-level inc))
    (alert (str "Level: " @current-level))))


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

(defn update-positions [snake apple walls]
  (dosync
    (if (eat-apple? @snake @apple)
      (do (ref-set apple (create-apple walls))
          (alter snake move :growfat)
          (alter current-score + points-per-apple)
          (load-new-level snake apple walls))
      (alter snake move)))
   nil)