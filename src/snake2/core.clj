(ns snake2.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener KeyEvent))
  (:use clojure.java.io))

;(defn -main
;  "I don't do a whole lot ... yet."
;  [& args]
;  ;; work around dangerous default behaviour in Clojure
;  (alter-var-root #'*read-eval* (constantly false))
;  (println "Hello, World!"))

;Work with files

;Here '#^String is maybe type hinting'
(defn serialize [data-structure #^String filename]
  ;'with-open' ensures that the reader is closed at the end of the form
  (with-open [w (writer (file filename))]
    (print-dup data-structure w)))

(defn deserialize [filename]
  (with-open [r (java.io.PushbackReader. (reader filename))]
    (read r)))

;Constants

(def board-width 75)
(def board-height 50)
(def point-size 10)
(def game-speed-millis 75)
(def win-length 5)
(def dirs {KeyEvent/VK_LEFT [-1 0]
           KeyEvent/VK_RIGHT [1 0]
           KeyEvent/VK_UP [0 -1]
           KeyEvent/VK_DOWN [0 1]})

;Used to calculate the new position of a moving game obj
(defn add-points [& pts]
  (vec (apply map + pts)))

(defn point-to-screen-rect
  "Converts a point in game space to a rect on the screen"
  [pt]
  (map #(* point-size % )
       [(pt 0) (pt 1) 1 1]))

(defn create-apple []
  {:location [(rand-int board-width) (rand-int board-height)]
   :color (Color. 210 50 80)
   :type :apple})

(defn create-snake []
  {:body (list [1 1])
   :dir [1 0]
   :color (Color. 15 160 70)
   :type :snake})

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
  (not (and
         (< 0 (head 0) board-width)
         (< 0 (head 1) board-height))))

(defn lose-game? [snake]
  (or (head-crashed-body? snake)
    (head-crashed-wall? snake)))

(defn eats-apple? [{[head] :body} {apple-location :location}]
  (= head apple-location))

(defn turn-snake [snake newdir]
  (assoc snake :dir newdir))



