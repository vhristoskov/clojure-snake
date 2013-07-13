(ns snake2.edit-level-gui
  (:use [seesaw core color graphics behave]
        (snake2 constants edit-level-logic)))


;(def board-margin 10)

(defn point-to-screen
  "Converts a point in game space to a rect on the screen"
  [point]
  (map #(* point-size % )
    [(point 0) (point 1) 1 1]))

(defn point-from-screen
  "Converts a point from the screen space to the game space"
  [point]
  (letfn [(project [n] (int (/ n point-size)))]
    [(-> point .x project)
     (-> point .y project)]))

(defn draw-board [c g]
  (doseq [point (selected-points)]
    (draw g (apply rect (point-to-screen point)) (style :background :brown))))

(defn redraw [root]
  (config! (select root [:.board]) :paint draw-board))

(declare board-frame)
(defn make-content-panel []
  (let [open-action (action
                      :handler (fn [e] (alert "I should open a new something."))
                      :name "Open"
                      :tip  "Open a new level.")
        save-action (action
                      :handler (fn [e] (when-let [filename (input "File name:" :value "newLevel.txt")]
                                         (save-level filename)))
                      :name "Save"
                      :tip  "Save the level model")
        clear-action (action
                      :handler (fn [e] (clear-all-points) (redraw board-frame))
                      :name "Clear"
                      :tip  "Clear the working area")
        exit-action (action
                      :handler (fn [e] (.dispose (to-frame e)))
                      :name "Exit"
                      :tip  "Close this window")]
    (border-panel
      :north (toolbar :items [open-action save-action clear-action exit-action])
      :center (canvas :paint draw-board
                      :class :board
                      :background :gray)
      :vgap 5
      :hgap 5
      :border 5)))

(defn make-frame []
  (frame :title "Snake Level Editor"
         :size [600 :by 600]
         :content (make-content-panel)
         :on-close :exit))

(defonce board-frame (make-frame))

(defn start-level-editor []
  (native!)
  (-> board-frame pack! show!)
  (listen (select board-frame [:.board]) :mouse-clicked (fn [e] (select-point! (point-from-screen (.getPoint e)))
                                                                  (redraw board-frame)))

  (listen (select board-frame [:.board]) :mouse-dragged (fn [e] (select-point! (point-from-screen (.getPoint e)))
                                                          (redraw board-frame)))
  nil)