(ns snake2.snake-gui
  (:use [seesaw core color graphics behave keymap]
        (snake2 [edit-level-gui :only (start-level-editor point-to-screen)]
                constants
                core)))


(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen pt)]
    (draw g (rect x y width height) (style :background color))
    ;  Draw Shadows for more realistic 3-D effect
    (draw g (rect x y width border-size) (style :background (.brighter color)))
    (draw g (rect x y border-size height) (style :background (.brighter color)))

    (draw g  (rect x (- y border-size) width border-size) (style :background (.darker color)))
    (draw g (rect (- x border-size) y border-size height) (style :background (.darker color)))
    (draw g (rect x (+ y height) width border-size) (style :background (.darker color)))
    (draw g (rect (+ x width) y border-size height) (style :background (.darker color)))))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defmethod paint :walls [g {:keys [coordinates color]}]
  (doseq [point coordinates]
    (fill-point g point color)))

(defn draw-board [c g]
  (doseq [point []]
    (draw g (apply rect (point-to-screen point)) (style :background :brown))))

(defn redraw [root]
  (config! (select root [:.board]) :paint draw-board))

(defn make-content-panel [snake apple walls]
  (border-panel
    :north (flow-panel :align :center
                       :items [(button :text "Exit" :class :exit)
                               (button :text "Pause/Play" :class :pause)
                               (button :text "Level Editor" :class :editor)
                               (label :text "Score: 0" :class :score)])
    :center (canvas :paint (fn [c g] (paint g @snake)
                                     (paint g @apple)
                                     (paint g @walls))
                    :class :board
                    :background :gray
                    :size [board-real-width :by board-real-height])
    :vgap 5
    :hgap 5
    :border 5))

(defn add-button-actions [root ticker]
  (listen (select root [:.exit]) :mouse-clicked (fn [e] (.dispose (to-frame e))))
  (listen (select root [:.editor]) :mouse-clicked (fn [e] (.stop ticker) (start-level-editor)))
  (listen (select root [:.pause]) :mouse-clicked (fn [e] (if (.isRunning ticker)
                                                           (.stop ticker)
                                                           (.start ticker)))))

(defn make-frame [snake apple walls]
  (frame :title "Clojure snake"
    :size [board-real-width :by board-real-height]
    :content (make-content-panel snake apple walls)
    :on-close :exit))

(defn add-keyboard-actions [root snake]
  (map-key root "UP" (fn [e] (update-dir snake (:up dirs))))
  (map-key root "DOWN" (fn [e] (update-dir snake (:down dirs))))
  (map-key root "LEFT" (fn [e] (update-dir snake (:left dirs))))
  (map-key root "RIGHT" (fn [e] (update-dir snake (:right dirs)))))

(defn start-game []
  (let [walls (ref (create-walls))
        snake (ref (create-snake))
        apple (ref (create-apple walls))
        board-frame (make-frame snake apple walls)
        ticker (timer (fn [_] (update-positions snake apple walls)
                              (config! (select board-frame [:.score]) :text (str "Score: " @current-score))
                              (when (lose-game? @snake @walls)
                                (alert "You lose! Try your skills again.")
                                (save-highscore)
                                (reset-game snake apple walls))
                              (when (win? @snake)
                                (alert "You win!")
                                (save-highscore)
                                (reset-game snake apple walls))
                              (repaint! (select board-frame [:.board])))

                     :delay game-speed-millis
                     :start? true)]
    (add-keyboard-actions board-frame snake)
    (native!)
    (-> board-frame pack! show!)
    (add-button-actions board-frame ticker))
  nil)


(defn -main [& args]
  (start-game))