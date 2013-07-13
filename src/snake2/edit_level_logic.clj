(ns snake2.edit-level-logic
  (:use snake2.data-manager))

(def ^:private wall-points (atom []))

(defn selected-points []
  @wall-points)

(defn select-point! [point]
  (swap! wall-points conj point))

(defn clear-all-points []
  (reset! wall-points []))

(defn save-level [filename]
  (serialize @wall-points filename))