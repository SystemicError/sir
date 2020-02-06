(ns sir.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def population 256)
(def step-scale 0.2)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (q/background 0)
  (for [i (range 0 population 10)]
    (let [s (- population i)
          i 1
          r (- population s i)]
      {:s s
       :i 1
       :r (- population s i)
       :beta 0.9
       :gamma 0.1
       :color [s i r]})))

(defn update-point [point]
  (let [s (:s point)
        i (:i point)
        r (:r point)
        beta (:beta point)
        gamma (:gamma point)]
    (assoc point
           :s (+ s (* step-scale (/ (* -1.0 beta s i) population)))
           :i (+ i (* step-scale (- (/ (* beta s i) population) (* gamma i))))
           :r (+ r (* step-scale (* gamma i))))))

(defn update-state [state]
  (map update-point state))

(defn rotate-point [point]
  (let [s (:s point)
        i (:i point)
        r (:r point)
        color (:color point)
        x (+ s (* r -0.366)) ; 1-cos(15)sqrt(2)
        y (+ i (* r -0.366))]
    {:x x
     :y y
     :color color}))


(defn draw-points [points]
  (if (not (empty? points))
    (let [point (first points)
          x (:x point)
          y (:y point)]
      (q/stroke (:color point))
      (q/ellipse x y 3 3)
      (recur (rest points)))))

(defn draw-state [state]
  ; Set circle color.
  (q/fill 255 255 255)
  ; set draw color
  (q/stroke 200 200 200)
  ; Calculate x and y coordinates of the circle.
  (let [points (map rotate-point state)
        dummy (println (str "state" (into [] state) "\n"
                         "Points" (into [] points)))]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(+ (/ (q/width) 2) (* -0.211 population))
                         (+ (/ (q/height) 2) (* -0.211 population))]
      ; draw bounds of triangle
      (q/line 0 population population 0)
      (q/line 0 population (* -0.366 population) (* -0.366 population))
      (q/line population 1 (* -0.366 population) (* -0.366 population))
      (q/text "Infected" 0 (* 1.1 population))
      (q/text "Susceptible" (* 1.1 population) 0)
      (q/text "Removed" (* 1.1 -0.366 population) (* 1.1 -0.366 population))
      ; Draw the circle.
      (draw-points points))))


(q/defsketch sir
  :title "SIR Compartmental Model"
  :size [512 512]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
