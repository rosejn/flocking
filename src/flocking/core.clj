(ns flocking.core
  (:use quil.core))

(def NUM-BOIDS 10)
(def CANVAS 800)
(def INIT-SPEED 2)
(def COHESION 0.01)

(defn make-boid
  []
  {:x  (rand-int CANVAS)
   :y  (rand-int CANVAS)
   :dx (rand-int INIT-SPEED)
   :dy (rand-int INIT-SPEED)})

(defn setup []
  (smooth)
  (frame-rate 30)
  (set-state! :boids (atom (take NUM-BOIDS (repeatedly make-boid))))
  (background 200))

(defn update-boids
  [boids]
  (let [avg-x 400 ;(/ (reduce + (map :x boids)) NUM-BOIDS)
        avg-y 400] ;(/ (reduce + (map :y boids)) NUM-BOIDS)]
    (doall
      (for [{:keys [x y dx dy] :as b} boids]
        (let [c-dx (* (- avg-x x) COHESION)
              c-dy (* (- avg-y y) COHESION)
              dx   (+ dx c-dx)
              dy   (+ dy c-dy)]
        (assoc b
               :x (mod (+ x dx) CANVAS)
               :y (mod (+ y dy) CANVAS)
               :dx dx
               :dy dy))))))

(defn draw []
  (background 100)
  (swap! (state :boids) update-boids)
  (fill 255 50 50)

  (doseq [{:keys [x y]} @(state :boids)]
    (triangle x y (+ x 10) (+ y 10) (+ x 10) (- y 10))))


(defsketch example
  :title "Flocking"
  :setup setup
  :draw draw :size [CANVAS CANVAS])

