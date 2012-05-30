(ns flocking.core
  (:use quil.core))

(def NUM-BOIDS 10)
(def CANVAS 800)
(def INIT-SPEED 2)
(def COHESION 0.001)
(def ALIGN 0.1)
(def MAX_SPEED 5)

(defn make-boid
  []
  {:x  (rand-int CANVAS)
   :y  (rand-int CANVAS)
   :dx (rand-int INIT-SPEED)
   :dy (rand-int INIT-SPEED)})

(defn setup []
  (smooth)
  (frame-rate 30)
  (set-state!   :mousepos (atom [(/ CANVAS 2) (/ CANVAS 2)])
                :boids (atom (take NUM-BOIDS (repeatedly make-boid))))
  (background 200))
  
(defn bound
    [val]
    (if (> val MAX_SPEED) MAX_SPEED (if (< val (- MAX_SPEED)) (- MAX_SPEED) val)))

(defn update-boids
  [boids]
  (let [avg-x (/ (reduce + (map :x boids)) NUM-BOIDS)
        avg-y (/ (reduce + (map :y boids)) NUM-BOIDS)
        avg-dx (/ (reduce + (map :dx boids)) NUM-BOIDS)
        avg-dy (/ (reduce + (map :dy boids)) NUM-BOIDS)
        ]
    (doall
      (for [{:keys [x y dx dy] :as b} boids]
        (let [c-dx (* (- avg-x x) COHESION)
              c-dy (* (- avg-y y) COHESION)
              a-dx (* (- avg-dx dx) ALIGN)
              a-dy (* (- avg-dy dy) ALIGN)
              m-dx (-> @(state :mousepos) (first) (- x) (* COHESION))
              m-dy (-> @(state :mousepos) (second) (- y) (* COHESION))
              dx   (bound (+ dx c-dx m-dx))
              dy   (bound (+ dy c-dy m-dy))]
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

(defn mouse-moved []
    (let [  x (mouse-x)
            y (mouse-y)]
            (reset! (state :mousepos) [x y])))

(defsketch example
  :title "Flocking"
  :setup setup
  :mouse-moved mouse-moved
  :draw draw :size [CANVAS CANVAS])

