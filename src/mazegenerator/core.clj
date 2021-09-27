(ns mazegenerator.core
  (:import (javax.swing JFrame JLabel)
           (java.awt.image BufferedImage)
           (java.awt Canvas Graphics Color)))

(defn maze-init [size]
  (vec (take (* size size) (repeat 0))))

(defn maze-size [maze]
  (int (Math/sqrt (count maze))))

(defn maze-index [maze x y]
  "Return the index into `maze` by the (`x`, `y`) coords"
  (+ x (* y (maze-size maze))))

(defn maze-at [maze x y]
  "Return the value of `maze` at coordinate (`x`, `y`)"
  (maze (maze-index maze x y)))

(defn maze-assoc [maze x y val]
  "Return a new maze with the `maze` at (`x`, `y`) set to `val`"
  (assoc maze (maze-index maze x y) val))

(defn maze-print [maze]
  (dotimes [y (maze-size maze)]
    (println (map #(maze-at maze % y) (range (maze-size maze))))))

(defn maze-assoc-at-seq [maze seq val]
  "Given a `maze` and a sequence of [[x y], ...] coordinates,
   return a maze with the coordinates set to `val`"
  (if (empty? seq)
    maze
    (let [[x y] (first seq)]
      (maze-assoc-at-seq (maze-assoc maze x y val) (rest seq) val))))

(defn maze-clamp-seq [left right top bottom seq]
  (filter #(let [[x y] %]
            (and (>= x left) (< x right) (>= y top) (< y bottom))) seq))

;; DEFERRED-- this was a bit of a dead-end it seems.
;;
;; Return a new maze with a semi-random walk from
;; (x1, y1) to (x2, y2) traced out in 1's
;; (defn maze-random-walk [maze x1 y1 x2 y1])
;; (def maze-allowed-directions [[0 1] [0 -1] [1 0] [-1 0]])
;; (repeatedly 5 #(rand-nth maze-allowed-directions))
;; (defn random-lazy-directions []
;;   (let [rand-direction #(rand-nth maze-allowed-directions)]
;;     (lazy-seq (cons (rand-direction) random-lazy-directions))))
;; (take 20 random-lazy-directions)
;; --- END DEFERRED

;; Just thinking on the problem has failed, so time to go to
;; https://en.wikipedia.org/wiki/Maze_generation_algorithm
;; ... now that I've skimmed the wikipedia page, let's try
;; implementing the recursive algorithm.
;; The idea with this one is that we recursively subdivide
;; the space into 4 regions of random height/width, then
;; we punch a hole between each region. Keep doing that until
;; further subdivisions would yield non-integer room width/heights.
;; This is definitely the easiest of all the algos so we'll start
;; with it and work up to the ones that require studying
;; up on your graph theory.

;; Routines for drawing the Maze using Java AWT
;; -- these became necessary when maze-print wasn't giving
;; me enough feedback
(def maze-grid-multiplier 20)

(defn maze-draw [maze graphics]
  (let [size (maze-size maze)]
    (doseq [x (range size) y (range size)]
      (let [xpos (* maze-grid-multiplier x)
            ypos (* maze-grid-multiplier y)
            n (+ x (* size y))]
        (if (= 1 (nth maze n))
          (.setColor graphics Color/blue)
          (.setColor graphics Color/white))
        (.fillRect graphics xpos ypos maze-grid-multiplier maze-grid-multiplier)))))

;; shamelessly cribbed & mod'ed from the good folks at
;; https://nakkaya.com/2009/09/28/fractals-in-clojure-fractal-fern/
(defn draw [maze]
  (let [size-mult (* maze-grid-multiplier (maze-size maze))
        image  (BufferedImage. size-mult size-mult BufferedImage/TYPE_INT_RGB)
        ;; TODO learn what proxy and paint do
        canvas (proxy [JLabel] []
                 (paint [g]
                   (.drawImage g image 0 0 this)))
        graphics (.createGraphics image)]
    (maze-draw maze graphics)
    (doto (JFrame.)
      (.add canvas)
      (.setSize size-mult size-mult)
      (.show))))


;; some utility funcs, moved out of maze-generation
(defn vline [x y1 y2]
  "Given an x coordinate and starting y1 y2 coords, return
   a seq of points along the vertical line formed between y1-y2"
  (map #(conj [] x %) (range y1 y2)))

(defn hline [y x1 x2]
    "Given a y coordinate and starting x1 x2 coords, return
   a seq of points along the horizontal line formed between y1-y2"
  (map #(conj [] % y) (range x1 x2)))

(defn midpoint [l u]
  (int (+ l (/ (- u l) 2))))

(defn rand-between [l u]
  "random number between `l` (inclusive) and `u` (exclusive)"
  (let [lower (min l u)
        upper (max l u)]
    (+ lower (rand-int (- upper lower)))))

(defn rand-between-except [lower upper except]
  (first (shuffle (vec (apply disj (set (range lower upper)) except)))))

(defn shrink-coords
  ([lx rx ty dy] (shrink-coords lx rx ty dy 1))
  ([lx rx ty dy delta]
   [(+ lx delta) (- rx delta) (+ ty delta) (- dy delta)]))

(defn render-room-to-maze
  "Given the `maze` vector, set to `wall-value` the boundary of the rect specified
   by `:top-left` and `:bottom-right` in `room` and to `door-value` the `:doors`
   specified in `room`. Note that the `:top-left` coordinate is inside the room and
   the `:bottom-right` coordinate is a part of the wall (still questioning this choice).
   The whole wall region will be overwritten so it's necessary to specify all `:doors`"
  ([maze room] (render-room-to-maze maze room 1 0))
  ([maze room wall-value door-value]
   (let [{top-left :top-left
          bottom-right :bottom-right
          doors :doors} room
         [top-left-x top-left-y] top-left
         [bottom-right-x bottom-right-y] bottom-right
         x1 (dec top-left-x) ;; the coords for room are the inner part of the room
         x2 bottom-right-x
         y1 (dec top-left-y)
         y2 bottom-right-y
         top-line (hline y1 x1 x2)
         bottom-line (hline y2 x1 x2)
         left-line (vline x1 y1 y2)
         right-line (vline x2 y1 (inc y2))
         all-lines (concat top-line bottom-line right-line left-line)
         maze-with-border (maze-assoc-at-seq maze all-lines wall-value)
         maze-with-border-and-doors (maze-assoc-at-seq maze-with-border doors door-value)]
     maze-with-border-and-doors)))

(defn room-height [room] (- (second (room :bottom-right)) (second (room :top-left))))
(defn room-width [room] (- (first (room :bottom-right)) (first (room :top-left))))

(defn door-borders-room? [door top-left bottom-right]
  (let [[x y] door
        [top-left-x top-left-y] top-left
        [bottom-right-x bottom-right-y] bottom-right]
    (or (and (= x (dec top-left-x))
             (>= y top-left-y)
             (< y bottom-right-y))
        (and (= x bottom-right-x)
             (>= y top-left-y)
             (< y bottom-right-y))
        (and (= y (dec top-left-y))
             (>= x top-left-x)
             (< x bottom-right-x))
        (and (= y bottom-right-y)
             (>= x top-left-x)
             (< x bottom-right-x)))))

(defn is-valid-room? [room]
  "A valid room has `:top-left` strictly < `:bottom-right` and doors bounding this region"
  (let [[top-left-x top-left-y] (room :top-left)
        [bottom-right-x bottom-right-y] (room :bottom-right)
        doors (room :doors)]
    (and (< top-left-x bottom-right-x)
         (< top-left-y bottom-right-y)
         (> top-left-x 0)
         (> top-left-y 0)
         (every? #(door-borders-room? % (:top-left room) (:bottom-right room)) doors))))

(defn maze-generation-recursive-2
  ([size] (maze-generation-recursive-2 size (* size size)))
  ([size max-iterations]
   (let [maze (maze-init size)
         dec-size (dec size)
         ;; simplifying somewhat by only having entry/exit top/bottom
         initial-doors [[(rand-between 1 dec-size) 0] [(rand-between 1 dec-size) dec-size]]
         initial-room {:top-left [1, 1], :bottom-right [dec-size dec-size], :doors initial-doors }]
     (println initial-room)
     (recursively-build-room-list initial-room []))))

;; the task is to
;; 1. create up to 4 subrooms given 1 larger room
;;     and a list of already existing doors
;; 2. the divider cannot block any of hte doors
;; 3. horizontal/vertical division stops when the width/height of the resulting
;;    inner room is <= 2 (?)

;; we will approach this problem by:
;; - adding a horizontal divider if the height is >= the width
;;   - divider position is random, excluding surrounding door coordinates
;; - adding a vertical divider otherwise
;; - pasing thru a list of all doors outside the current room
;; - filtering that list to only the doors bounding the current room

(defn recursively-build-room-list [room room-list]
  (let [doors (:doors room)
        [top-left-x top-left-y] (:top-left room)
        [bottom-right-x bottom-right-y] (:bottom-right room)]
    (if (not (is-valid-room? room))
      room-list
      (if (>= (room-height room) (room-width room))
        (let [avoid-y-coords (distinct (map second doors))]
          (if (<= (room-height room) (count avoid-y-coords))
            room-list
            ;; divide horizontal & recurse
            (divide-horizontal room room-list avoid-y-coords)))

        (let [avoid-x-coords (distinct (map second doors))]
          (if (<= (room-width room) (count avoid-x-coords))
            room-list
            (divide-vertical room room-list avoid-x-coords)))))))

(defn divide-horizontal [room room-list avoid-y-coords]
  (let [doors (:doors room)
        [top-left-x top-left-y] (:top-left room)
        [bottom-right-x bottom-right-y] (:bottom-right room)
        ;; pick a y coordinate for the divider, excluding the doors
        divider-y (rand-between-except top-left-y bottom-right-y avoid-y-coords)
        ;; pick a random x coordinate on that divider line as a door
        divider-door [(rand-between top-left-x bottom-right-x) divider-y]
        ;; define the rooms on each side of the divider line
        new-doors (conj doors divider-door)
        top-left-top (:top-left room)
        bottom-right-top  [bottom-right-x divider-y]
        top-room {:top-left top-left-top
                  :bottom-right bottom-right-top
                  :doors (filter #(door-borders-room? % top-left-top bottom-right-top) new-doors)}
        top-left-bottom [top-left-x (inc divider-y)]
        bottom-right-bottom (:bottom-right room)
        bottom-room {:top-left top-left-bottom
                     :bottom-right bottom-right-bottom
                     :doors (filter #(door-borders-room? % top-left-bottom bottom-right-bottom) new-doors)}]
    (concat room-list
            (cons top-room (recursively-build-room-list top-room '()))
            (cons bottom-room (recursively-build-room-list bottom-room '())))))

(defn divide-vertical [room room-list avoid-x-coords]
  (let [doors (:doors room)
        [top-left-x top-left-y] (:top-left room)
        [bottom-right-x bottom-right-y] (:bottom-right room)
        ;; pick an x coordinate for the divider, excluding the doors
        divider-x (rand-between-except top-left-x bottom-right-x avoid-x-coords)
        ;; pick a random y coordinate on that divider line as a door
        divider-door [divider-x (rand-between top-left-y bottom-right-y)]
        ;; define the rooms on each side of the divider line
        new-doors (conj doors divider-door)
        top-left-left (:top-left room)
        bottom-right-left  [divider-x bottom-right-y]
        left-room {:top-left top-left-left
                   :bottom-right bottom-right-left
                   :doors (filter #(door-borders-room? % top-left-left bottom-right-left) new-doors)}
        top-left-right [(inc divider-x) top-left-y]
        bottom-right-right (:bottom-right room)
        right-room {:top-left top-left-right
                    :bottom-right bottom-right-right
                    :doors (filter #(door-borders-room? % top-left-right bottom-right-right) new-doors)}]
    (concat room-list
            (cons left-room (recursively-build-room-list left-room '()))
            (cons right-room (recursively-build-room-list right-room '())))))


(defn maze-generation-recursive-algorithm
  ([size] (maze-generation-recursive-algorithm size (* size size)))
  ([size max-iterations]
   (let [maze (maze-init size)
         bounding-box (concat
                       (vline 0 0 size)
                       (vline (dec size) 0 size)
                       (hline 0 0 size)
                       (hline (dec size) 0 size))
         start [(rand-between 1 (dec size)) 0]
         end [(rand-between 1 (dec size)) (dec size)]
         bounded-maze (maze-assoc-at-seq maze bounding-box 1)
         bounded-maze-with-doors (maze-assoc-at-seq bounded-maze (conj [] start end) 0)]

     (defn gen-doors [left right top bottom rx ry]
       (let [ldoor [(rand-between left rx) ry]
             rdoor [(rand-between (inc rx) right) ry]
             tdoor [rx (rand-between top ry)]
             bdoor [rx (rand-between (inc ry) bottom)]
             doors (conj [] ldoor rdoor tdoor bdoor)
             doors-clamped (maze-clamp-seq left right top bottom doors)]
         (println "rdoor " rdoor " ldoor " ldoor " bdoor " bdoor " tdoor " tdoor)
         (if (> 3 (count doors-clamped))
           (drop 1 (shuffle doors-clamped))
           (shuffle doors-clamped))))

     (defn generate-inner [m left right top bottom i]
       (let [width (- right left)
             height (- bottom top)]
         (if (or (< width 2) (< height 2) (<= i 0))
           m
           (let [rx (rand-between (+ 2 left) (- right 2))
                 ry (rand-between (+ 2 top) (- bottom 2))
                 ;; rx (midpoint left right)
                 ;; ry (midpoint top bottom)
                 vdiv (vline rx top bottom)
                 hdiv (hline ry left right)
                 divs (concat vdiv hdiv)
                 doors (gen-doors left right top bottom rx ry)]
             (println "left " left " right " right " ry " ry " rx " rx)
             ;; (println vdiv)
             ;; (println divs)
             ;; (maze-print (maze-assoc-at-seq m divs 1))
             ;; (println " doors " doors)
             ;; (maze-print (maze-assoc-at-seq m doors 1))
             (let [cur-maze (maze-assoc-at-seq (maze-assoc-at-seq m divs 1) doors 0)]
               (maze-print cur-maze)
               (let [top-left-coords (shrink-coords left rx top ry 0)
                     top-left-maze (apply generate-inner cur-maze (conj top-left-coords (dec i)))
                     top-right-coords (shrink-coords (inc rx) right top ry 0)
                     top-right-maze (apply generate-inner top-left-maze (conj top-right-coords (- i 2)))
                     bottom-left-coords (shrink-coords left rx (inc ry) bottom 0)
                     bottom-left-maze (apply generate-inner top-right-maze (conj bottom-left-coords (- i 3)))
                     bottom-right-coords (shrink-coords (inc rx) right (inc ry) bottom 0)
                     bottom-right-maze (apply generate-inner bottom-left-maze (conj bottom-right-coords (- i 4)))]
                 bottom-right-maze))
             ))))
     (generate-inner bounded-maze-with-doors 1 (dec size) 0 (dec size) max-iterations))))

(def m2 (maze-generation-recursive-algorithm 21 2))
(draw m2)
(def m (maze-init 10))
(maze-print m)
(maze-at m 9 1)
(maze-at (maze-assoc m 9 1 1) 9 1)
(maze-print (maze-assoc-at-seq m [[1 1] [2 2] [3 3] [4 4]] 1))
(maze-print (maze-assoc m 9 0 1))

;; Reflections / TODOs
;; - It might have been / still be better to model Points as maps {:x 1 :y 2}
;; - You're missing basic math ops like translating a Point by a vector, scaling a Rectangle, etc
;;   that might be useful here.
;;   - In part this seems to be a failure to recognize these core types and write utility funcs for them
;;      .... OTOH maybe these aren't hte important types? Point yes, Rect less clear


;; All of the icns/decs seem like a red flags
;; Got fixated on the grid-based model I started with, didn't build higher order data types
;; Feedback from maze-print and draw are helpful at seeing what is wrong, course correcting
;; the details but not so much the big picture


;; some kind of path-dependence inside the REPl that going to pen & paper breaks

;; Don't like how the initial condition seems so much different from the inner recursion
