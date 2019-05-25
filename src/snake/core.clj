(ns snake.core
  (:import (java.awt Color Dimension Font)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class))

;;; constants

(def c-width   "number of horizontal elements on the court" 20)
(def c-height  "number of vertical elements on the court"   20)
(def e-size    "size of an element in pixels"               30)
(def i-quantum "initial duration of repainting period"      100)
(def d-quantum "change of the duration for two succ levels" -5)
(def m-quantum "limit for the duration"                     50)
(def i-length  "initial length for the snake to win"        5)
(def d-length  "change of the length for two succ levels"   3)
(def p-width   "width of the game panel"   (* c-width e-size))
(def p-height  "height of the game panel" (* c-height e-size))
(def dirs      "mapping from even code to direction"
  {KeyEvent/VK_LEFT  [-1  0]
   KeyEvent/VK_RIGHT [ 1  0]
   KeyEvent/VK_UP    [ 0 -1]
   KeyEvent/VK_DOWN  [ 0  1]})

;;; colors

(def color-variation  0)
(def background-color (Color/BLACK))
(def text-color       (Color/GREEN))

;;; pure section

(defn quantum [level]
  "Evaluates period of repainting based on level."
  (max (+ i-quantum (* level d-quantum)) m-quantum))

(defn length [level]
  "Evaluates length of the snake that will cause win."
  (+ i-length (* level d-length)))

; the function is called every period
(def length (memoize length))

(defn add-points [[x0 y0] [x1 y1]]
  "Adds two points, used to shift head to the snake."
  [(+ x0 x1) (+ y0 y1)])

(defn move [{:keys [body dir] :as snake} grows]
  "Evaluates snake after one move."
  (assoc snake :body
         (cons (add-points (first body) dir)
               (if grows body (butlast body)))))

(defn turn [snake dir]
  (assoc snake :dir dir))

(defn win? [{body :body} level]
  "Snake wins if it is long enough for actual level."
  (>= (count body) (length level)))

(defn eats-self? [[head & tail]]
  "Evaluates true if snake do some cannibalism."
  (contains? (set tail) head))

(defn eats-border? [[[x y]]]
  "Evaluates true when snake starts to eat border!"
  (or (>= x c-width)
      (>= y c-height)
      (< x 0)
      (< y 0)))

(defn lose? [{body :body}]
  "Snake loses when it eats something but the food!"
  (or (eats-self?   body)
      (eats-border? body)))

(defn eats-food? [{[head] :body} {[food] :body}]
  "Evaluates true when snake eats the food."
  (= head food))

(defn screen-rect [[x y]]
  "Converts a pair of coordinates into x, y, width, and height of a
  rectangle on the screen."
  (map (fn [x] (* e-size x))
       [x y 1 1]))

(def screen-rect (memoize screen-rect)) ; creating 'table' of relations

;;; random values

(defn ->color [[r g b]]
  (Color. r g b))

(defn vary-component [x]
  "Varies component of color."
  (letfn [(pm [x] [(rand-int x) (rand-int (- x))])]
    (let [x (apply + x (pm color-variation))]
      (cond (> x 255) 255
            (< x 0)   0
            :else     x))))

(defn vary-color [color]
  "Varies given color."
  (->color (map vary-component color)))

(defn new-snake []
  "Creates brand new snake."
  {:body  (list [1 1])
   :dir   [1 0]
   :color (seq [255 255 255])})

(defn new-food-for [{color :color}]
  "Creates brand new food."
  {:body  [[(rand-int c-width)
            (rand-int c-height)]]
   :color (seq [0 200 20])})

;;; non-pure section

(defn reset-game [snake food pause]
  "Resets game."
  (dosync
   (ref-set snake (new-snake))
   (ref-set food (new-food-for @snake))
   (ref-set pause true))
  nil)

(defn update-dir [snake dir]
  "Updates direction of snake."
  (when dir
    (dosync (alter snake turn dir))))

(defn update-pos [snake food]
  "Updates positions of snake and food."
  (dosync
   (if (eats-food? @snake @food)
     (do (ref-set food (new-food-for @snake))
         (alter   snake move true))
     (alter snake move false)))
  nil)

;;; GUI

(defn paint [g {:keys [body color]}]
  "Paints constructions like snake or food."
  (doseq [[x y w h] (map screen-rect body)]
    (doto g
      (.setColor (vary-color color))
      (.fillRect x y w h))))

(defn show-text [g title subtitle sub2]
  "Shows some text: title and subtitle."
  (doto g
    (.setColor text-color)
    (.setFont (Font. "Tahoma" Font/TRUETYPE_FONT 35))
    (.drawString title 250 252)
    (.setFont (Font. "Arial" Font/TRUETYPE_FONT 20))
    (.drawString subtitle 205 298)
    (.drawString sub2 245 318)))

(defn game-panel [snake food level pause timer]
  "Creates game panel."
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (if @pause
        (show-text g (str "(Level " @level ")")
                   "(when (press-any-key)" "(dosync (continue))")
        (do (paint g @snake)
            (paint g @food))))
    (actionPerformed [e]
      (when-not @pause
        (update-pos snake food))
      (when (lose? @snake)
        (reset-game snake food pause))
      (when (win? @snake @level)
        (swap! level inc)
        (reset-game snake food pause)
        (.setDelay timer (quantum @level)))
      (.repaint this))
    (keyPressed [e]
      (if @pause
        (dosync (ref-set pause false))
        (update-dir snake (dirs (.getKeyCode e)))))
    (windowClosed []
      (System/exit 0))
    (keyReleased [e])
    (keyTyped    [e])))

(defn game []
  "Creates some stuff and starts the game."
  (let [snake  (ref (new-snake))
        food  (ref (new-food-for @snake))
        level  (atom 0)
        pause  (ref true)
        frame  (JFrame. "Snake")
        timer  (Timer. (quantum @level) nil)
        panel  (game-panel snake food level pause timer)]
    (doto panel
      (.setFocusable   true)
      (.addKeyListener panel)
      (.setBackground  background-color)
      (.setPreferredSize (Dimension. p-width p-height)))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setResizable false)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setLocationRelativeTo nil))
    (doto timer
      (.addActionListener panel)
      (.start))
    [snake food level timer]))

(defn -main [& args]
  "The main function."
  (game))
