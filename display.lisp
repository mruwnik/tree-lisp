;;;; display.lisp

(in-package #:tree-sim)

;;; tree drawing stuff
(defparameter *wind-strength* 100)

(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *v-sensitivity* 3)
(defparameter *h-sensitivity* 3)
(defparameter *movement-step* 1)

;; The projection matrix. 
(defparameter *projection* nil)

(defparameter *tree-node* nil)

;; Lighting values
;; ambientLight   The lowest amount of light to use. An RGB value.
(defparameter ambientLight '(.2 .2 .2))

;; lightIntensity The maximum power of the light.    An RGB value.
(defparameter lightIntensity '(.8 .8 .8))

;; lightDirection The direction of the light source. An XYZ normal value.
(defparameter lightDirection '(0.5772705 -0.5772705 -0.5772705))
(defparameter lightDirection '(0.0 -1.0 0.0))


;; Create a window-resized event handler 
(clinch:defevent clinch:*on-window-resized* (win width height ts)
  (format t "Window Resized: ~A ~A~%" width height)
  (setf *projection* (clinch::make-perspective-transform (clinch:degrees->radians 45)
							 (/ width height) .1 1000)))


;; Run this once before the next on-idle call.
(clinch:defevent clinch:*next* ()

  (format t "initialization!~%")

  ;; Set the window's clear color. I like blue.
  (gl:clear-color 0 0 0 0)

  ;; Enable a few opengl features. 
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d)

  ;; Set the blending mode.
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)

  ;; create the triangle entity.
  (setf *leaf* (leaf-model ambientlight lightintensity lightdirection))
  (setf *segment* (cylinder ambientlight lightintensity lightdirection))
  (setf *tree-node*
        (make-instance 'clinch:node
                       :children (list (draw-part *tree* *dna*))
                       :translation (clinch:v! 0 -1.25 -2))))

(clinch:defevent clinch:*on-mouse-move* (win mouse state x y xrel yrel ts)
  (format t "x:~A y:~A mouse:~A state:~A~%" x y mouse state)
  (case state
    (1 (clinch:rotate *tree-node*
		      (q:from-fixed-angles (clinch:degrees->radians yrel) (clinch:degrees->radians xrel) 0.0)))
    (2 (clinch:translate *tree-node* (clinch:v! (/ xrel 2) (/ yrel -2) 0)))))

(clinch:defevent clinch:*on-mouse-wheel-move* (win mouse x y ts)
  ;;(format t "win=~A mouse=~A x=~A y=~A ts=~A~%" win mouse x y ts)
  (clinch:translate *tree-node* (clinch:v! 0 0 (/ y 1))))


(let ((last-tick 0))
  (defun render-tree (tree dna)
    "Render the given tree. This will regenerate the whole model if the time changes."
    (unless (= *now* last-tick)
      (setf last-tick *now*)
      (setf (clinch:children *tree-node*) (list (or (draw-part tree dna) *cube*))))
    (when *tree-node*
      (clinch:render *tree-node* :projection *projection*))))

;; Create an on-idle envent handler.
(clinch:defevent clinch:*on-idle* ()
  ;; clear the window
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; render the tree - this will use a buffered version, unless something changes
  (render-tree *tree* *dna*)
)

;; Start the window.
(clinch:init :init-controllers nil)
(when nil
    (clinch:uninit))
