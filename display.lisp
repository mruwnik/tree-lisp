;;;; tree.lisp

(in-package #:tree)

;(ql:quickload "cl-glu")
;(ql:quickload "cl-glut")

;;; tree drawing stuff
(defparameter *wind-strength* 100)

(defgeneric draw-part(part dna)
 (:documentation "draws the given part and all its subparts"))

(defmethod draw-part(part dna)
  "default method in case a NIL object is tried to be drawn")

(defmethod draw-part((bud bud) dna)
  (if (> (health bud) 0)
      (gl:color 0.547059 0.264706 0.0064706)
;glColor3f(0.647059 * getHealth()/1000,0.164706 * getHealth()/1000,0.164706 * getHealth()/1000);
      (gl:color 0 0 0))
  (glut:solid-sphere 0.1 20 20)
  (when (leaf bud)
    (gl:push-matrix)
    (when (> *wind-strength* 1)
      (gl:rotate (- (/ *wind-strength* 10)
		    (random (ceiling (/ *wind-strength* 5))))
		 (/ (random *wind-strength*) *wind-strength*) 
		 (/ (random *wind-strength*) *wind-strength*)
		 (/ (random *wind-strength*) *wind-strength*)))
    (gl:rotate (bud-sprout-angle dna) 0 0 1.0)
    (if (is-dead (leaf bud))
	(gl:color 0.357059 0.134706 0.0044706)
	(gl:color 0 0.5 0))
    (let* ((xr (/ (width (leaf bud)) 2))
	   (xl (- 0 xr))
	   (l (- (leaf-len (leaf bud)))))
      (gl:with-primitives :quads      ; start drawing quadrilaterals
	(gl:vertex -0.1 xl l)    ; top-left vertex
	(gl:vertex -0.1 xr l)    ; top-right vertex
	(gl:vertex -0.1 xr 0)    ; bottom-right vertex
	(gl:vertex -0.1 xl 0)))   ; bottom-left vertex    
     (gl:pop-matrix)))

(defmethod draw-part :around((part tip) dna)
  (gl:color 0.647059 0.164706 (* 0.164706 (auxin (supplies part))))
  (draw-prism (width part) (height part))
  (call-next-method))
(defmethod draw-part((part segment) dna)
  (gl:push-matrix)
  (reposition)
  (gl:translate (first (end part)) (second (end part)) (third (end part)))
  (gl:color 0 0 1)
  (glut:solid-sphere 0.2 20 20)
  (gl:pop-matrix)
  (gl:translate 0 (height part) 0)
  (draw-part (apex part) dna)
  (let ((angle 0)
	(angle-step (/ 360 (segment-buds dna))))
    (dolist (bud (buds part))
      (gl:rotate angle 0 1 0) 
      (gl:rotate (- (bud-sprout-angle dna)) 0 0 1.0)
      (gl:rotate 90 0 0 1.0) 
      (gl:translate 0 0 (- (width part)))
      (draw-part bud dna)
    
     ; undo the transformations - glPush (and pop) Matrix are problematic 
     ; because the stack is limited
      (gl:translate 0 0 (width part))
      (gl:rotate -90 0 0 1.0)
      (gl:rotate (bud-sprout-angle dna) 0 0 1.0) 
      (gl:rotate (- angle) 0 1 0)
      (incf angle angle-step)))
  (gl:translate 0 (- (height part)) 0))

;;; "tree" goes here. Hacks and glory await!

(defparameter *x* 0)
(defparameter *y* -1)
(defparameter *z* -16)

(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *v-angle* 0)
(defparameter *h-angle* 0)
(defparameter *v-sensitivity* 0.2)
(defparameter *h-sensitivity* 0.2)
(defparameter *movement-step* 1)

(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (width :initarg width :initform 400 :accessor width)
   (height :initarg height :initform 300 :accessor height))
  (:default-initargs :width 400 :height 300
                     :title "tut02: triangles and quads"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil))

(defparameter *window* (make-instance 'my-window))

(defmethod glut:display-window :before ((win my-window))
  (gl:shade-model :smooth)        ; enables smooth shading
  (gl:clear-color 0 0 0 0)        ; background will be black
  (gl:clear-depth 1)              ; clear buffer to maximum depth
  (gl:enable :depth-test)         ; enable depth testing
  (gl:depth-func :lequal)         ; okay to write pixel if its depth
                                  ; is less-than-or-equal to the
                                  ; depth currently written
                                  ; really nice perspective correction
  (gl:hint :perspective-correction-hint :nicest)

  (when (fullscreen-p win)        ; check to see if fullscreen needed
    (glut:full-screen))           ; if so, then tell GLUT
)

(defmethod glut:reshape ((win my-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:perspective 45 (/ width (max height 1)) 1/10 500)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity)              ; reset the matrix
)

(defun reposition()
  (gl:matrix-mode :MODELVIEW)
  (gl:load-identity);
  (gl:rotate *h-angle* 0 1 0)
  (gl:rotate *v-angle* 1 0 0)
  (gl:translate *x* *y* *z*))

(defmethod glut:mouse (window button state X Y)
  (print Y))

(defmethod glut:passive-motion (window x y)
  (setf *h-angle* (+ *h-angle* 
		     (* (- x *mouse-x*)
			*h-sensitivity*)))
  (setf *v-angle* (+ *v-angle* 
		     ( * (- y *mouse-y*)
			 *v-sensitivity*)))
  (when (< *v-angle* -90)
    (setf *v-angle* -90))
  (when (> *v-angle* 90)
    (setf *v-angle* 90))

  (setf *mouse-x* x)
  (setf *mouse-y* y)
  (reposition))
 
(defmethod glut:keyboard ((win my-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\w) (incf *z* *movement-step*) (reposition))
    ((#\s) (decf *z* *movement-step*) (reposition))
    ((#\a) (incf *x* *movement-step*) (reposition))
    ((#\d) (decf *x* *movement-step*) (reposition))
    ((#\q #\Q #\Escape) (glut:close win))
    ((#\f #\F)                      ; when we get an 'f'
                                    ; save whether we're in fullscreen
         (let ((full (fullscreen-p win)))
           (glut:close win)         ; close the current window
           (glut:display-window     ; open a new window with fullscreen toggled
               (make-instance 'my-window
                              :fullscreen (not full)))))))

(defmethod glut:keyboard-up ((win my-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) t)))

(defun draw-prism (radius length &optional (gons 6))
  (gl:with-primitives :quad-strip
    (gl:vertex  (- radius)  0  0.0)    ; top vertex
    (gl:vertex  (- radius)  length 0.0)    ; top vertex
    (gl:vertex (* radius -0.5) 0 (* radius (/ (sqrt 3) 2)))
    (gl:vertex (* radius -0.5) length (* radius (/ (sqrt 3) 2)))
    (gl:vertex (* radius 0.5) 0 (* radius (/ (sqrt 3) 2)))
    (gl:vertex (* radius 0.5) length (* radius (/ (sqrt 3) 2)))
    (gl:vertex radius 0 0)
    (gl:vertex radius length 0)
    (gl:vertex (* radius 0.5) 0 (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (* radius 0.5) length (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (* radius -0.5) 0 (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (* radius -0.5) length (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (- radius) 0 0)
    (gl:vertex (- radius) length 0)))

(defun display ()
  (glut:schedule-timer 100 #'display)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (gl:push-matrix)

  (gl:color 0.2 0.41 0)
  (gl:with-primitives :quads      ; start drawing quadrilaterals
    (gl:vertex -10000.0 0  10000.0)    ; top-left vertex
    (gl:vertex  10000.0 0  10000.0)    ; top-right vertex
    (gl:vertex  10000.0 0 -10000.0)    ; bottom-right vertex
    (gl:vertex -10000.0 0 -10000.0))   ; bottom-left vertex    
  
  (gl:color 0.647059 0.164706 0.164706)

  (draw-part *tree* *dna*)
;  (gl:with-primitives :triangles  ; start drawing triangles
;    (gl:vertex  0.0  1.0  0.0)    ; top vertex
;    (gl:vertex -1.0 -1.0  0.0)    ; bottom-left vertex
;    (gl:vertex  1.0 -1.0  0.0))   ; bottom-right vertex
;  (gl:translate 3.0 0.0 0.0)      ; translate right

  (gl:pop-matrix)
  (glut:swap-buffers))

(progn 
  (glut:schedule-timer 100 #'display)
  (glut:display-window *window*))

(dotimes (i 100)
  (diffuse *supplies* *tree* *dna*)
  (health-check *tree* *dna*)
  (grow *tree* *dna*)
)
