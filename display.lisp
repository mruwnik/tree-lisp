;;;; display.lisp

(in-package #:tree-sim)

;;; tree drawing stuff
(defparameter *wind-strength* 100)

(defparameter *camera-pos* (vector -26 7 1 1))
(defparameter *camera-angle* (quart-normalise (quarternion (/ PI 2) 1 0 0)))

(defparameter *camera-look-at* (vector -25 7 0 1))
(defparameter *light-pos* (vector 0 60 100 1))
(defparameter *light-look-at* (vector 0 15 0 ))
(defparameter *framebuffer* 0)
(defparameter *shadow-texture* 123)
(defparameter *shadow-shader* 0)
(defparameter *shadow-map* 0)

(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *shadow-map-ratio* 2)


(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *v-sensitivity* 3)
(defparameter *h-sensitivity* 3)
(defparameter *movement-step* 1)

(defparameter *draw-position* NIL)
(defparameter *draw-wind* NIL)
(defparameter *draw-tree* t)
(defparameter *draw-leaf-occulence* NIL)
(defparameter *show-help* NIL)
(defparameter *show-debug-info* NIL)

(defclass tree-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (width :initarg width :initform 400 :accessor width)
   (height :initarg height :initform 300 :accessor height)
   (shaders :initarg shaders :initform t :accessor shaders))
  (:default-initargs :width 640 :height 480
                     :title "Tree simulation"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil))

(defmethod glut:display-window :before ((win tree-window))
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

  ;; set up shaders
  (handler-case
      (setup-shaders win)
    (shader-error (se) 
      (progn 
	(setf (shaders win) NIL)
	(print (text se)))))
  (gl:enable :depth-test)
  (gl:depth-func :less)
)


(defmethod glut:reshape ((win tree-window) width height)
  (gl:viewport 0 0 width height)  ; reset the current viewport
  (gl:matrix-mode :projection)    ; select the projection matrix
  (gl:load-identity)              ; reset the matrix

  ;; set perspective based on window aspect ratio
  (glu:perspective 45 (/ width (max height 1)) 1/10 500)
  (gl:matrix-mode :modelview)     ; select the modelview matrix
  (gl:load-identity)              ; reset the matrix
  (setf *width* width)
  (setf *height* height))


(defun move-view(&optional direction)
  (when direction
    (setf *camera-pos*
	  (absolute-position 
	   *camera-pos*
	   (case direction
	     (:up (vector 0 0 1))
	     (:down (vector 0 0 -1))
	     (:left (vector 1 0 0))
	     (:right (vector -1 0 0))
	     (:forward (vector 0 1 0))
	     (:back (vector 0 -1 0)))
	   *camera-angle*)))
  (setf *camera-look-at* (absolute-position *camera-pos* (vector 0 1 0) *camera-angle*)))



(defmethod glut:mouse (window button state X Y)
  (print Y))

(defun scaled-max-abs (number max scale-by)
  (if (= number 0) 0
      (if (> (/ (abs number) scale-by) max) 
	  (* (/ (abs number) number) max)
	  (/ number scale-by))))

(defmethod glut:passive-motion (window x y)
  (setf *camera-angle* 
	(reduce 'multiply-quarts 
           (list
      	    (quart-normalise (quarternion (deg-to-rad (scaled-max-abs (- *mouse-x* x) *h-sensitivity* 5)) 0 1 0))
	    (quart-normalise (quarternion (deg-to-rad (scaled-max-abs (- y *mouse-y*) *v-sensitivity* 5)) 0 0 1))
	    *camera-angle*
		)))
  (setf *mouse-x* x)
  (setf *mouse-y* y)
  (move-view))


(defmethod glut:keyboard ((win tree-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\w) (move-view :forward))
    ((#\s) (move-view :back))
    ((#\a) (move-view :left))
    ((#\d) (move-view :right))
    ((#\e) (move-view :up))
    ((#\r) (move-view :down))
    ((#\n #\N) (reset-tree))
    ((#\y) (simulate-years 1 *dna*))
    ((#\l) (setf *sunshine* (not *sunshine*)))
    ((#\S) (setf *seasons* (not *seasons*)))
    ((#\h) (setf *health-checks* (not *health-checks*)))
    ((#\g) (setf *growth-ratio* (not *growth-ratio*)))
    ((#\G) (setf *growth* (not *growth*)))
    ((#\u) (setf *use-supplies* (not *use-supplies*)))
;    ((#\q #\Q #\Escape) (glut:close win))
    ((#\f #\F)                      ; when we get an 'f'
                                    ; save whether we're in fullscreen
         (let ((full (fullscreen-p win)))
           (glut:destroy-current-window)         ; close the current window
           (glut:display-window     ; open a new window with fullscreen toggled
               (make-instance 'tree-window
                              :fullscreen (not full)))))
    ((#\h #\H) (setf *show-help* (not *show-help*)))
    ((#\1) (setf *draw-wind* (not *draw-wind*)))
    ((#\2) (setf *draw-position* (not *draw-position*)))
    ((#\3) (setf *draw-tree* (not *draw-tree*)))
    ((#\4) (setf *draw-leaf-occulence* (not *draw-leaf-occulence*)))
    ((#\0) (setf *show-debug-info* (not *show-debug-info*)))
    ))

(defmethod glut:keyboard-up ((win tree-window) key xx yy)
  (declare (ignore xx yy))
  (case key
    ((#\q #\Q #\Escape) t)))

(defun draw-prism (radius length &optional (gons 6))
  (gl:with-primitives :quad-strip
    (gl:normal (- radius) 0 0)
    (gl:vertex  (- radius)  0  0.0)    ; top vertex
    (gl:vertex  (- radius)  length 0.0)    ; top vertex
    (gl:normal (* radius -0.5) 0 (* radius (/ (sqrt 3) 2)))
    (gl:vertex (* radius -0.5) 0 (* radius (/ (sqrt 3) 2)))
    (gl:vertex (* radius -0.5) length (* radius (/ (sqrt 3) 2)))
    (gl:normal (* radius 0.5) 0 (* radius (/ (sqrt 3) 2)))
    (gl:vertex (* radius 0.5) 0 (* radius (/ (sqrt 3) 2)))
    (gl:vertex (* radius 0.5) length (* radius (/ (sqrt 3) 2)))
    (gl:normal radius 0 0)
    (gl:vertex radius 0 0)
    (gl:vertex radius length 0)
    (gl:normal (* radius 0.5) 0 (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (* radius 0.5) 0 (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (* radius 0.5) length (* radius (- (/ (sqrt 3) 2))))
    (gl:normal (* radius 0.5) 0 (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (* radius -0.5) 0 (* radius (- (/ (sqrt 3) 2))))
    (gl:vertex (* radius -0.5) length (* radius (- (/ (sqrt 3) 2))))
    (gl:normal (- radius) 0 0)
    (gl:vertex (- radius) 0 0)
    (gl:vertex (- radius) length 0))
  (gl:normal 0 0 0))

(defun setup-shaders (win)
  (generate-shadow-fbuffer)
  (setf *shadow-shader* (load-shaders "treeShadow.vs" "treeShadow.fs"))
  (setf *shadow-map* (gl:get-uniform-location *shadow-shader* "ShadowMap"))
  (gl:enable :depth-test)
  (gl:clear-color 0 0 0 1)
  (gl:enable :cull-face)
  (gl:hint :perspective-correction-hint :nicest)
)

(defun draw-string (string x y &optional (colour '(1 0 0)))
  "Draw the given string on the screen.
  Because of the projection matrix used, x and y can be thought of as percentage offsets from the bottom left corner."
  (gl:matrix-mode :projection)
  (gl:with-pushed-matrix
    (gl:load-identity)
    (gl:ortho 0 100 0 100 -1 10)
    (gl:matrix-mode :modelview)
    (gl:with-pushed-matrix
      (gl:load-identity)
      (apply 'set-colour colour)
      (loop for line in (split-sequence:split-sequence #\newline string)
	   for i from 0 to 100 by 5 do
	   (progn
	     (gl:raster-pos x (- y i))
	     (glut:bitmap-string glut:+bitmap-8-by-13+ line)))
      (gl:matrix-mode :projection))
    (gl:matrix-mode :modelview)))

(defmacro with-report-usage (&body body)
  `(if *show-debug-info*
     (draw-string
      (with-output-to-string (*trace-output*)
	(time (progn ,@body))) 4 96)
     (progn ,@body)))


(defun show-help ()
  (draw-string 
(apply 'format NIL "h - show this help
1 - simulate wind
2 - draw position
3 - draw the tree
4 - show how much sunlight each leaf gets

y - simulate a year's growth
n - show a new tree

Options:
G - ~a growth
g - ~a growth ratio
h - ~a health checks
S - ~a seasons
l - ~a sunshine checking
u - ~a supplies checking
" (mapcar #'(lambda (x) (if x "disable" "enable"))
	  (list *growth* *growth-ratio* *health-checks* *seasons* *sunshine* *use-supplies*)))
   2 95 '(0 0 1)))


(defun render-sun (x y z &optional (w 1))
  (gl:disable :lighting)
  (gl:light :light0 :position `(,x ,y ,z ,w))
  (gl:light :light0 :ambient '(1 1 1 1))
  (gl:enable :light0)
  (gl:enable :depth-test)

  (set-colour 253/255 184/255 19/255)
  (gl:with-pushed-matrix
    (gl:translate x y z)
    (glut:solid-sphere 0.1 20 20))
  (gl:enable :lighting)

  (when *draw-position*
    (with-report-usage
      (draw-position *tree* *dna* 
		     (vector 0 0 0 0) 
		     (quart-normalise (vector 0 0 1 0)))))
)

(defun draw-cube()
  (gl:with-primitives :QUADS
    (gl:vertex  1.0 1.0 -1.0);    // Top Right Of The Quad (Top)
    (gl:vertex -1.0 1.0 -1.0);    // Top Left Of The Quad (Top)
    (gl:vertex -1.0 1.0 1.0);    // Bottom Left Of The Quad (Top)
    (gl:vertex  1.0 1.0 1.0);    // Bottom Right Of The Quad (Top)

    (gl:vertex  1.0 -1.0 1.0);    // Top Right Of The Quad (Bottom)
    (gl:vertex -1.0 -1.0 1.0);    // Top Left Of The Quad (Bottom)
    (gl:vertex -1.0 -1.0 -1.0);    // Bottom Left Of The Quad (Bottom)
    (gl:vertex  1.0 -1.0 -1.0);    // Bottom Right Of The Quad (Bottom)

    (gl:vertex  1.0 1.0 1.0);    // Top Right Of The Quad (Front)
    (gl:vertex -1.0 1.0 1.0);    // Top Left Of The Quad (Front)
    (gl:vertex -1.0 -1.0 1.0);    // Bottom Left Of The Quad (Front)
    (gl:vertex  1.0 -1.0 1.0);    // Bottom Right Of The Quad (Front)

    (gl:vertex  1.0 -1.0 -1.0);    // Top Right Of The Quad (Back)
    (gl:vertex -1.0 -1.0 -1.0);    // Top Left Of The Quad (Back)
    (gl:vertex -1.0 1.0 -1.0);    // Bottom Left Of The Quad (Back)
    (gl:vertex  1.0 1.0 -1.0);    // Bottom Right Of The Quad (Back)

    (gl:vertex -1.0 1.0 1.0);    // Top Right Of The Quad (Left)
    (gl:vertex -1.0 1.0 -1.0);    // Top Left Of The Quad (Left)
    (gl:vertex -1.0 -1.0 -1.0);    // Bottom Left Of The Quad (Left)
    (gl:vertex -1.0 -1.0 1.0);    // Bottom Right Of The Quad (Left)

    (gl:vertex 1.0 1.0 -1.0);    // Top Right Of The Quad (Right)
    (gl:vertex 1.0 1.0 1.0);    // Top Left Of The Quad (Right)
    (gl:vertex 1.0 -1.0 1.0);    // Bottom Left Of The Quad (Right)
    (gl:vertex 1.0 -1.0 -1.0)));    // Bottom Right Of The Quad (Right))

(defun draw-objects ()
; draw grass
  (gl:color 0.2 0.5 0)
  (gl:material :front :ambient '(0.2 0.41 0 1))
  (gl:with-primitives :quads      ; start drawing quadrilaterals
    (gl:normal 0 1 0)
    (gl:vertex -10000.0 0  10000.0)    ; top-left vertex
    (gl:vertex  10000.0 0  10000.0)    ; top-right vertex
    (gl:vertex  10000.0 0 -10000.0)    ; bottom-right vertex
    (gl:vertex -10000.0 0 -10000.0))   ; bottom-left vertex    

  (gl:with-pushed-matrix
    (gl:translate 10 1 0)
    (gl:color 0.4 0.4 0.4)
    (draw-cube))
  (gl:normal 0 0 0)

  (when *draw-tree*
    (with-report-usage
      (set-colour 0.647059 0.164706 0.164706)
      (draw-part *tree* *dna*))))


(defmethod glut:display ((window tree-window))  
  (gl:bind-framebuffer :framebuffer 0)
  (gl:clear-color 0.529 0.808 0.922 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ; render scene from the sun's point of view
  (gl:bind-framebuffer-ext :framebuffer-ext *framebuffer*)
  (gl:use-program 0)
  (gl:viewport 0 0 (* *width* *shadow-map-ratio*) (* *height* *shadow-map-ratio*))
  (gl:clear :depth-buffer-bit)
  
  (gl:color-mask :false :false :false :false)
  (setup-matrice *light-pos* *light-look-at*)
  (gl:enable :cull-face)
  (gl:cull-face :front)
  (draw-objects)

  ; render the scene from the camera's point of view
  (set-texture-matrix)
  (gl:bind-framebuffer-ext :framebuffer-ext 0)
  (gl:viewport 0 0 *width* *height*)
  (gl:color-mask :true :true :true :true)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (gl:use-program *shadow-shader*)
  (gl:active-texture :texture7)
  (gl:bind-texture :texture-2d *shadow-texture*)
  (gl:uniformi *shadow-map* 7)
  
  (setup-matrice *camera-pos* *camera-look-at*)
  (gl:cull-face :back)
  (draw-objects)

  (gl:disable :cull-face)
  (render-sun (x *light-pos*) (y *light-pos*) (z *light-pos*))

  (when *show-help*
    (show-help))

  (glut:swap-buffers))


(defmethod glut:idle ((window tree-window))
  (glut:post-redisplay))


(print "starting up a window")
(progn
  (defparameter *window* (make-instance 'tree-window))
  (glut:display-window *window*)
)
(print "done")

