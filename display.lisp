;;;; display.lisp

(in-package #:tree-sim)

;;; tree drawing stuff
(defparameter *wind-strength* 100)

(defun set-colour (r g b &optional (alpha 1))
  (gl:material :front :specular `(,r ,g ,b ,alpha))
  (gl:material :front :ambient `(,r ,g ,b 0.1))
  (gl:material :front :shininess 1)
  (gl:color r g b))


(defmacro with-saved-matrix (matrix &body body)
  (let ((matrix-name (gensym)))
    `(let ((,matrix-name (gl:get-double ,matrix)))
       ,@body
       (gl:load-identity)
       (gl:mult-matrix ,matrix-name))))

(defgeneric draw-part(part dna)
 (:documentation "draws the given part and all its subparts"))

(defmethod draw-part(part dna)
  "default method in case a NIL object is tried to be drawn")

(defmethod draw-part((bud bud) dna)

  (if (> (health bud) 0)
      (set-colour 0.547059 0.264706 0.0064706)
;glColor3f(0.647059 * getHealth()/1000,0.164706 * getHealth()/1000,0.164706 * getHealth()/1000);
      (set-colour 0 0 0))
;  (glut:solid-sphere 0.1 20 20)
  (when (leaf bud)
    (gl:with-pushed-matrix
      (when (and *draw-wind* (> *wind-strength* 1))
	(gl:rotate (- (/ *wind-strength* 10)
		      (random (ceiling (/ *wind-strength* 5))))
		   (/ (random *wind-strength*) *wind-strength*) 
		   (/ (random *wind-strength*) *wind-strength*)
		   (/ (random *wind-strength*) *wind-strength*)))
      (if *draw-leaf-occulence*
	  (cond
	    ((> (in-sun (leaf bud)) 0.9) (set-colour 1 1 1))
	    ((> (in-sun (leaf bud)) 0.8) (set-colour 1 0 0))
	    ((> (in-sun (leaf bud)) 0.7) (set-colour 1 0.5 0))
	    ((> (in-sun (leaf bud)) 0.6) (set-colour 1 1 0))
	    ((> (in-sun (leaf bud)) 0.5) (set-colour 0 1 0))
	    ((> (in-sun (leaf bud)) 0.4) (set-colour 0 0 1))
	    ((> (in-sun (leaf bud)) 0.3) (set-colour (/ 75 255) 0 (/ 130 255)))
	    ((> (in-sun (leaf bud)) 0.2) (set-colour (/ 139 255) 0 1))
	    (T (set-colour 0 0 0)))
	  (if (is-dead (leaf bud))
	      (set-colour 0.357059 0.134706 0.0044706)
	      (set-colour 0 0.5 0)))
      (let* ((xr (/ (width (leaf bud)) 2))
	     (xl (- xr))
	     (l (- (leaf-len (leaf bud)))))
	(gl:disable :cull-face)
	(gl:with-primitives :quads      ; start drawing quadrilaterals
	  (gl:vertex xl 0 l)    ; top-left vertex
	  (gl:vertex xr 0 l)    ; top-right vertex
	  (gl:vertex xr 0 -0.1)    ; bottom-right vertex
	  (gl:vertex xl 0 -0.1))
	 (gl:enable :cull-face)))))   ; bottom-left vertex    

(defmethod draw-part :around((part tip) dna)
  (set-colour 0.647059 0.164706 (* 0.164706 (auxin (supplies part))))
  (draw-prism (width part) (height part))
  (call-next-method))

(defmethod draw-part((part segment) dna)
  (with-saved-matrix :modelview-matrix
    (gl:mult-matrix (quart-to-matrix (angles part)))
    (gl:translate 0 (height part) 0)
    (draw-part (apex part) dna)
    (gl:rotate 90 0 1 0)
    (let ((angle 0)
	  (angle-step (/ 360 (segment-buds dna))))
      (dolist (bud (buds part))
	(with-saved-matrix :modelview-matrix
	  (gl:rotate angle 0 1 0) 
	  (gl:rotate (bud-sprout-angle dna) 1 0 0)
	  (draw-part bud dna))
	(incf angle angle-step)))))

(defparameter *camera-pos* (vector -26 7 1 1))
(defparameter *camera-angle* (quart-normalise (quarternion 1 0 0 0)))

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
    (case direction
      (:up (incf (y *camera-pos*) *movement-step*))
      (:down (decf (y *camera-pos*) *movement-step*))
      (:left (decf (z *camera-pos*) *movement-step*))
      (:right (incf (z *camera-pos*) *movement-step*))
      (:forward (incf (x *camera-pos*) *movement-step*))
      (:back (decf (x *camera-pos*) *movement-step*))))
  (setf *camera-look-at* (matrix-by-vector-multi 
			  (4-by-4-multi (translation-matrix (vector 1 0 0)) (quart-to-matrix *camera-angle*) )
			  *camera-pos*)))

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
      	    (quart-normalise (quarternion (deg-to-rad (scaled-max-abs (- x *mouse-x*) *h-sensitivity* 5)) 0 1 0))
	    (quart-normalise (quarternion (deg-to-rad (scaled-max-abs (- *mouse-y* y) *v-sensitivity* 5)) 0 0 1))
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
    ((#\q #\Q #\Escape) (glut:destroy-current-window))
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
"h - show this help
1 - simulate wind
2 - draw position
3 - draw the tree
4 - show how much sunlight each leaf gets"
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
      (draw-position *tree* *dna* (vector 0 0 0 0) (vector 0 0 1 0))))
)

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
  
  (setup-matrice *camera-look-at* (vector 0 (y *camera-pos*) 0))
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

