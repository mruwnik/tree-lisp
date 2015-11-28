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
	(gl:with-primitives :quads      ; start drawing quadrilaterals
	  (gl:vertex xl 0 l)    ; top-left vertex
	  (gl:vertex xr 0 l)    ; top-right vertex
	  (gl:vertex xr 0 -0.1)    ; bottom-right vertex
	  (gl:vertex xl 0 -0.1))))))   ; bottom-left vertex    

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

(defparameter *draw-position* NIL)
(defparameter *draw-wind* NIL)
(defparameter *draw-tree* t)
(defparameter *draw-leaf-occulence* NIL)
(defparameter *show-help* NIL)
(defparameter *show-debug-info* NIL)

(defclass my-window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p)
   (width :initarg width :initform 400 :accessor width)
   (height :initarg height :initform 300 :accessor height)
   (shaders :initarg shaders :initform t :accessor shaders))
  (:default-initargs :width 400 :height 300
                     :title "tree"
                     :x 100 :y 100
                     :mode '(:double :rgb :depth)
                     :fullscreen nil))

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
    ((#\e) (decf *y* *movement-step*) (reposition))
    ((#\r) (incf *y* *movement-step*) (reposition))
    ((#\q #\Q #\Escape) (glut:close win))
    ((#\f #\F)                      ; when we get an 'f'
                                    ; save whether we're in fullscreen
         (let ((full (fullscreen-p win)))
           (glut:close win)         ; close the current window
           (glut:display-window     ; open a new window with fullscreen toggled
               (make-instance 'my-window
                              :fullscreen (not full)))))
    ((#\h #\H) (setf *show-help* (not *show-help*)))
    ((#\1) (setf *draw-wind* (not *draw-wind*)))
    ((#\2) (setf *draw-position* (not *draw-position*)))
    ((#\3) (setf *draw-tree* (not *draw-tree*)))
    ((#\4) (setf *draw-leaf-occulence* (not *draw-leaf-occulence*)))
    ((#\0) (setf *show-debug-info* (not *show-debug-info*)))
    ))

(defmethod glut:keyboard-up ((win my-window) key xx yy)
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
  (defparameter *shader-program*
    (load-shaders "shadowmapping.vs" "shadowmapping.fs"))  
  (setup-depth-shaders win))

(defun setup-depth-shaders (win)
  (defparameter *depth-program*
    (load-shaders "depthDTT.vs" "depthDTT.fs"))
  (gl:bind-attrib-location *depth-program* 0 "fragmentdepth")
  (gl:bind-attrib-location *depth-program* 0 "vertexPosition_modelspace")

  (defparameter *shadow-framebuffer* (first (gl:gen-framebuffers 1)))
  (gl:bind-framebuffer :framebuffer *shadow-framebuffer*)

  (defparameter *shadow-texture* (first (gl:gen-textures 1)))
  (gl:bind-texture :texture-2d *shadow-texture*)
  (gl:tex-image-2d :texture-2d 0 :depth-component16 1024 1024 0 :depth-component :float (cffi:null-pointer))
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-compare-func :lequal)
  (gl:tex-parameter :texture-2d :texture-compare-mode :compare-r-to-texture)
  (gl:framebuffer-texture-2d :framebuffer :depth-attachment :texture-2d *shadow-framebuffer* 0)

  (gl:draw-buffer :none)
  (when (not (member
	      (gl:check-framebuffer-status :framebuffer)
	      '(:framebuffer-complete :framebuffer-complete-oes :framebuffer-complete-ext)))
    (error "the shadow framebuffor wasn't initialised correctly.")))

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

(defun shadow-pass ()
  (gl:push-matrix)
  (gl:bind-framebuffer :framebuffer *shadow-framebuffer*)
  (gl:viewport 0 0 1024 1024)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:clear :depth-buffer-bit :color-buffer-bit)
  (gl:use-program *depth-program*)
  (let* (
	 (projection-matrix (glm-ortho -10 10 -10 10 -10 20))
	 (view-matrix (glm-look-at (vector 0.5 2 2) (vector 0 0 0) (vector 0 1 0)))
	 (model-matrix (matrix 16 1))
	 (mvp (4-by-4-multi projection-matrix view-matrix model-matrix))
	 )
    (gl:uniform-matrix 
     (gl:get-uniform-location *depth-program* "depthMVP")
     4 (vector mvp) NIL)
    (draw-part *tree* *dna*)
    )
  (gl:use-program 0)
  (gl:pop-matrix))

(defun display ()
  (glut:schedule-timer 100 #'display)

  ; calculate the tree's shadow
  ;(shadow-pass)
  
  (gl:bind-framebuffer :framebuffer 0)
  (gl:clear-color 0.529 0.808 0.922 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (apply 'render-sun *sun-pos*)

  (gl:push-matrix)
  ; draw grass
  (gl:color 0 0.5 0)
  (gl:material :front :ambient '(0.2 0.41 0 1))
  (gl:with-primitives :quads      ; start drawing quadrilaterals
    (gl:normal 0 -1 0)
    (gl:vertex -10000.0 0  10000.0)    ; top-left vertex
    (gl:vertex  10000.0 0  10000.0)    ; top-right vertex
    (gl:vertex  10000.0 0 -10000.0)    ; bottom-right vertex
    (gl:vertex -10000.0 0 -10000.0))   ; bottom-left vertex    

  (gl:normal 0 0 0)

  (when *draw-tree*
    (with-report-usage
      (set-colour 0.647059 0.164706 0.164706)
      (draw-part *tree* *dna*)))

  (when *show-help*
    (show-help))
  (gl:pop-matrix)
  (glut:swap-buffers))

(print "starting up a window")
(defparameter *window* (make-instance 'my-window))
(progn 
  (glut:schedule-timer 100 #'display)
  (glut:display-window *window*))
(print "done")

