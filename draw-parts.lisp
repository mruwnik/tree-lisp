(in-package #:tree)


;; Simulate push and pop matrix. This is needed because OpenGL has
;; a limit on how many matrixes may be pushed at once. This limit
;; is quite small (like 64), which is a problem given the amount 
;; of branching needed to draw a tree. This macro saves the current
;; matrix into a variable and then replaces it after the body is
;; executed.
(defmacro with-saved-matrix (matrix &body body)
  (let ((matrix-name (gensym)))
    `(let ((,matrix-name (gl:get-double ,matrix)))
       ,@body
       (gl:load-identity)
       (gl:mult-matrix ,matrix-name))))


(defun set-colour (r g b &optional (alpha 1))
  (gl:material :front :specular `(,r ,g ,b ,alpha))
  (gl:material :front :ambient `(,r ,g ,b 0.1))
  (gl:material :front :shininess 1)
  (gl:color r g b))


(defgeneric draw-part(part dna)
 (:documentation "Draws the given part and all its subparts."))

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



(defgeneric draw-position (part dna base-pos rotation)
  (:documentation "Draw the absolute position of the given part.
This draws the parts based on their absolute position, without using
OpenGL's rotations and translations. This means calculating
exactly where each point should be, translating to that position,
drawing that point and then reseting the matrix. The calculations
are used to calculate how much sun light each leaf recieves. These
display functions are here for debugging perposes.
"))
(defmethod draw-position (part dna base-pos rotation)
  "the default, catch all method.")
(defmethod draw-position ((part segment) dna base-pos rotation)
  (let ((tip (absolute-position base-pos (vector 0 (height part) 0 0) rotation)))
    (draw-position (apex part) dna tip 
		   (multiply-quarts rotation (angles part)))
    (gl:with-pushed-matrix
      (set-colour 153/255 84/255 90/255)

      (gl:with-primitives :triangles
	(vector-vertex base-pos (- (width part)))
	(vector-vertex base-pos (width part))
	(vector-vertex tip (- (width part)))

	(vector-vertex base-pos (width part))
	(vector-vertex tip (width part))
	(vector-vertex tip (- (width part)))))
    (gl:with-pushed-matrix
      (gl:translate (svref tip 0) (svref tip 1) (svref tip 2))
      (set-colour 153/255 84/255 190/255)
      (glut:solid-sphere 0.2 20 20))
    (let ((angle 0)
	  (angle-step (/ (* 2 PI) (segment-buds dna))))
      (dolist (bud (buds part))
	(draw-position 
	 bud dna tip
	 (reduce 'multiply-quarts
		 (list
		  rotation
		  (quart-normalise 
		   (quarternion (+ angle (/ PI 2)) 0 1 0))
		  (quart-normalise 
		   (quarternion 
		    (deg-to-rad (bud-sprout-angle dna)) 1 0 0))
		  (angles bud)
		  )))
	(incf angle angle-step)))
    tip))
(defmethod draw-position ((part bud) dna base-pos rotation)
  (when (leaf part)
        (set-colour 153/255 84/255 190/255)
    (gl:with-pushed-matrix
      (let* ((xr (/ (width (leaf part)) 2))
	     (xl (- 0 xr))
	     (l (- (leaf-len (leaf part)))))
	(gl:with-primitives :quads      ; start drawing quadrilaterals))
	  (dolist (coords
		    (loop for coords in `((,xl 0 ,l) (,xr 0 ,l) (,xr 0 -0.1) (,xl 0 -0.1)) collecting
			 (absolute-position base-pos (apply 'vector coords) rotation)))
	    (vector-vertex coords))
	  )))))
