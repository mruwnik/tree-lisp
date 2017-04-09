(in-package #:tree-sim)

(defparameter *leaf* nil)
(defparameter *segment* nil)


(defun safe-node (&keys children &rest args)
  (when (remove nil children)
    (apply 'make-instance 'clinch:node :children (remove nil children) args)))

(defgeneric draw-part(part dna)
 (:documentation "Draws the given part and all its subparts."))

(defmethod draw-part(part dna)
  "default method in case a NIL object is tried to be drawn"
  nil)

(defmethod draw-part((bud bud) dna)
  (when (leaf bud)
    (safe-node
     :children (list *leaf*)
     :scale (clinch:v! (width (leaf bud)) 1 (leaf-len (leaf bud)))
     :rotation (rtg-math.quaternions:from-fixed-angles
              0.0
              (ensure-float (+ 0 (bud-sprout-angle dna)))
              0.0)
   ;  :translation (clinch:v! 0 0 0.5)
     )))

(defun old-draw-bud (bud dna)
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
          (set-colour 0 (* 0.80 (in-sun (leaf bud))) 0)
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
	  (gl:vertex xl 0 -0.1))   ; bottom-left vertex
  (gl:enable :cull-face)))))

(defun set-colour (&rest a))
(defmethod draw-part :around((part tip) dna)
  (set-colour 0.647059 0.164706 (* 0.164706 (auxin (supplies part))))
  (call-next-method))


(defun sprout-matrix (angle dna)
  "Generate a rotation matrix for the given angle."
  (rtg-math.matrix4:*
   (rtg-math.matrix4:rotation-y (ensure-float angle))
   (rtg-math.matrix4:rotation-x (ensure-float (bud-sprout-angle dna)))
   (rtg-math.matrix4:rotation-y (ensure-float (/ PI 2)))
   (rtg-math.matrix4:identity)))


(defmethod draw-part((part segment) dna)
  (safe-node
   :children (list
              ;; draw the actual segment
              (safe-node
               :children (list *segment*)
               :scale (clinch:v! (width part) (height part) (width part) 1))
              ;; get all children - the apex and any side shoots
              (safe-node
               :children
                  (append
                    ;; the apex
                    (list (draw-part (apex part) dna))
                    ;; the side shoots
                    (loop for angle-step = (/ (* 2 PI) (segment-buds dna))
                          for i from 0 to (1- (segment-buds dna))
                          for bud in (buds part) collecting
                          (safe-node
                           :children (list (draw-part bud dna))
                           :scale (clinch:v! 0.5 1 1)
                           :matrix (sprout-matrix (* angle-step i) dna))))
              :translation (clinch:v! 0 (* 2 (height part)) 0)))
   :rotation (angles part)))


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
      ;(glut:solid-sphere 0.2 20 20)
      )
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
