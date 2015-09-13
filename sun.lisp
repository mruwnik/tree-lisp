(in-package #:tree)

(defparameter *sun-pos* '(0 5000 0 1))

(defmacro x (point)
  `(svref ,point 0))
(defmacro y (point)
  `(svref ,point 1))
(defmacro z (point)
  `(svref ,point 2))

(defun quarternion (angle x y z)
  (let* ((half-angle (/ angle 2))
	 (sin-angle (sin half-angle)))
    (vector (cos half-angle) 
	    (* x sin-angle) (* y sin-angle) (* z sin-angle))))

(defun quart-magnitude (quart)
  (sqrt (reduce '+ (map 'list #'(lambda (x) (expt x 2)) quart))))

(defun quart-normalise (quart)
 (let ((magnitude (quart-magnitude quart)))
   (map 'vector #'(lambda (x) (/ x magnitude)) quart)))

(defun multiply-quarts (q1 q2)
  (let ((w1 (svref q1 0))
	(x1 (svref q1 1))
	(y1 (svref q1 2))
	(z1 (svref q1 3))
	(w2 (svref q2 0))
	(x2 (svref q2 1))
	(y2 (svref q2 2))
	(z2 (svref q2 3)))
    (vector (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2))
	    (+ (* w1 x2) (* x1 w2) (* y1 z2) (- (* z1 y2)))
	    (+ (* w1 y2) (- (* x1 z2)) (* y1 w2) (* z1 x2))
	    (+ (* w1 z2) (* x1 y2) (- (* y1 x2)) (* z1 w2)))))
	       
(defun rotate-by-quart (current quart)
  (multiply-quarts quart current))

(defun quart-to-matrix (quart)
  (let ((w (svref quart 0))
	(x (svref quart 1))
	(y (svref quart 2))
	(z (svref quart 3)))
    (vector
     (- 1 (* 2 (expt y 2)) (* 2 (expt z 2)))
     (- (* 2 x y) (* 2 w z))
     (+ (* 2 x z) (* 2 w y))
     0

     (+ (* 2 x y) (* 2 w z))
     (- 1 (* 2 (expt x 2)) (* 2 (expt z 2)))
     (- (* 2 y z) (* 2 w x))
     0

     (- (* 2 x z) (* 2 w y))
     (+ (* 2 y z) (* 2 w x))
     (- 1 (* 2 (expt x 2)) (* 2 (expt y 2)))
     0
     
     0 0 0 1)))

(defmacro identity-matrix ()
  (vector
   1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1))

(defun translation-matrix (translate-by)
  (vector 
   1 0 0 (svref translate-by 0)
   0 1 0 (svref translate-by 1)
   0 0 1 (svref translate-by 2)
   0 0 0 1))

(defun compare-x (p1 p2) 
  (> (svref p1 0) (svref p2 0)))

(defun compare-z (p1 p2) 
  (> (svref p1 2) (svref p2 2)))

(defun sort-triangle (a b c)
  "Sort the given 3 points to get a sort of canonical representation of the triangle.
the points are returned in the following order:
 - max(x), with the min(z) in case of several points having the max x
 - min(z) from the other 2 points
 - the point that is left over.

this representation means that one can always go along the edges, knowing that the left side of the traversed edge is outside the triangle, while the right side is inside it."
  (let ((x-sorted (sort (reverse (sort (list a b c) #'compare-z)) #'compare-x)))
    (cons (first x-sorted) (reverse (sort (rest x-sorted) #'compare-z)))))

(defun linear-func(p1 p2)
  "get a linear function calculated on the basis of the given 2 points. it will be on the y plane (only the x and z coords will be used)."
  (if (eq (y p1) (y p2))
      #'(lambda (x) (y p2))
      (let* ((a (/ (- (z p1) (z p2)) (- (x p1) (x p2))))
	     (b (- (z p1) (* a (x p1)))))
	#'(lambda (x) (+ (* a x) b)))))

(defun rev-linear-func(p1 p2)
  "get a reverted linear function (get x on the basis of y) calculated on the basis of the given 2 points. it will be on the y plane (only the x and z coords will be used)."
  (if (eq (x p1) (x p2)) #'(lambda (y) (x p1))
      (let* ((a (/ (- (z p1) (z p2)) (- (x p1) (x p2))))
	     (b (- (z p1) (* a (x p1)))))
	#'(lambda (y) (/ (- y b) a)))))

(defun rasterise-triangle(raster a b c &optional (marker 1))
  "rasterize the given triangle by appending to each point in the raster matrix that it covers (in the y plane) with the given marker."
  (let* ((ab (rev-linear-func a b))
	 (bc (unless (eq (z b) (z c))
	       (rev-linear-func b c)))
	 (ca (unless (eq (z a) (z c)) 
	       (rev-linear-func c a)))
	 (left-fun ab)
	 (right-fun ca))
  (loop for z from (z a) downto (min (z b) (z c)) do
       (progn
	 (loop for x from (round (funcall left-fun z))
	    to (round (funcall right-fun z))
	    do (setf (aref raster x z)
		     (nconc (aref raster x z) (list marker))))
	 (when (eq z (z b))
	   (setf left-fun bc))
	 (when (eq z (z c))
	   (setf right-fun bc))))
  raster))

(defun absolute-position (position translate-by rotation)
  "Turn by the given rotation and then move the given amount from the given position. The new position is returned.
vector position: the current postition to which the transformations are to be applied (4 elements).
vector translate-by: a 4 elem vector stating by how much to translate
quarternion rotation: a quarternion containing info how to rotate
"
  (let ((pos 
	 (if (not translate-by)
	     position
	     (if rotation
		 (let ((translate-matrix (translation-matrix translate-by))
		       (rotation-matrix (when rotation (quart-to-matrix rotation))))
		   (sv+
		    (matrix-column (4-by-4-multi rotation-matrix translate-matrix) 3)
		    position))
		 position))))
    (setf (svref pos 3) 1)
    pos)
)

(defun vector-vertex (vector &optional (x 0) (y 0) (z 0))
  (gl:vertex 
   (+ x (svref vector 0)) 
   (+ y (svref vector 1)) 
   (+ z (svref vector 2))))

(defgeneric draw-position (part dna base-pos rotation)
  (:documentation "draw the absolute position of the given part."))
(defmethod draw-position (part dna base-pos rotation)
  "the default, catch all method.")
(defmethod draw-position ((part segment) dna base-pos rotation)
  (let ((tip (absolute-position base-pos (vector 0 (height part) 0 0) rotation)))
    (draw-position (apex part) dna tip rotation)
    (gl:push-matrix)
    (set-colour 153/255 84/255 90/255)
    (gl:rotate (rad-to-deg (svref rotation 0)) (svref rotation 1) (svref rotation 2) (svref rotation 3))
    (gl:with-primitives :triangles
      (vector-vertex base-pos (- (width part)))
      (vector-vertex base-pos (width part))
      (vector-vertex tip (- (width part)))

      (vector-vertex base-pos (width part))
      (vector-vertex tip (width part))
      (vector-vertex tip (- (width part)))
      )
    (gl:pop-matrix)
    (gl:push-matrix)
    (gl:translate (svref tip 0) (svref tip 1) (svref tip 2))
    (set-colour 153/255 84/255 190/255)
    (glut:solid-sphere 0.2 20 20)
    (gl:pop-matrix)
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
		    (deg-to-rad (bud-sprout-angle dna)) 1 0 0)))))
	(incf angle angle-step)))
    tip))
(defmethod draw-position ((part bud) dna base-pos rotation)
  (when (leaf part)
        (set-colour 153/255 84/255 190/255)
    (gl:push-matrix)
    (let* ((xr (/ (width (leaf part)) 2))
	   (xl (- 0 xr))
	   (l (- (leaf-len (leaf part)))))
    (gl:with-primitives :quads      ; start drawing quadrilaterals))
      (dolist (coords
      (loop for coords in `((,xl 0 ,l) (,xr 0 ,l) (,xr 0 -0.1) (,xl 0 -0.1)) collecting
	   (absolute-position base-pos (apply 'vector coords) rotation)))
      (vector-vertex coords))
      ))
    (gl:pop-matrix)
))


(defparameter *shadow-granularity* 1)
(defgeneric map-shadow (shadow-map part dna base-pos rotation)
  (:documentation "Add the given part to the shadow map."))
(defmethod map-shadow (shadow-map part dna base-pos rotation)
  "the default, catch all method.")
(defmethod map-shadow (shadow-map (part segment) dna base-pos rotation)
  (let ((tip (absolute-position base-pos (vector 0 (height part) 0 0) rotation)))
    
    (map-shadow shadow-map (apex part) dna tip rotation)
    (let ((angle 0)
	  (angle-step (/ (* 2 PI) (segment-buds dna))))
      (dolist (bud (buds part))
	(map-shadow 
	 shadow-map bud dna tip
	 (reduce 'multiply-quarts
		 (list
		  rotation
		  (quart-normalise (quarternion angle 0 1 0))
		  (quart-normalise 
		   (quarternion 
		    (deg-to-rad (bud-sprout-angle dna)) 1 0 0)))))
	(incf angle angle-step)))
    tip))
(defmethod map-shadow (shadow-map (part bud) dna base-pos rotation)
  (when (leaf part)
    (let* ((xr (/ (width (leaf part)) 2))
	   (xl (- 0 xr))
	   (l (- (leaf-len (leaf part)))))
      (loop for coords in `((,xl 0 ,l) (,xr 0 ,l) (,xr 0 -0.1) (,xl 0 -0.1)) collecting
	   (absolute-position base-pos (apply 'vector coords) rotation)))
      ))

;(let ((shadow-map (make-hash-table :test #'equal)))
;  (map-shadow shadow-map *tree* *dna* (vector 0 0 0 0) (vector 1 0 0 0))
;  shadow-map)
