(in-package #:tree-sim)

(defparameter *sun-pos* '(0 5000 0 1))

(defmacro x (point)
  `(svref ,point 0))
(defmacro y (point)
  `(svref ,point 1))
(defmacro z (point)
  `(svref ,point 2))

(defun almost-equal (a &rest values)
  (let ((error-margin 0.00001))
    (every #'(lambda (val) 
	       (and (< a (+ val error-margin))
		    (> a (- val error-margin)))) values)))

(defun round-to (number &optional (precision 1) (what #'round))
  (if (= precision 0)
      (funcall what number)
      (let ((div (expt 10 precision)))
	(/ (funcall what (* number div)) div))))

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


(defun x-sorted (p1 p2) (< (x p1) (x p2)))
(defun z-sorted (p1 p2) (> (z p1) (z p2)))

(defun sort-triangle (a b c)
  "Sort the given 3 points to get a sort of canonical representation of the triangle.
the points are returned in the following order:
 - max(x), with the min(z) in case of several points having the max x
 - min(z) from the other 2 points
 - the point that is left over.

this representation means that one can always go along the edges, knowing that the left side of the traversed edge is outside the triangle, while the right side is inside it."
  (let* ((x-sorted (stable-sort (sort (list a b c) #'x-sorted) #'z-sorted))
	 (p1 (first x-sorted)))
    (nconc 
     (list (first x-sorted))
     (if (almost-equal (z p1) (z (second x-sorted)))
	 (reverse (rest x-sorted))
	 (stable-sort (reverse (sort (rest x-sorted) #'z-sorted)) #'x-sorted)))))

(defun linear-func(p1 p2)
  "get a linear function calculated on the basis of the given 2 points. it will be on the y plane (only the x and z coords will be used)."
  (if (almost-equal (y p1) (y p2))
      #'(lambda (x) (y p2))
      (let* ((a (/ (- (z p1) (z p2)) (- (x p1) (x p2))))
	     (b (- (z p1) (* a (x p1)))))
	#'(lambda (x) (+ (* a x) b)))))

(defun rev-linear-func(p1 p2)
  "get a reverted linear function (get x on the basis of y) calculated on the basis of the given 2 points. it will be on the y plane (only the x and z coords will be used)."
  (if (almost-equal (x p1) (x p2)) #'(lambda (y) (x p1))
      (let* ((a (/ (- (z p1) (z p2)) (- (x p1) (x p2))))
	     (b (- (z p1) (* a (x p1)))))
	#'(lambda (y) (/ (- y b) a)))))


(defun add-marker (raster key marker)
  (setf (gethash key raster)
	(nconc (gethash key raster) (list (cons 2 marker)))))

(defun rasterise-normed-triangle(raster a b c &optional (marker 1) (precision 1))
  "rasterize the given triangle by appending to each point in the raster matrix that it covers (in the y plane) with the given marker."
  (let* ((ab (rev-linear-func a b))
	 (bc (unless (almost-equal (z b) (z c))
	       (rev-linear-func b c)))
	 (ca (unless (almost-equal (z a) (z c)) 
	       (rev-linear-func c a)))
	 (left-fun ab)
	 (right-fun (if ca ca bc))
	 (step (/ 1 (expt 10 precision)))

	 ; this needs a decent function to calculate the height
	 ; for the given point, but I can't be bothered to think
	 ; of a decent way, so it just uses the average for the whole
	 ; triangle. This should probably work well enough anyway.
	 (avg-height (/ (+ (y a) (y b) (y c)) 3))
	 (height (lambda (x z) avg-height)))
  (loop for z from (z a) downto (min (z b) (z c)) by step
     with rounded-z do
       (progn
	 (setf rounded-z (round-to z precision))
	 (loop for x from (round-to (funcall left-fun z) precision)
	    to (round-to (funcall right-fun z) precision) by step
	    do (add-marker raster (list x rounded-z) marker))
	 (when (<= z (z b))
	   (setf left-fun bc))
	 (when (<= z (z c))
	   (setf right-fun bc))))
  raster))

(defparameter *bounds* '(0 0 0 0))

(defun rasterise-triangle (raster a b c &optional (marker 1) (precision 1))
  (cond
    ((almost-equal (x a) (x b) (x c)) 
     (loop for i from (min (z a) (z b) (z c)) to (max (z a) (z b) (z c))
	by (/ 1 (expt 10 precision)) do
	  (add-marker raster (list (x a) (round-to i precision)) marker)))
    ((almost-equal (z a) (z b) (z c))
     (loop for i from (min (x a) (x b) (x c)) to (max (x a) (x b) (x c))
	by (/ 1 (expt 10 precision)) do
	  (add-marker raster (list (round-to i precision) (z a)) marker)))
    (T (apply 'rasterise-normed-triangle raster 
	      (append (sort-triangle a b c) (list marker precision))))))


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


(defparameter *shadow-granularity* 0.5)
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
	   (l (- (leaf-len (leaf part))))
	   (points (loop for coords in 
			`((,xl 0 ,l) (,xr 0 ,l)
			  (,xr 0 -0.1) (,xl 0 -0.1))
		      collecting
			(absolute-position
			 base-pos (apply 'vector coords) rotation))))
      (rasterise-triangle 
       shadow-map (first points) (second points) (fourth points)
       (leaf part) *shadow-granularity*)
      (rasterise-triangle 
       shadow-map (second points) (third points) (fourth points)
       (leaf part) *shadow-granularity*))))

(defun shine (shadow-map)
  (let ((shadow-counter (make-hash-table :test #'equal)))
    (loop for key being the hash-keys of shadow-map do 
	 (loop for item in (sort (gethash key shadow-map) 
				 #'(lambda (a b) (> (first a) (first b))))
	    with pos = 1 do
	      (progn
		(setf current-count 
		      (cond ((gethash (cdr item) shadow-counter)) ('(0 0))))
		(setf (gethash (cdr item) shadow-counter)
		      (list (+ (first current-count) (/ 1 pos))
			    (1+ (second current-count))))
		(incf pos))))
;(print (loop for key being the hash-keys of shadow-map collect key))
    (loop for part being the hash-keys of shadow-counter do
	 (setf (in-sun part) (apply '/ (gethash part shadow-counter))))
    shadow-counter))

(defparameter *shadow-map* (make-hash-table :test #'equal))


(defgeneric in-sun (part)
  (:documentation "return how much the given part is in the sun, from 0 to 1"))
(defmethod in-sun (part) 1)
