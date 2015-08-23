(in-package #:tree)

(defparameter *sun-pos* '(0 5000 0 1))

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
    pos))

(defgeneric get-position (part dna base-pos rotation)
  (:documentation "return the absolute position of the given part."))
(defmethod get-position (part dna base-pos rotation)
  "the default, catch all method.")
(defmethod get-position ((part segment) dna base-pos rotation)
  (let ((tip (absolute-position base-pos (vector 0 (height part) 0 0) rotation)))
    (get-position (apex part) dna tip rotation)
;    (set-colour 153/255 84/255 190/255)
;    (gl:push-matrix)
;    (gl:translate (svref tip 0) (svref tip 1) (svref tip 2))
;    (glut:solid-sphere 0.2 20 20)
;    (gl:pop-matrix)
    (let ((angle 0)
	  (angle-step (/ 360 (segment-buds dna))))
      (dolist (bud (buds part))
	(get-position bud dna tip
		      (reduce 'multiply-quarts
			      (list
			       (quarternion 0 1 0 0)
		       (quarternion 90 0 0 1)
		       (quarternion angle 0 1 0)

		       )))
      (incf angle angle-step)))
    tip))
(defmethod get-position ((part bud) dna base-pos rotation)
  (when (leaf part)
;        (set-colour 153/255 84/255 190/255)
;    (gl:push-matrix)
;    (gl:translate (svref base-pos 0) (svref base-pos 1) (svref base-pos 2))
    (let* ((xr (/ (width (leaf part)) 2))
	   (xl (- 0 xr))
	   (l (- (leaf-len (leaf part)))))
;    (gl:with-primitives :quads      ; start drawing quadrilaterals))
      (dolist (coords
      (loop for coords in `((-0.1 ,xl ,l) (-0.1 ,xr ,l) (-0.1 ,xr 0) (-0.1 ,xl 0)) collecting
	   (absolute-position base-pos (apply 'vector coords) rotation)))
;      (gl:vertex (svref coords 0) (svref coords 1) (svref coords 2)))
      ))
;    (gl:pop-matrix)
))


(get-position *tree* *dna* (vector 0 0 0 0) (quarternion PI 0 0 0))

(absolute-position (vector 0 0 0 1) (vector 1 0 0 0) (quarternion PI 0 0 1))


