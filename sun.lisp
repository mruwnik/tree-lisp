(in-package #:tree)

(defparameter *sun-pos* '(0 5000 0 1))

(defun quartilion (angle x y z)
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
     (- 1 (* 2 (expt y 2) (* 2 (expt z 2))))
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

(defun apply-transformations (position translate-by rotation)
  "Translate and rotate the given position. The new position is returned.
vector position: the current postition to which the transformations are to be applied (4 elements).
vector translate-by: a 4 elem vector stating by how much to translate
quartinion rotation: a quartinion containing info how to rotate
"
  (let ((translate-matrix (when translate-by 
			    (vector 
			     1 0 0 (svref translate-by 0)
			     0 1 0 (svref translate-by 1)
			     0 0 1 (svref translate-by 2)
			     0 0 0 1)))
	(rotation-matrix (when rotation (quart-to-matrix rotation))))
    (if (or translate-matrix rotation-matrix)
      (matrix-by-vector-multi 
       (if (and translate-matrix rotation-matrix)
	   (4-by-4-multi translate-matrix rotation-matrix)
	   (if translate-matrix translate-matrix rotation-matrix))
	   position)
      position)))

(apply-transformations (vector 1 1 1 1) (vector 0 2 0) NIL)
