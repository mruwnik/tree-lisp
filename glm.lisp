(in-package #:tree-sim)

(defmacro matrix (a b &rest c)
  `(make-array ,a :initial-element ,b ,@c))

(defmacro armi-ref4 (vec &rest index)
  `(svref ,vec (+ (nth 0 (list ,@index)) (* (nth 1 (list ,@index)) 4))))

(defmacro sv-op (func a b)
  `(let ((result (make-array (length ,a))))
     (loop for x in ,b
	do (cond
	     ((typep x 'vector) 
	      (loop for i from 0 below (length ,a)
		 do (setf (svref result i) (,func (svref ,a i) (svref x i)))))
	     ((typep x 'number)
	      (loop for i from 0 below (length ,a)
		 do (setf (svref result i) (,func (svref ,a i) x))))))
     result))

(defun sv- (a &rest b)
  (sv-op - a b))
(defun sv+ (a &rest b)
  (sv-op + a b))
(defun sv/ (a &rest b)
  (sv-op / a b))
(defun sv* (a &rest b)
  (sv-op * a b))
(defun sv= (a &rest b)
  (let ((state T)
	(test (coerce (sv-op = a b) 'list)))
    (loop for x in test
       do (if (not x)
	      (setq state nil)))
    state))

(defun 4-by-4-multi (&rest matrices)
  (reduce 
     (lambda (a b)
       (let ((result (matrix 16 1)))
	 (dotimes (i 4 result)
	   (dotimes (j 4)
	     (setf (armi-ref4 result i j) 
		   (glm-dot (matrix-column b i) (matrix-row a j)))))))
     matrices))

(defun matrix-by-vector-multi (matrix vector)
  (let ((result (matrix 4 0)))
    (dotimes (i 4 result)
      (setf (svref result i) 
	    (glm-dot (matrix-row matrix i) vector)))))

(defun matrix-row (matrix row)
  (subseq matrix (* row 4) (* (1+ row) 4)))

(defun matrix-column (matrix column)
  (apply 'vector 
	 (loop for i upto 3 collect (svref matrix (+ (* i 4) column)))))

(defun sqr (x) (* x x))
(defun glm-normalize (vec)
  (if (not (sv= vec (vector 0 0 0)))
      (let ((d (sqrt (apply #'+ (map 'list #'sqr vec)))))
	(map 'vector #'(lambda (x) (/ x d)) vec))
      (vector 0 0 0)))

(defun glm-cross (vec1 vec2)
  (vector
   (- (* (svref vec1 1) (svref vec2 2))
      (* (svref vec1 2) (svref vec2 1)))
   (- (* (svref vec1 2) (svref vec2 0)) 
      (* (svref vec1 0) (svref vec2 2)))
   (- (* (svref vec1 0) (svref vec2 1)) 
      (* (svref vec1 1) (svref vec2 0)))))

(defun glm-dot (vec1 vec2)
  (reduce '+ (map 'list #'(lambda (a b)(* a b)) vec1 vec2)))

(defun glm-ortho (left right bottom top znear zfar)
  (let ((result (matrix 16 0)))
    (setf (armi-ref4 result 0 0) (/ 2 (- right left)))
    (setf (armi-ref4 result 1 1) (/ 2 (- top bottom)))
    (setf (armi-ref4 result 2 2) (- (/ 2 (- zFar zNear))))
    (setf (armi-ref4 result 3 0) (- (/ (+ right left) (- right left))))
    (setf (armi-ref4 result 3 1) (- (/ (+ top bottom) (- top bottom))))
    (setf (armi-ref4 result 3 2) (- (/ (+ zFar zNear) (- zFar zNear))))
    (setf (armi-ref4 result 3 3) 1)
    result))

(defun glm-look-at (eye center up)
  (let* ((result (matrix 16 1))
	 (f (glm-normalize (sv- center eye)))
	 (s (glm-normalize (glm-cross f (glm-normalize up))))
	 (u (glm-cross s f)))
    
    (setf (armi-ref4 result 0 0) (svref s 0))
    (setf (armi-ref4 result 1 0) (svref s 1))
    (setf (armi-ref4 result 2 0) (svref s 2))
    
    (setf (armi-ref4 result 0 1) (svref u 0))
    (setf (armi-ref4 result 1 1) (svref u 1))
    (setf (armi-ref4 result 2 1) (svref u 2))
    
    (setf (armi-ref4 result 0 2) (- (svref f 0)))
    (setf (armi-ref4 result 1 2) (- (svref f 1)))
    (setf (armi-ref4 result 2 2) (- (svref f 2)))
    
    (setf (armi-ref4 result 3 0) (- (glm-dot s eye)))
    (setf (armi-ref4 result 3 1) (- (glm-dot u eye)))
   ;(setf (armi-ref4 result 3 1) (neg (vec-apply #'+ (sv* u eye))))
    (setf (armi-ref4 result 3 2) (glm-dot f eye))
    (setf (armi-ref4 result 3 3) 1)
    
    result))
