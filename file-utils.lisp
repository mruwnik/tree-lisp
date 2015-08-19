
(in-package #:tree)

(defparameter *shaders-path* "programs/lisp/tree/shaders/")
;(defparameter *shaders-path* "shaders/")

(defun slurp-file (file-name)
  (let* ((stream (open file-name))
	 (seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(define-condition shader-error (error)
  ((text :initarg :text :reader text)))

(defun load-shaders (vertex-shader-file fragment-shader-file)
  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader)))
    (gl:shader-source vs (slurp-file (concatenate 'string *shaders-path* vertex-shader-file)))
    (gl:compile-shader vs)
    (gl:shader-source fs (slurp-file (concatenate 'string *shaders-path* fragment-shader-file)))
    (gl:compile-shader fs)

    (when (> (length (gl:get-shader-info-log vs)) 0)
      (error 'shader-error :text 
	     (concatenate 'string "vs: " (gl:get-shader-info-log vs))))
    (when (> (length (gl:get-shader-info-log fs)) 0)
      (error 'shader-error :text 
	     (concatenate 'string "fs: " (gl:get-shader-info-log fs))))

    (setf program (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)

    (gl:bind-attrib-location program 0 "fragmentdepth")
    (gl:bind-attrib-location program 0 "vertexPosition_modelspace")
    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program program)
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
;    (gl:use-program program)
    program))

(defmacro matrix (a b &rest c)
  `(make-array ,a :initial-element ,b ,@c))

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

(defmacro armi-ref4 (vec &rest index)
  `(svref ,vec (+ (nth 0 (list ,@index)) (* (nth 1 (list ,@index)) 4))))


(defun sqr (x) (* x x))
(defun glm-normalize (vec)
  (if (not (sv= vec (vector 0 0 0)))
      (let ((d (sqrt (apply #'+ (map 'list #'sqr vec)))))
	(map 'vector #'(lambda (x) (/ x d)) vec))
      (vector 0 0 0)))

(defun glm-cross (vec1 vec2)
  (let ((vec3 (matrix 3 0)))
    (setf (svref vec3 0) 
	  (- (* (svref vec1 1) (svref vec2 2))
	     (* (svref vec1 2) (svref vec2 1))))
    (setf (svref vec3 1) 
	  (- (* (svref vec1 2) (svref vec2 0)) 
	     (* (svref vec1 0) (svref vec2 2))))
    (setf (svref vec3 2) 
	  (- (* (svref vec1 0) (svref vec2 1)) 
	     (* (svref vec1 1) (svref vec2 0))))
    vec3))

(defun glm-dot (vec1 vec2)
  (let ((A 0))
    (setf A (+ A (* (svref vec1 0) (svref vec2 0))))
    (setf A (+ A (* (svref vec1 1) (svref vec2 1))))
    (setf A (+ A (* (svref vec1 2) (svref vec2 2))))
    A))

(defun glm-ortho (left right bottom top znear zfar)
  (let ((result (matrix 16 1)))
    (setf (armi-ref4 result 0 0) (/ 2 (- right left)))
    (setf (armi-ref4 result 1 1) (/ 2 (- top bottom)))
    (setf (armi-ref4 result 2 2) (- (/ 2 (- zFar zNear))))
    (setf (armi-ref4 result 3 0) (- (/ (+ right left) (- right left))))
    (setf (armi-ref4 result 3 1) (- (/ (+ top bottom) (- top bottom))))
    (setf (armi-ref4 result 3 2) (- (/ (+ zFar zNear) (- zFar zNear))))
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
