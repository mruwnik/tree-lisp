(in-package #:tree-sim)

(defparameter *shaders-path* "programs/lisp/tree-lisp/shaders/")
;(defparameter *shaders-path* "shaders/")

(defun slurp-file (file-name)
  (let* ((stream (open file-name))
	 (seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(define-condition shader-error (error)
  ((text :initarg :text :reader text)))

(defmacro deg-to-rad (degrees)
  `(* PI (/ ,degrees 180)))

(defmacro rad-to-deg (degrees)
  `(* 180 (/ ,degrees PI)))

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

    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program program)

    (when (> (length (gl:get-program-info-log program)) 0)
      (error 'shader-error :text
	     (concatenate 'string "fs: " (gl:get-program-info-log program))))

    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
;    (gl:use-program program)
    program))

(defun generate-shadow-fbuffer()
  (let ((width (* *width* *shadow-map-ratio*))
	(height (* *height* *shadow-map-ratio*)))
    (setf *shadow-texture* (first (gl:gen-textures 1)))
    (gl:bind-texture :texture-2d *shadow-texture*)
    
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
;  (gl:tex-parameter :texture-2d :texture-compare-func :lequal)
;  (gl:tex-parameter :texture-2d :texture-compare-mode :compare-r-to-texture)

    (gl:tex-image-2d :texture-2d 0 :depth-component width height 0 :depth-component :unsigned-byte (cffi:null-pointer))
    (gl:bind-texture :texture-2d 0)

    (setf *framebuffer* (first (gl:gen-framebuffers 1)))
    (gl:bind-framebuffer :framebuffer *framebuffer*)
  
    (gl:draw-buffer :none)
    (gl:read-buffer :none)


    (gl:framebuffer-texture-2d :framebuffer :depth-attachment :texture-2d *shadow-texture* 0)  

 ; (print (gl:check-framebuffer-status :framebuffer))
 ; (print (gl:check-framebuffer-status-ext :framebuffer-ext))
 ; (format t "error: ~a~%" (gl:get-error))
    (when (not (member
		(gl:check-framebuffer-status :framebuffer)
		'(:framebuffer-complete :framebuffer-complete-oes :framebuffer-complete-ext)))
      (error "the shadow framebuffor wasn't initialised correctly."))
    
    (gl:bind-framebuffer :framebuffer 0)
))

(defun setup-matrice (pos-x pos-y pos-z look-x look-y look-z)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45 (/ *width* *height*) 10 40000)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at pos-x pos-y pos-z look-x look-y look-z 0 1 0)
)

(defun set-texture-matrix ()
  (let ((modelview (gl:get-double :modelview-matrix))
	(projection (gl:get-double :projection-matrix)))
    (gl:matrix-mode :texture)
    (gl:active-texture :texture7)

    (gl:load-identity)
    (gl:load-matrix (make-array 16 :initial-contents 
				'(0.5 0.0 0.0 0.0 
				  0.0 0.5 0.0 0.0
				  0.0 0.0 0.5 0.0
				  0.5 0.5 0.5 1.0)))
    
    (gl:mult-matrix projection)
    (gl:mult-matrix modelview)
    
    (gl:matrix-mode :modelview)
))


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
