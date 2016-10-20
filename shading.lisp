(in-package #:tree-sim)

(defparameter *shaders-path* "~/programs/lisp/tree-lisp/shaders/")

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

;  (print (gl:check-framebuffer-status :framebuffer))
;  (print (gl:check-framebuffer-status-ext :framebuffer-ext))
;  (format t "error: ~a~%" (gl:get-error))
    (when (not (member
                (gl:check-framebuffer-status :framebuffer)
		            '(:framebuffer-complete :framebuffer-complete-oes :framebuffer-complete-ext)))
      (print "bad shadow framebuffer")
      (error "the shadow framebuffor wasn't initialised correctly."))

    (gl:bind-framebuffer :framebuffer 0)
))

(defun setup-matrice (pos look-at)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45 (/ *width* *height*) 1 40000)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at (x pos) (y pos) (z pos) (x look-at) (y look-at) (z look-at) 0 1 0)
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
