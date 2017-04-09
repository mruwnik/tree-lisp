(in-package #:tree-sim)

(defun cube (ambient-light sun-intensity sun-direction)
	(make-instance 'clinch:entity
		       :shader-program  (clinch::get-generic-single-diffuse-light-per-vertex-color-shader)                                   ;; Add the shader
		       :indexes (make-instance 'clinch:index-buffer :data '(0  1  2       ;; Add the index buffer
									    2  1  3
									    4  5  6
									    6  5  7
									    8  9 10 
									    10  9 11
									    12 13 14 
									    14 13 15
									    16 17 18 
									    18 17 19
									    20 21 22 
									    22 21 23))   
		       :uniforms   `(("P" . :projection)                                 ;; Set the projection matrix (:projection is a special value, which uses 
				     ("M" . :model);; the projection matrix passed to the entity. "P" is just the name I picked.
				     ("N" . :normal)                                       ;; Current normal matrix
				     ("ambientLight"  . ,(lambda () ambient-light))      ;; Ambient light
				     ("lightIntensity" . ,(lambda () sun-intensity))    ;; Brightness
				     ("lightDirection" . ,(lambda () sun-direction)))   ;; Direction


		       :attributes   `(("v" . ,(make-instance 'clinch:buffer              ;; Set the vertex buffer, "v" is just the name I picked.
							      :Stride 3                   ;; Stride 3, there are 3 float (x, y, z) for each vertex.
							      :data '(
                            -0.5 -0.5  0.5
						0.5 -0.5  0.5
						-0.5  0.5  0.5
						0.5  0.5  0.5
						-0.5 -0.5 -0.5
						0.5 -0.5 -0.5
						-0.5 -0.5  0.5
						0.5 -0.5  0.5
						-0.5  0.5 -0.5
						0.5  0.5 -0.5
						-0.5 -0.5 -0.5
						0.5 -0.5 -0.5
						-0.5  0.5  0.5
						0.5  0.5  0.5
						-0.5  0.5 -0.5
						0.5  0.5 -0.5
						0.5 -0.5  0.5
						0.5 -0.5 -0.5
						0.5  0.5  0.5
						0.5  0.5 -0.5
						-0.5 -0.5 -0.5 
						-0.5 -0.5  0.5 
						-0.5  0.5 -0.5 
						-0.5  0.5  0.5)))
				       ("n" . ,(make-instance 'clinch:buffer              ;; Set the vertex buffer, "v" is just the name I picked.
							      :Stride 3                   ;; Stride 3, there are 3 float (x, y, z) for each vertex.
							      :data '(0.0 0.0 1.0
								      0.0 0.0 1.0
								      0.0 0.0 1.0
								      0.0 0.0 1.0
								      0.0 -1.0 0.0
								      0.0 -1.0 0.0
								      0.0 -1.0 0.0
								      0.0 -1.0 0.0
								      0.0 0.0 -1.0
								      0.0 0.0 -1.0
								      0.0 0.0 -1.0
								      0.0 0.0 -1.0
								      0.0 1.0 0.0
								      0.0 1.0 0.0
								      0.0 1.0 0.0
								      0.0 1.0 0.0
								      1.0 0.0 0.0
								      1.0 0.0 0.0
								      1.0 0.0 0.0
								      1.0 0.0 0.0
								      -1.0 0.0 0.0
								      -1.0 0.0 0.0
								      -1.0 0.0 0.0
								      -1.0 0.0 0.0)))
				       ("vertexColor" . ,(make-instance 'clinch:buffer         ;; Create a buffer to hold the color data. "colors" is just the name I picked.
								   :stride 3              ;; There are three floats (red, green, blue) for each vertex.
								   :data '(1.0 0.0 0.0
									     1.0 0.0 0.0
									     1.0 0.0 0.0
									     1.0 0.0 0.0
									     0.0 1.0 0.0
									     0.0 1.0 0.0
									     0.0 1.0 0.0
									     0.0 1.0 0.0
									     1.0 1.0 0.0
									     1.0 1.0 0.0
									     1.0 1.0 0.0
									     1.0 1.0 0.0
								 	     0.0 1.0 1.0
									     0.0 1.0 1.0
									     0.0 1.0 1.0
									     0.0 1.0 1.0
									     0.0 0.0 1.0
									     0.0 0.0 1.0
									     0.0 0.0 1.0
									     0.0 0.0 1.0
									     1.0 0.0 1.0
									     1.0 0.0 1.0
									     1.0 0.0 1.0
									     1.0 0.0 1.0))))))

(defun leaf-model (ambient-light sun-intensity sun-direction)
  	(make-instance 'clinch:entity
		       :shader-program  (clinch::get-generic-single-diffuse-light-per-vertex-color-shader)                                   ;; Add the shader
		       :indexes (make-instance 'clinch:index-buffer :data '(0  1  2       ;; Add the index buffer
                                                                2  1  3
                                                                4  5  6
                                                                6  5  7))
		       :uniforms   `(("P" . :projection)
                         ("M" . :model)
                         ("N" . :normal)                                    ;; Current normal matrix
                         ("ambientLight"  . ,(lambda () ambient-light))     ;; Ambient light
                         ("lightIntensity" . ,(lambda () sun-intensity))    ;; Brightness
                         ("lightDirection" . ,(lambda () sun-direction)))   ;; Direction


		       :attributes `(
              ("v" . ,(make-instance 'clinch:buffer
                 :Stride 3  ;; Stride 3, there are 3 float (x, y, z) for each vertex.
                 :data '(-0.5 0.0 0.0
                         0.5 0.0 0.0
                         -0.5 0.0 1.0
                         0.5 0.0 1.0
                         -0.5 0.0 1.0
                         0.5 0.0 1.0
                         -0.5 0.0 0.0
                         0.5 0.0 0.0)))
              ("n" . ,(make-instance 'clinch:buffer
                 :Stride 3          ;; Stride 3, there are 3 float (x, y, z) for each vertex.
                 :data '(0.0 -1.0 0.0
                         0.0 -1.0 0.0
                         0.0 -1.0 0.0
                         0.0 -1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0)))
              ("vertexColor" . ,(make-instance 'clinch:buffer
							   :stride 3    ;; There are three floats (red, green, blue) for each vertex.
                 :data '(0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0
                         0.0 1.0 0.0))))))


(defun point-on-circle (x0 y0 z0 r angle)
  "This returns points for a circle on the X-Z plane."
  (list (+ x0 (* r (cos angle))) y0 (+ z0 (* r (sin angle)))))

(defun add-offsets (value offsets) (mapcar #'(lambda (x) (+ x value)) offsets))

(defun cylinder (ambient-light sun-intensity sun-direction
                 &key (segments 6) (length 2.0) (radius 0.5) (colour '(0.6 0.3294 0.3529)))
	(make-instance 'clinch:entity
		       :shader-program  (clinch::get-generic-single-diffuse-light-per-vertex-color-shader)                                   ;; Add the shader
		       :uniforms   `(("P" . :projection)
                         ("M" . :model)
                         ("N" . :normal)                                    ;; Current normal matrix
                         ("ambientLight"  . ,(lambda () ambient-light))     ;; Ambient light
                         ("lightIntensity" . ,(lambda () sun-intensity))    ;; Brightness
                         ("lightDirection" . ,(lambda () sun-direction)))   ;; Direction
		       :indexes (make-instance 'clinch:index-buffer
                     :data (loop for i from 0 to segments append (add-offsets (* i 4) '(0 1 2 2 1 3))))
		       :attributes `(
              ("v" . ,(make-instance 'clinch:buffer
                 :Stride 3  ;; Stride 3, there are 3 float (x, y, z) for each vertex.
                 :data
                 ;; Find 'segments' points around a circle with the given radius, and for each such
                 ;; point generate 4 vertices - which represent a single wall
                    (mapcar 'ensure-float (loop for i from 0 to segments
                          for step = 0 then (/ (* PI 2) segments)
                          for angle = 0 then (* step i)
                          appending
                            (append nil
                                    (point-on-circle 0.0 0.0 0.0 radius angle)
                                    (point-on-circle 0.0 length 0.0 radius angle)
                                    (point-on-circle 0.0 0.0 0.0 radius (+ angle step))
                                    (point-on-circle 0.0 length 0.0 radius (+ angle step)))))))
              ("n" . ,(make-instance 'clinch:buffer
                 :Stride 3          ;; Stride 3, there are 3 float (x, y, z) for each vertex.
                 :data
                    (mapcar 'ensure-float (loop for i from 0 to segments
                           for step = 0 then (/ (* PI 2) segments)
                           for angle = 0 then (* step i)
                           appending
                              (append nil
                                      (point-on-circle 0.0 0.0 0.0 radius angle)
                                      (point-on-circle 0.0 0.0 0.0 radius angle)
                                      (point-on-circle 0.0 0.0 0.0 radius (+ angle step))
                                      (point-on-circle 0.0 0.0 0.0 radius (+ angle step)))))))
              ("vertexColor" . ,(make-instance 'clinch:buffer
							   :stride 3    ;; There are three floats (red, green, blue) for each vertex.
                 ;:data (loop for i from 0 to (+ (* segments 4) 3) append colour))))))
                 :data (loop for i from 0 to (* (+ (* segments 4) 3) 3) collecting
                                                                        (ensure-float (* (/ 1 (* (+ (* segments 4) 3) 3)) i))))))))
