(in-package #:tree-sim)

;;; the algorythm is as follows:
;;; 1. diffuse supplies - which means to simply send up water etc. and 
;;;    collect sugars. the amount of supplies is in a feedback loop
;;;    to simulate the diffusion of minerals etc.
;;; 1b. send auxin down. this would be best integrated into the previous
;;;    step, but the important thing is for it to work.
;;; 2. check health - whether the amount of available supplies is enough
;;;    substain the part (not implemented yet)
;;; 3. grow - that is change supplies into biomass.

(defparameter *dna* (make-instance 'dna))
(defparameter *tree* (make-instance 'internode-segment :height 1))

(defun set-temp(temp)
  (defparameter *temperature* temp)
  (if (> temp 5)
      (defparameter *supplies* (make-instance 'supplies :growth 4/10
					      :auxin 1/4
					      :sugar 1 :minerals 1
					      :water 1
					      :abscisic-acid 0))
      (defparameter *supplies* (make-instance 'supplies :growth 0
					      :auxin 1/4
					      :sugar 1 :minerals 1 
					      :water 1
					      :abscisic-acid 1))))
(set-temp 1)
(set-temp 10)

(defun sunshine (tree dna)
  "Shine on this crazy (in another million years) diamond."
  (let ((shadow-map (make-hash-table :test #'equal)))
    (map-shadow shadow-map tree dna
		(vector 0 0 0 0) *sun-angle*)
    (shine shadow-map)))

(defun run-rounds (amount)
  (dotimes (i amount T)
    (when *use-supplies*
      (diffuse *supplies* *tree* *dna*))
    (when *diffuse-auxin*
      (diffuse-auxin (auxin *supplies*) *tree* *dna*))
    (when *health-checks*
      (health-check *tree* *dna*))
    (when (and *sunshine* (= 0 (mod i 5)) (not (winter-p)))
      (sunshine *tree* *dna*))
    (when *growth*
      (grow *tree* *dna*))))

(defun simulate-years (years dna)
  (dotimes (year years)
    (run-rounds (* 200 (tip-sprout-times *dna*)))
    (when *seasons*
      (set-temp 1)
      (run-rounds 150)
      (set-temp 10)
      (run-rounds 150))))

(defun reset-tree ()
  (setf *tree* 
	(make-instance 'internode-segment 
		       :height 1 :supplies *supplies*)))

(print "doing a couple of years to start off with")
(with-output-to-string (*trace-output*)
  (time
   (progn
     (reset-tree)
     (simulate-years 3 *dna*))))
(print "done")

(with-output-to-string (*trace-output*)
  (time
   (progn
     (dotimes (i 50)
       (grow *tree* *dna*)))))
;       (diffuse *supplies* *tree* *dna*)) ;)))
;    (diffuse-auxin (auxin *supplies*) *tree* *dna*)
;    (health-check *tree* *dna*)(with-output-to-string (*trace-output*)
;    (when NIL
;      (let ((shadow-map (make-hash-table :test #'equal)))
;	(map-shadow shadow-map *tree* *dna* 
;		    (vector 0 0 0 0) (vector 1 0 0 0))
;	(shine shadow-map)
;	))
;    (grow *tree* *dna*)
;)))

(progn
  (setf *tmp-tree* *tree*)
  (setf *tree* (make-instance 'internode-segment :height 1))
  T)
(progn (setf *tree* *tmp-tree*) T)

