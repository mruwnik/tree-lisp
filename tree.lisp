(in-package #:tree)

;;; the algorythm is as follows:
;;; 1. diffuse supplies - which means to simply send up water etc. and 
;;;    collect sugars. the amount of supplies is in a feedback loop
;;;    to simulate the diffusion of minerals etc.
;;; 1b. send auxin down. this would be best integrated into the previous
;;;    step, but the important thing is for it to work.
;;; 2. check health - whether the amount of available supplies is enough
;;;    substain the part (not implemented yet)
;;; 3. grow - that is change supplies into biomass.


(defgeneric is-dead(part)
  (:documentation "check if this part is dead"))
(defmethod is-dead(part)
  t)
(defmethod is-dead((part part))
  (not (and part (> (health part) 0))))


(defgeneric health-check(part dna)
  (:documentation "checks that the given part has enough supplies to survice, and if not, then reduces this parts health. otherwise heals it if such is needed"))
(defmethod health-check(part dna))
(defmethod health-check((part part) dna)
  (unless (is-dead part)
    (if (or (less (supplies part) (min-requirements part dna))
	    (and (max-requirements part dna)
		 (more (supplies part) (max-requirements part dna))))
	(decf (health part) (wilter-rate dna))
	(when (< (health part) 1)
	  (incf (health part) (revive-rate dna)))))
  (health part))
(defmethod health-check((bud bud) dna)
  (health-check (leaf bud) dna)
  (call-next-method))
(defmethod health-check((part segment) dna)
  (health-check (apex part) dna)
  (loop for bud in (buds part) do
       (health-check bud dna))
  (call-next-method))

(defgeneric growth-ratio(supplies part dna)
  (:documentation "calculates how much the given part should grow by proportionaly. anything less than 0 means no growth"))
(defmethod growth-ratio(supplies part dna)
  0)
(defmethod growth-ratio(supplies (leaf leaf) dna)
  (if (or (> (growth-time leaf) (leaf-growth-time dna))
	  (less supplies (leaf-growth-requirements dna)))
      0 1))

(defmethod growth-ratio(supplies (bud bud) dna)
  (if (or (less supplies (bud-min-sprout-requirements dna))
	  (more supplies (bud-max-sprout-requirements dna)))
      (progn
	(setf (growth-time bud)
	      (if (> (growth-time bud) 0)
		  (1- (growth-time bud))
		  0))
	0)
      (- 1 (/ (abs (- 
		    (/ (- (auxin (bud-max-sprout-requirements dna))
			  (auxin (bud-min-sprout-requirements dna)))
		       2)
		    (- (auxin supplies)
		       (auxin (bud-min-sprout-requirements dna)))))
	      (/ (- (auxin (bud-max-sprout-requirements dna))
		    (auxin (bud-min-sprout-requirements dna)))
		 2)))))
     
(defmethod growth-ratio :around (supplies (segment tip) dna)
  (if (or (less supplies (segment-min-growth-requirements dna))
	  (more supplies (segment-max-growth-requirements dna)))
      0 (call-next-method)))
(defmethod growth-ratio(supplies (segment tip) dna)
  1)
(defmethod growth-ratio(supplies (segment segment) dna)
  (- 1 (/ (abs 
	   (- (/ (- (auxin (segment-max-growth-requirements dna))
		    (auxin (segment-min-growth-requirements dna)))
		 2)
	      (- (auxin supplies)
		 (auxin (segment-min-growth-requirements dna)))))
	      (/ (- (auxin (segment-max-growth-requirements dna))
		    (auxin (segment-min-growth-requirements dna)))
		 2))))

(defgeneric get-children(part)
 (:documentation "gets the given parts children"))
(defmethod get-children(part))
(defmethod get-children((part bud))
  (when (leaf part)
    (list (leaf part))))
(defmethod get-children((part segment))
  (remove NIL (append (list (apex part)) (buds part))))

(defgeneric diffuse(supplies part dna)
 (:documentation "saturates the tree with supplies. on the way up it sends water, minerals and growth hormones. on the way down it sends sugar"))
; default method in case a NIL part is used
(defmethod diffuse(supplies part dna))

(defmethod diffuse :before (supplies (part bud) dna)
  (if (is-dead (leaf part))
      (setf (flow-strength part) 1)
      (setf (flow-strength part) 
	    (1+ (flow-strength (leaf part))))))
(defmethod diffuse :before (supplies (part segment) dna)
  (setf (flow-strength part) 
	(reduce '+ (mapcar 'flow-strength (get-children part)))))

(defmethod diffuse(supplies (part part) dna)
  (unless (is-dead part)
    (setf (supplies part) 
	  (apply 'average 
		 (list supplies (flow-strength part))
		 (list (supplies part) (flow-strength part))
		 (mapcar #'(lambda(a)(diffuse supplies a *dna*))
			 (get-children part))))

    (unless (supplies part)  ; make sure this part has any supplies
      (setf (supplies part) supplies))

    (when (production part dna)
	(setf (supplies part)
	      (operate '+ supplies (production part dna))))
    (list supplies (flow-strength part))))

(defmethod diffuse :around (supplies (segment segment) dna)
  (let ((auxin (auxin (supplies segment)))
	(supplies (call-next-method)))
    (setf (auxin (supplies segment)) auxin)
    supplies))

(defgeneric diffuse-auxin(auxin part dna)
  (:documentation "diffuses auxin through the plant.
 - a tip sets its auxin level to the amount it produces and returns the same
 - a bud sets its level to the amount given and returns the amount it produces
 - a segment sets its level to the sum of its childrens levels and returns the same"))
(defmethod diffuse-auxin(auxin (part part) dna)
  auxin)
(defmethod diffuse-auxin(auxin (tip tip) dna)
  (setf (auxin (supplies tip)) (auxin (production tip dna))))
(defmethod diffuse-auxin(auxin (segment segment) dna)
  (setf (auxin (supplies segment)) 
	(reduce '+ 
		(mapcar #'(lambda(a)
			    (diffuse-auxin 
			     (auxin (supplies segment)) a *dna*))
			(get-children segment)))))
(defmethod diffuse-auxin(auxin (bud bud) dna)
  (setf (auxin (supplies bud)) auxin)
  (auxin (production bud dna)))

(defgeneric grow(part dna)
 (:documentation "causes the given part to grow or whatever. returns this part"))
(defmethod grow(part dna))
(defmethod grow((leaf leaf) dna)
  (if (is-dead leaf)
      (unless (< (petiole-strength leaf) 0)
	(decf (petiole-strength leaf) (random 0.1))
	leaf)
      (let ((growth (growth-ratio (supplies leaf) leaf dna)))
	(when (> growth 0)
	  (incf (width leaf) (* growth (leaf-width-gain dna)))
	  (incf (leaf-len leaf) (* growth (leaf-length-gain dna)))
	  (incf (growth-time leaf))
	  (setf (flow-strength leaf)
		(/ (* (width leaf) (leaf-len leaf))
		   (* (leaf-width-gain dna)
		      (leaf-length-gain dna)
		      (expt (leaf-growth-time dna) 2)))))
	leaf)))

(defmethod grow((part bud) dna)
  (setf (leaf part) (grow (leaf part) dna))
  (if (is-dead (leaf part))
    (let ((growth-ratio (growth-ratio (supplies part) part dna)))
      (if (> growth-ratio 0)
	  (if (< (growth-time part) (bud-sprout-time dna))
	      (progn 
		(incf (growth-time part) growth-ratio)
		part)
	      (set-pos (make-instance 'tip :health (health part)
			     :angles (angles part)
			     :pos (pos part)) dna))
	  part))
    part))

(defmethod grow((tip tip) dna)
  (incf (growth-time tip)) ; this will cause bad days to also count 
                           ; towards the time to sprout - hence
                           ; lower branches will be shorter
  (let ((growth-ratio (growth-ratio (supplies tip) tip dna)))
    (if (> growth-ratio 0)
	(progn 
	  (setf (supplies tip)
		(operate '- (supplies tip) 
			 (operate (lambda (a)(/ a growth-ratio))
				  (segment-growth-usage dna))))
	  (if (> (growth-time tip) (tip-sprout-time dna))
	      (set-pos (make-instance 'segment :height (height tip)
			     :width (width tip)
			     :health (health tip)
			     :supplies (supplies tip)
			     :pos (pos tip)
			     :end (end tip)
			     :angles (angles tip))
		       dna)
	      (progn
		(incf (width tip) 
		      (* (segment-width-gain dna) growth-ratio))
		(incf (height tip) 
		      (* (segment-length-gain dna) growth-ratio))
		(setf (end tip)
		      (get-end (pos tip) (height tip) (angles tip)))
		tip)))
	tip)))

(defmethod grow((part segment) dna)
  (unless (is-dead part)
    (setf (apex part) (grow (apex part) dna))
    (setf (buds part)
	  (remove-if 'null
	   (loop for bud in (buds part) collecting
		(grow bud dna))))

    (let ((growth (growth-ratio (supplies part) part dna)))
      (when (> growth 0)
	(setf (supplies part) 
	      (operate '- (supplies part)
		       (segment-growth-usage dna)))
	(incf (width part) (* (segment-width-gain dna) growth)))))
  part)

(defgeneric set-pos (part dna)
  (:documentation "calculates and sets up all neceseray positional info"))
(defmethod set-pos ((part part) dna)
  part)
(defmethod set-pos ((segment segment) dna)
  (setf (end segment) 
	(get-end (pos segment) (height segment) (angles segment)))
  (when (apex segment)
    (setf (angles (apex segment)) (angles segment))
    (set-pos (apex segment) dna))
  (let ((angle 0)
	(angle-step (/ 360 (segment-buds dna)))
	(var (/ (bud-sprout-angle dna) 20)))
    (dolist (bud (buds segment))
      (when bud
	(setf (pos bud) (end segment))
	(setf (angles bud)
	      `(,(+ (- 90 (bud-sprout-angle dna))
		    (vert-ang segment) 
		    (* var (- 1 (* 1/50 (random 100)))))
		 ,(+ angle (hor-ang segment) 
		  (* var (- 1 (* 1/50 (random 100)))))))
	(set-pos bud dna))
      (incf angle angle-step)))
  segment)
;; doesn't work once a point has been rotated around the z-axis.

(defparameter *dna* (make-instance 'dna))
(defparameter *tree* (set-pos (make-instance 'segment :height 1) *dna*))

(defun set-temp(temp)
  (defparameter *temperature* temp)
  (if (> temp 5)
      (defparameter *supplies* (make-instance 'supplies :growth 0.4
					      :auxin 0.25
					      :sugar 1 :minerals 1 
					      :water 1
					      :abscisic-acid 0))
      (defparameter *supplies* (make-instance 'supplies :growth 0
					      :auxin 0.25
					      :sugar 1 :minerals 1 
					      :water 1
					      :abscisic-acid 1))))
(set-temp 1)
(set-temp 10)
(part-values *dna*)
(dotimes (i 100)
  (diffuse *supplies* *tree* *dna*)
  (diffuse-auxin (auxin *supplies*) *tree* *dna*)
  (health-check *tree* *dna*)
  (grow *tree* *dna*)
)

(defparameter *part* (leaf (first (buds *tree*))))

(part-values (grow *part* *dna*))
(part-values (supplies  *tree*))
(part-values (supplies (first (buds *tree*))))
(part-values (supplies (apex *tree*)))
(part-values (supplies (apex (apex *tree*))))

(part-values (supplies (first (buds *tree*))))
(part-values (leaf (first (buds *tree*))))
(part-values (min-requirements (leaf (first (buds *tree*))) *dna*))

