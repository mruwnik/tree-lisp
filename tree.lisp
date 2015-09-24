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
      0 (in-sun leaf)))

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
     
(defmethod growth-ratio(supplies (segment tip) dna)
    (if (or (less supplies (segment-min-growth-requirements dna))
	    (more supplies (tip-max-growth-requirements dna)))
      0 1))

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

(defmethod diffuse(supplies (part tip) dna)
   (with-slots ((part-supplies supplies)) part
     (aggr-replace part-supplies '+ supplies (tip-production dna))))

(defmethod diffuse(supplies (part segment) dna)
  (unless (is-dead part)
    (with-slots ((part-supplies supplies)
		 (apex apex)
		 (buds buds)) part
      (setf (flow-strength part) 
	    (+ (if apex (flow-strength apex) 0)
	       (loop for bud in buds when bud sum (flow-strength bud))))
      
      (when apex
	(diffuse part-supplies apex dna))
      (dolist (bud buds)
	(when bud
	  (diffuse part-supplies bud dna)))

      (average-supplies part-supplies
	  supplies (flow-strength part)
	  apex (first buds) (second buds))
      (let ((prod (production part dna)))
	(when prod
	  (aggr-replace part-supplies '+ supplies prod))))))


(defmethod diffuse(supplies (part bud) dna)
  (with-slots ((part-supplies supplies)
	       (leaf leaf)) part
    (if (is-dead leaf)
	(progn 
	  (setf (flow-strength part) 1)
	  (setf part-supplies supplies))
	(progn
	  (setf (flow-strength part) 
		(1+ (flow-strength leaf)))
	  (diffuse supplies leaf dna)
	  (average-supplies part-supplies 
	     supplies (flow-strength part) leaf)))
    (aggr-replace part-supplies '+ part-supplies (production part dna))))

(defmethod diffuse(supplies (part leaf) dna)
  (with-slots ((part-supplies supplies)) part
    (aggr-replace part-supplies '+ supplies (production part dna))))


(defgeneric diffuse-auxin(auxin part dna)
  (:documentation "diffuses auxin through the plant.
 - a tip sets its auxin level to the amount it produces and returns the same
 - a bud sets its level to the amount given and returns the amount it produces
 - a segment sets its level to the sum of its childrens levels and returns the same"))
(defmethod diffuse-auxin(auxin (part part) dna)
  auxin)
(defmethod diffuse-auxin(auxin (tip tip) dna)
  (setf (auxin (supplies tip)) (auxin (production tip dna))))
(defmethod diffuse-auxin(auxin (tip apex-segment) dna)
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
	      (make-instance 'tip :health (health part)))
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
	      ; if it's time for the tip to grow a new segment, do so
	       (make-instance 
		(if (< (sprouts tip) (tip-sprout-times dna))
		    'internode-segment 'apex-segment)
		:height (height tip)
		:width (width tip)
		:sprouts (1+ (sprouts tip))
		:health (health tip)
		:supplies (supplies tip)
		:angles (if (< (sprouts tip) (tip-sprout-times dna))
			    (angles tip) (quart-normalise 
					  (quarternion (/ PI 2) 0 1 0))))
	      (progn
		(incf (width tip) 
		      (* (segment-width-gain dna) growth-ratio))
		(incf (height tip) 
		      (* (segment-length-gain dna) growth-ratio))
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

(defun run-rounds (amount)
  (dotimes (i amount T)
    (diffuse *supplies* *tree* *dna*)
    (diffuse-auxin (auxin *supplies*) *tree* *dna*)
    (health-check *tree* *dna*)
    (when (= 0 (mod i 5)) 
      (let ((shadow-map (make-hash-table :test #'equal)))
	(map-shadow shadow-map *tree* *dna* 
		    (vector 0 0 0 0) (vector 1 0 0 0))
	(shine shadow-map)))
    (grow *tree* *dna*)))

(defun simulate-years (years dna)
  (dotimes (year years)
    (set-temp 1)
    (run-rounds 150)
    (set-temp 10)
    (run-rounds (* 200 (tip-sprout-times dna)))))


(print "doing a couple of years to start off with")
(with-output-to-string (*trace-output*)
  (time
   (progn
     (setf *tree* (make-instance 'internode-segment :height 1))
     (simulate-years 4 *dna*))))
(print "done")

(with-output-to-string (*trace-output*)
  (time
   (progn
     (dotimes (i 50)
       (diffuse *supplies* *tree* *dna*)) ;)))
;    (diffuse-auxin (auxin *supplies*) *tree* *dna*)
;    (health-check *tree* *dna*)(with-output-to-string (*trace-output*)
    (when NIL
      (let ((shadow-map (make-hash-table :test #'equal)))
	(map-shadow shadow-map *tree* *dna* 
		    (vector 0 0 0 0) (vector 1 0 0 0))
;	(shine shadow-map)
	))
;    (grow *tree* *dna*)
)))


(progn
  (setf *tmp-tree* *tree*)
  (setf *tree* (make-instance 'internode-segment :height 1))
  T)
(progn (setf *tree* *tmp-tree*) T)


;(map-shadow *shadow-map* *tree* *dna* (vector 0 0 0 0) (vector 1 0 0 0))

;(let ((bla (shine *shadow-map*)))(with-output-to-string (*trace-output*)
;  (loop for key being the hash-keys of bla collect (gethash key bla)))


(defgeneric count-parts (part)
  (:documentation "a helper function to count how many children parts the given part has"))
(defmethod count-parts(part) 0)
(defmethod count-parts((segment segment))
  (+ (count-parts (apex segment)) 
     (loop for bud in (buds segment) sum (count-parts bud))))
(defmethod count-parts((bud bud))
  (if (leaf bud) 2 1))

(count-parts *tree*)


