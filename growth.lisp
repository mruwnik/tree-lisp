(in-package #:tree-sim)

(defun winter-p ()
  (and *seasons* (< *temperature* 5)))

(defgeneric growth-ratio(supplies part dna)
  (:documentation "calculates how much the given part should grow by proportionaly. anything less than 0 means no growth"))
(defmethod growth-ratio :around (supplies part dna)
  (if (and *growth-ratio* (not (winter-p)))
      (call-next-method)
      (if (winter-p) 0 1)))
(defmethod growth-ratio(supplies part dna)
  0)
(defmethod growth-ratio(supplies (leaf leaf) dna)
  (if (and *use-supplies* (less supplies (leaf-growth-requirements dna)))
      0 (if *sunshine* (in-sun leaf) 1)))

(defmethod growth-ratio(supplies (bud bud) dna)
  (unless *use-supplies*
    (return-from growth-ratio 1))
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
    (unless *use-supplies*
      (return-from growth-ratio 1))
    (if (or (less supplies (segment-min-growth-requirements dna))
	    (more supplies (tip-max-growth-requirements dna)))
      0 1))

(defmethod growth-ratio(supplies (segment segment) dna)
  (unless *use-supplies*
    (return-from growth-ratio 1))
  (- 1 (/ (abs 
	   (- (/ (- (auxin (segment-max-growth-requirements dna))
		    (auxin (segment-min-growth-requirements dna)))
		 2)
	      (- (auxin supplies)
		 (auxin (segment-min-growth-requirements dna)))))
	      (/ (- (auxin (segment-max-growth-requirements dna))
		    (auxin (segment-min-growth-requirements dna)))
		 2))))

(defgeneric growth-consume (part dna growth-ratio)
  (:documentation "Consume the appropriate amount of supplies needed to grow."))
(defmethod growth-consume (part dna growth-ratio))
(defmethod growth-consume :around (part dna growth-ratio)
  (when *use-supplies*
    (call-next-method)))
(defmethod growth-consume ((tip tip) dna growth-ratio)
  (setf (supplies tip)
	(operate '- (supplies tip) 
		 (operate (lambda (a)(/ a growth-ratio))
			  (segment-growth-usage dna)))))
(defmethod growth-consume ((segment segment) dna growth-ratio)
  (setf (supplies segment) 
	(operate '- (supplies segment)
		 (segment-growth-usage dna))))


(defgeneric increase (part dna growth-ratio)
  (:documentation "Increase the part by growing."))
(defmethod increase (part dna growth-ratio))
(defmethod increase ((leaf leaf) dna growth-ratio)
  (unless (> (growth-time leaf) (leaf-growth-time dna))
    (incf (width leaf) (* growth-ratio (leaf-width-gain dna)))
    (incf (leaf-len leaf) (* growth-ratio (leaf-length-gain dna)))
    (setf (flow-strength leaf)
	  (/ (* (width leaf) (leaf-len leaf))
	     (* (leaf-width-gain dna)
		(leaf-length-gain dna)
		(expt (leaf-growth-time dna) 2))))))
(defmethod increase ((tip tip) dna growth-ratio)
  (unless (> (growth-time tip) (tip-sprout-time dna))
;    (when *sunshine*
 ;     (setf growth-ratio (* growth-ratio (in-sun tip))))
    (incf (width tip) (* (segment-width-gain dna) growth-ratio))
    (incf (height tip) (* (segment-length-gain dna) growth-ratio))))
(defmethod increase ((segment segment) dna growth-ratio)
  (incf (width segment) (* (segment-width-gain dna) growth-ratio)))


(defgeneric sprout (part dna)
  (:documentation "Sprout into a new part if the conditions are right."))
(defmethod sprout (part dna))
(defmethod sprout ((part bud) dna)
  (when (> (growth-time part) (bud-sprout-time dna))
    (unless (and *seasons* (leaf part))
      (make-instance 'tip :health (health part)))))
(defmethod sprout ((segment segment) dna))
(defmethod sprout ((tip tip) dna)
  (when (and (> (growth-time tip) (tip-sprout-time dna))
	     (or (not *sunshine*) (> (in-sun tip) (bud-sprout-light dna))))
      (make-instance 
       (if (or (not *seasons*) (< (sprouts tip) (tip-sprout-times dna)))
	   'internode-segment 'apex-segment)
       :height (height tip)
       :width (width tip)
       :sprouts (1+ (sprouts tip))
       :health (health tip)
       :supplies (supplies tip)
       :angles (if (< (sprouts tip) (tip-sprout-times dna))
		   (angles tip) (quart-normalise 
				 (quarternion (/ PI 2) 0 1 0))))))

(defgeneric increase-grow-time (part growth-ratio)
  (:documentation "Increse the growth time counter"))
(defmethod increase-grow-time (part growth-ratio))
(defmethod increase-grow-time ((bud bud) growth-ratio)
  (when (or (not *growth-ratio*) (> growth-ratio 0))
    (incf (growth-time bud) growth-ratio)))
(defmethod increase-grow-time ((leaf leaf) growth-ratio)
  (when (or (not *growth-ratio*) (> growth-ratio 0))
    (incf (growth-time leaf))))
(defmethod increase-grow-time ((tip tip) growth-ratio)
; this will cause bad days to also count towards the time to
;  sprout - hence lower branches will be shorter
  (incf (growth-time tip)))


(defgeneric grow-children (part dna)
  (:documentation "Cause all children parts to grow."))
(defmethod grow-children (part dna))
(defmethod grow-children ((bud bud) dna)
  (setf (leaf bud) (grow (leaf bud) dna)))
(defmethod grow-children ((segment segment) dna)
  (setf (apex segment) (grow (apex segment) dna))
  (setf (buds segment)
	(remove-if 'null
		   (loop for bud in (buds segment) collecting
			(grow bud dna)))))

(defgeneric grow(part dna)
 (:documentation "causes the given part to grow or whatever. returns this part"))
(defmethod grow(part dna)
  (when part
    (grow-children part dna)
    (let ((growth (growth-ratio (supplies part) part dna)))
      (increase-grow-time part growth)
      (unless (> growth 0)
	(return-from grow part))
      (growth-consume part dna growth)
      (or (sprout part dna) 
	  (progn (increase part dna growth) part)))))
