(in-package #:tree-sim)


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
