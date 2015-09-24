(in-package #:tree)

(setf *random-state* (make-random-state t))

(defun get-slots(object)
  (get-class-slots (class-of object)))

(defun get-class-slots (class)
  ;; thanks to cl-prevalence
  #+openmcl
  (mapcar #'ccl:slot-definition-name
      (#-openmcl-native-threads ccl:class-instance-slots
       #+openmcl-native-threads ccl:class-slots
       class))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots class))
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots class))
  #+lispworks
  (mapcar #'hcl:slot-definition-name (hcl:class-slots class))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots class))
  #+clisp
  (mapcar #'clos:slot-definition-name (clos:class-slots class))
  #-(or openmcl cmu lispworks allegro sbcl clisp)
  (error "not yet implemented"))

(defmethod print-object ((object standard-object) stream)
  (if t
  (call-next-method)
  (format stream "{~s: ~s}" (type-of object)
      (loop for i in (get-slots object)
    collect (cons i (slot-value object i))))))

(defgeneric part-values(object)
  (:documentation "returns a plant parts values"))
(defmethod part-values (object)
  (let ((slots (get-slots object)))
    (if slots
	(loop for slot in slots collecting
	     (list slot (slot-value object slot)))
	object)))

(defclass supplies ()
  ((water 
    :initarg :water
    :initform 0
    :accessor water)
   (minerals
    :initarg :minerals
    :initform 0
    :accessor minerals)
   (sugar
    :initarg :sugar
    :initform 0
    :accessor sugar)
   (auxin
    :initarg :auxin
    :accessor auxin
    :initform 0)
   (growth
    :initarg :growth
    :initform 0
    :accessor growth)
   (abscisic-acid
    :initarg :abscisic-acid
    :initform 0
    :accessor abscisic-acid))
  (:documentation "represents the supplies available to any given plant part"))

(defgeneric less(s1 s2)
  (:documentation "checks if the first supplies are less than the second"))
(defmethod less((s1 supplies) (s2 supplies))
  (or (< (water s1) (water s2))
       (< (minerals s1) (minerals s2))
       (< (sugar s1) (sugar s2))
       (< (auxin s1) (auxin s2))
       (< (growth s1) (growth s2))
       (< (abscisic-acid s1) (abscisic-acid s2))))
(defmethod more((s1 supplies) (s2 supplies))
  (or (> (water s1) (water s2))
      (> (minerals s1) (minerals s2))
      (> (sugar s1) (sugar s2))
      (> (auxin s1) (auxin s2))
      (> (growth s1) (growth s2))
      (> (abscisic-acid s1) (abscisic-acid s2))))

(defun scale-supplies (supplies scale)
  (when supplies
    (let ((s (make-instance (class-of supplies))))
      (dolist (slot (get-slots s) s)
	(setf (slot-value s slot) (* (slot-value supplies slot) scale))))))

(defmacro aggr-replace (target function &rest supplies)
  "Replace all the slots of the given target with the result of applying
the given function to all the appropriate values from the provided supplies.
This is a nasty macro, in that it doesn't evaluate what it gets, it just inserts everything."
  (let ((slots (get-slots (make-instance 'supplies)))
	(target-name (gensym))
	(func-name (gensym)))
    `(let ((,target-name ,target)
	   (,func-name ,function))
       (loop for s in (list ,@supplies)
	    ,@(apply 'append 
		     (loop for slot in slots collect 
			  `(for ,slot = (,slot s) then 
				(funcall ,func-name ,slot (,slot s)))))
	    finally (progn 
		      ,@(loop for slot in slots collecting
			    `(setf (,slot ,target-name) ,slot)))
))))

(defun operate(function &rest supplies)
  (when supplies
    (let ((s1 (make-instance (class-of (first supplies)))))
      (dolist (slot (get-slots s1) s1)
	(setf (slot-value s1 slot) 
	      (loop for s in supplies
		 for total = (slot-value s slot) 
		 then (funcall function total (slot-value s slot))
		 finally (return total)))))))


(defmacro weighted-average (target supplies)
  "Replace all the slots of the given target with the weighted average of the appropriate values from the provided supplies.
'supplies' should be a list of '(supplies weight).
 This is a nasty macro, in that it doesn't evaluate what it gets, it just inserts everything."
  (let ((slots (get-slots (make-instance 'supplies)))
        (target-name (gensym))
	(weight-name (gensym))
	(item-name (gensym))
	(items-name (gensym))
	(n-name (gensym)))
    `(let* ((,target-name ,target)
	    (,items-name (remove-if 
			  #'(lambda(a)(or (not a) (eq 0 (second a)))) 
			  ,supplies)))
       (loop for (,item-name ,weight-name) in ,items-name
	  count ,weight-name into ,n-name
	    ,@(apply 'append 
		     (loop for slot in slots collect 
			  `(sum (* ,weight-name (,slot ,item-name)) into ,slot)))
	  finally (progn 
		    ,@(loop for slot in slots collecting
                           `(setf (,slot ,target-name) 
                                  (if (= 0 ,slot) 0 (/ ,slot ,n-name)))))
	    ))))

(defmacro average-supplies (target supplies sup-weight &rest parts)
  "Average out the supplies of the given parts.
  'target' is a supplies where the averaged values should end up in,
  'supplies' is a supplies from below
  'sup-weight' is the weight of the supplies from below
  'parts' are all the children whose supplies are to be averaged."
  (let ((slots (get-slots (make-instance 'supplies)))
	(target-name (gensym))
	(weight-name (gensym))
	(sup-weight-name (gensym))
	(supplies-name (gensym))
	(item-name (gensym))
	(items-name (gensym))
	(loop-sup-name (gensym))
	(n-name (gensym)))
    `(let* ((,target-name ,target)
	    (,items-name (list ,@parts))
	    (,sup-weight-name ,sup-weight)
	    (,supplies-name ,supplies))
       (loop for ,item-name in ,items-name 
	  for ,weight-name = (if ,item-name (flow-strength ,item-name) 0) then (if ,item-name (flow-strength ,item-name) 0)
	  for ,loop-sup-name = (if ,item-name (supplies ,item-name) NIL) then (if ,item-name (supplies ,item-name) NIL)
	  when (and ,item-name ,loop-sup-name)
	  sum ,weight-name into ,n-name
	    ,@(apply 'append 
		     (loop for slot in slots collect 
			  `(and sum (* ,weight-name (,slot ,loop-sup-name)) into ,slot)))
	    finally (progn 
		      (unless ,target-name
			(print "no target - adding")
			(setf ,target-name (make-instance 'supplies)))
		      (unless ,supplies-name
			(print "no supplies"))
		      (setf ,n-name ,sup-weight-name)
		      ,@(loop for slot in slots collecting
			    `(setf (,slot ,target-name) 
				   (/ (+ ,slot (* ,sup-weight-name (,slot ,supplies-name))) ,n-name))))
))))


(defun subtract-supplies(base flow &rest supplies)
  (when supplies
    (let ((s1 (make-instance (class-of base))))
      (dolist (slot (get-slots s1) s1)
	(setf (slot-value s1 slot) 
	      (/ (apply '- 
			(loop for s in (append `((,base ,flow)) supplies)
			   collecting
			     (* (slot-value (first s) slot) (second s))))
		 flow))))))

(defclass dna ()
  ((segment-width-gain 
    :initarg :segment-width-gain

    :initform 0.00005
    :accessor segment-width-gain
    :documentation "by how much a segment gains in width during 1 growth")
   (segment-length-gain
    :initarg :segment-length-gain
    :initform 0.01
    :accessor segment-length-gain
    :documentation "by how much a segment gains in length during 1 growth")
   (segment-requirements 
    :initarg :segment-requirements
    :initform (make-instance 'supplies :water 0.1 :sugar 0.001)
    :accessor segment-requirements
    :documentation "min. supplies needed to survive")
   (segment-min-growth-requirements 
    :initarg :segment-min-growth-requirements
    :initform (make-instance 'supplies :water 0.7 :sugar 0.7 :minerals 0.5
			     :growth 0.3)
    :accessor segment-min-growth-requirements
    :documentation "min. supplies needed to grow")
   (segment-max-growth-requirements 
    :initarg :segment-max-growth-requirements
    :initform (make-instance 'supplies :water 10 :sugar 10 :minerals 10
			     :auxin 0.7 :growth 1 :abscisic-acid 1)
    :accessor segment-max-growth-requirements
    :documentation "max. supplies needed to grow")
   (segment-growth-usage 
    :initarg :segment-growth-usage
    :initform (make-instance 'supplies :water 0.01 :sugar 0.01 :minerals 0.01)
    :accessor segment-growth-usage
    :documentation "how much supplies are used by a segment to grow")
   (segment-buds
    :initarg :segment-buds
    :initform 2
    :accessor segment-buds
    :documentation "how many lateral buds are on each segment")
   (segment-inter-rotation 
    :initarg :segment-rotation
    :initform 0
    :accessor segment-rotation
    :documentation "by how much each subsequent segment is rotated relative to the previous")
   (segment-production
    :initarg :segment-production
    :initform (make-instance 'supplies :auxin 0.1)
    :accessor segment-production
    :documentation "how much supplies a segment produces")

   (tip-sprout-time 
    :initarg :tip-sprout-time
    :initform 100
    :accessor tip-sprout-time
    :documentation "after how many hours a tip changes into a segment")
   (tip-sprout-times
    :initarg :tip-sprout-times
    :initform 4
    :accessor tip-sprout-times
    :documentation "how many times a tip sprouts buds before stopping (at which point it becomes a bud)")
   (tip-production
    :initarg :tip-production
    :initform (make-instance 'supplies :sugar 0.1 :auxin 0.4)
    :accessor tip-production
    :documentation "how much supplies a tip produces")
   (tip-max-growth-requirements 
    :initarg :tip-max-growth-requirements
    :initform (make-instance 'supplies :water 10 :sugar 10 :minerals 10
			     :auxin 7 :growth 1 :abscisic-acid 1)
    :accessor tip-max-growth-requirements
    :documentation "the max. supplies allowed for this to grow")

   (bud-sprout-time 
    :initarg :bud-sprout-time
    :initform 20
    :accessor bud-sprout-time
    :documentation "after how many hours a bud may sprout")
   (bud-requirements
    :initarg :bud-requirements
    :initform (make-instance 'supplies :water 0.001)
    :accessor bud-requirements
    :documentation "min. supplies needed for a bud to survive")
   (bud-min-sprout-requirements 
    :initarg :bud-min-sprout-requirements
    :initform (make-instance 'supplies :water 0.9 :sugar 0.7 :minerals 0.2
			     :growth 0.3 :auxin 0.02)
    :accessor bud-min-sprout-requirements
    :documentation "min. supplies needed for a bud to sprout")
   (bud-max-sprout-requirements 
    :initarg :bud-max-sprout-requirements
    :initform (make-instance 'supplies :water 10 :sugar 10 :minerals 10
			     :growth 1 :auxin 0.6 :abscisic-acid 0)
    :accessor bud-max-sprout-requirements
    :documentation "max. supplies needed for a bud to sprout")
   (bud-sprout-angle
    :initarg :bud-sprout-angle
    :initform 30
    :accessor bud-sprout-angle
    :documentation "the angle at which a bud will sprout")
   (bud-sprout-usage
    :initarg :bud-sprout-usage
    :initform (make-instance 'supplies :water 0.01 :sugar 0.2 :minerals 0.2)
    :accessor bud-sprout-usage
    :documentation "the angle at which a bud will sprout")
   (bud-production
    :initarg :bud-production
    :initform (make-instance 'supplies :sugar 0.1 :auxin 0.01)
    :accessor bud-production
    :documentation "how much supplies a bud produces")

   (leaf-growth-requirements
    :initarg :leaf-growth-requirements
    :initform (make-instance 'supplies :water 0.9 :sugar 0.7 :minerals 0.2)
    :accessor leaf-growth-requirements
    :documentation "supplies needed for a leaf to grow")
   (leaf-growth-usage
    :initarg :leaf-growth-usage
    :initform (make-instance 'supplies :water 0.5 :sugar 0.1 :minerals 0.2)
    :accessor leaf-growth-usage
    :documentation "supplies consumed by a leaf while growing")
   (leaf-requirements
    :initarg :leaf-requirements
    :initform (make-instance 'supplies :water 1/27)
    :accessor leaf-requirements
    :documentation "min. supplies needed for a leaf to survive")
   (leaf-max-requirements
    :initarg :leaf-max-requirements
    :initform (make-instance 'supplies :water 10 :sugar 10 :minerals 10
			     :auxin 0.999 :growth 1 :abscisic-acid 0)
    :accessor leaf-max-requirements
    :documentation "max. supplies allowed for a leaf to survive")
   (leaf-width-gain
    :initarg :leaf-width-gain
    :initform 0.04
    :accessor leaf-width-gain
    :documentation "by how much a leaf widens")
   (leaf-length-gain
    :initarg :leaf-length-gain
    :initform 0.1
    :accessor leaf-length-gain
    :documentation "by how much a leaf lengthens")
   (leaf-production
    :initarg :leaf-production
    :initform (make-instance 'supplies :sugar 1)
    :accessor leaf-production
    :documentation "how much supplies a leaf produces")
   (leaf-growth-time
    :initarg :leaf-growth-time
    :initform 25
    :accessor leaf-growth-time
    :documentation "how long a leaf grows for")
   (wilter-rate
    :initarg :wilter-rate
    :initform 0.01
    :accessor wilter-rate
    :documentation "how fast this plant wilters")
   (revive-rate
    :initarg :revive-rate
    :initform 0.01
    :accessor revive-rate
    :documentation "how fast this plant revives after wilting"))
  (:documentation "contains all information defining a tree"))

(defclass part ()
  ((health
    :initarg :health
    :initform 1
    :accessor health
    :documentation "when the health of a part is equal 0, that part is considered dead")
   (flow-strength
    :initarg :flow-strength
    :initform 1
    :accessor flow-strength
    :documentation "describes how much can flow through this part")
   (supplies
    :initarg :supplies
    :initform (make-instance 'supplies)
    :accessor supplies
    :documentation "what supplies are currently flowing through this part")
   (growth-time
    :initform 0
    :initarg :growth-time
    :accessor growth-time)
   (angles
    :initform #(1 0 0 0)
    :initarg :angles
    :accessor angles
    :documentation "by how much this part should be rotated relative to its parent part (in rads). This is a strictly personal thing - the base angles of various parts are defined in the dna"))
  (:documentation "a base class for all plant parts"))

(defclass leaf (part)
  ((width 
    :initarg :width
    :initform 0
    :accessor width)
   (leaf-len
    :initarg :length
    :initform 0
    :accessor leaf-len)
   (in-sun
    :initarg :in-sun
    :initform 1
    :accessor in-sun
    :documentation "how much this leaf is in the sun (from 0 to 1)")
   (petiole-strength
    :initform 1
    :accessor petiole-strength
    :documentation "when this is equeal 0 the leaf falls off")))

(defmethod initialize-instance :after ((leaf leaf) &key)
  (when (or (= (width leaf) 0) (= (leaf-len leaf) 0))
    (setf (flow-strength leaf) 0)))

(defclass bud (part)
  ((leaf
    :initarg :leaf
    :initform (make-instance 'leaf)
    :accessor leaf)))

(defclass tip (part)
  ((width
    :initarg :width
    :initform 0.1
    :accessor width)
   (height
    :initarg :height
    :initform 0
    :accessor height)
   (end
    :initarg :end
    :initform '(0 0 0)
    :accessor end
    :documentation "the end point of this segment")
   (sprouts
    :initarg :sprouts
    :initform 1
    :accessor sprouts
    :documentation "the amount of times this tip has sprouted buds"))
  (:documentation "an apical shoot"))


(defclass segment (tip)
  ((apex
    :initarg :apex
    :initform NIL
    :accessor apex)
   (buds
    :initarg :buds
    :initform NIL
    :accessor buds))
  (:documentation "a branch section"))


(defmethod initialize-instance :after ((tip segment) &key)
  (when (apex tip)
    (setf (angles (apex tip)) (angles tip))))

(defclass apex-segment (segment)
  ((apex
    :initarg :apex
    :initform (make-instance 'bud)
    :accessor apex))
  (:documentation "an apex segment (ending with a bud, not a tip)"))


(defclass internode-segment (segment)
  ((apex :initform (make-instance 'tip))
   (buds :initform (list (make-instance 'bud) (make-instance 'bud))))
  (:documentation "an internode segment"))

(defmethod initialize-instance :after ((tip internode-segment) &key)
   (when (apex tip)
     (setf (sprouts (apex tip)) (sprouts tip))))

(defgeneric production(part dna)
  (:documentation "returns how much this part produces"))
(defmethod production(part dna))
(defmethod production((part tip) dna)
  (segment-production dna))
(defmethod production((part apex-segment) dna)
  (tip-production dna))
(defmethod production((part leaf) dna)
  (scale-supplies (leaf-production dna) (in-sun part)))
(defmethod production((part bud) dna)
  (bud-production dna))

(defgeneric min-requirements(part dna)
  (:documentation "returns how much supplies this part needs to live (min amount)"))
(defmethod min-requirements(part dna))
(defmethod min-requirements((part tip) dna)
  (segment-requirements dna))
(defmethod min-requirements((part leaf) dna)
  "the amount of supplies a leaf needs is a function of its size. a leaf grows in both directions proportionaly, so its width can be used to decide how much supplies are needed. the max. amount of times it can grow is decided by the dna - if (* <the amount of steps> <base water requirements>) is bigger that 1, then a full grown leaf will automaticly start to wilt, because it requires too much water."
  (let ((supplies 
	 (make-instance 'supplies 
			:water (water (leaf-requirements dna)))))
    (setf (water supplies) (* (water supplies) 
			      (/ (width part) (leaf-width-gain dna))))
    supplies))
(defmethod min-requirements((part bud) dna)
  (bud-requirements dna))

(defgeneric max-requirements(part dna)
  (:documentation "returns how much supplies this part can survive (max amount)"))
(defmethod max-requirements(part dna))
(defmethod max-requirements((part leaf) dna)
  (leaf-max-requirements dna))


(defmacro vert-ang(part)
  `(first (angles ,part)))
(defmacro hor-ang(part)
  `(second (angles ,part)))
(defmacro deg-to-rad(angle)
  `(* 2 PI (/ ,angle 360)))

(defun get-end (point len angles)
  (let ((a (* len (cos (deg-to-rad (first angles))))))
    `(,(+ (first point) (* a (cos (deg-to-rad (second angles)))))
       ,(+ (second point) (* len (sin (deg-to-rad (first angles)))))
       ,(+ (third point) (* a (sin (deg-to-rad (second angles))))))))
