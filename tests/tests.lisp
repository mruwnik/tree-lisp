(in-package #:tree-sim)

(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro runtest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `((lambda ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body))))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)x



;;; test growth ratios
(deftest test-leaf-growth-ratio()
  (let* ((dna (make-instance 'dna))
	 (supplies (make-instance 'supplies :growth 0.4
					:auxin 0.25
					:sugar 1 :minerals 1 
					:water 1))
	 (leaf (make-instance 'leaf)))
    (check
      (> (growth-ratio supplies leaf dna) 0)
      (= (growth-ratio (operate '- (leaf-growth-requirements dna) supplies)
		       leaf dna) 0)
      (= 0 (growth-ratio supplies 
		       (make-instance 'leaf :growth-time 
					       (1+ (leaf-growth-time dna)))
		       dna)))))

(deftest test-leaf-growth()
  (let* ((dna (make-instance 'dna))
	 (supplies (make-instance 'supplies :growth 0.4
					:auxin 0.25
					:sugar 1 :minerals 1 
					:water 1))
	 (leaf (make-instance 'leaf :supplies supplies)))
    (check
      (equal (grow leaf dna) leaf)
      (setf (health leaf) 0)
      (equal (grow leaf dna) leaf)
      (setf (petiole-strength leaf) -1)
      (not (grow leaf dna)))))
