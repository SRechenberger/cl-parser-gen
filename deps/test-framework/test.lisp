(in-package :com.gigamonkeys.test)

(defvar *test-name* nil)

(defmacro define-test (name parameters &body body)
  "Define a test function. Within a test function we can call other
test functions or use `check' to run individual test cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in `forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating `forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by `check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro define-case-test ((name parameters &key (all-test-name nil atn-p)) &body test-cases)
;; Assuming the body like
;;  (CASE-1 &REST rest1)
;;  (CASE-2 &REST rest2)
;; this macro expands to 
;;  (defgeneric NAME (TESTCASE @PARAMETERS))
;;  (defmethod NAME ((TESTCASE (eql CASE-1))) ...)
;;  (defmethod NAME ((TESTCASE (eql CASE-2))) ...)"
  (let ((generic-case (gensym))
	(case-names (mapcar #'first test-cases)))
    `(progn
       (defgeneric ,name (,generic-case ,@parameters)
	 (:documentation "none for now..."))

       ,@(loop
	  for test-case in test-cases
	  collect (destructuring-bind (case-name &rest case-body) test-case
		    `(defmethod ,name ((,generic-case (eql ',case-name)) ,@parameters)
		       (let ((*test-name* (append *test-name* (list (list ',name ',case-name)))))
			 ,@case-body))))
       ,(when atn-p
	  `(define-test ,all-test-name ,parameters
	     (check
	       ,@(loop
		   for case-name in case-names
		   collect `(,name ',case-name))))))))

(defmacro define-test-for-all (name (test-object test-objects) (&rest parameters) &body body)
  `(define-test ,name ,parameters
     (combine-results
       ,@(loop
	    for to in test-objects
	    collect `(let ((,test-object ,to))
		       ,@body)))))

(defmacro define-test-method (fun-name parameters &body body)
  `(defmethod ,fun-name ,@parameters
     (let ((*test-name* (append *test-name* (list ',fun-name))))
       ,@body)))	  
