(in-package :paren-psos)

(defparameter *client-debug-p* t)

(ps:defpsmacro effective-throw (error-obj)
  `(progn
    (log ,error-obj :error)
    (console.trace)
    (throw ,error-obj)))

(defparameter *console-output* nil)
(defparameter *firebug-output* t)

(ps:defpsmacro log (message &optional level (scalar-level 100))
  (declare (ignore scalar-level))
;  (format t "Logging c/f ~A ~A~%" *console-output* *firebug-output*)
  (if (and *client-debug-p*
					;	   nil)
	   t)
					;	   (or (> scalar-level 10000) (eql :error level)))
					;	   (or (> scalar-level 101) (equal :error level) (equal :warn level)))
      (let ((result
	     `(cond
	       ,@(when *console-output*
		       (list `(js-global::print (js-global::print ,message))))
	       ,@(when *firebug-output*
		       (list
			`((and
			   js-global::window
			   js-global::window.console
			   js-global::console.firebug)
			  ,(case level
				 (:error `(methcall :error js-global::console ,message))
				 (:warn `(methcall :warn js-global::console.warn ,message))
				 (:warning `(methcall :warn js-global::console ,message))
				 ((:info nil) `(methcall :info js-global::console ,message)))))))))
;	(format t "~S~%" result)
	result)))
	

(ps:defpsmacro debug-time-start (str)
  (if *client-debug-p*
      `(if console (console.time ,str))))

(ps:defpsmacro debug-time-end (str)
  (if *client-debug-p*
      `(if console (console.time-end ,str))))

(ps:defpsmacro with-debug-timer (str &rest body)
  `(progn
    (debug-time-start ,str)
    ,@body
    (debug-time-end ,str)))


(ps:defpsmacro dolist2 (i-array &rest body)
  (ps:with-ps-gensyms (arrvar idx)
    (let ((var (first i-array))
	  (array (second i-array))
	  (direction (or (third i-array) :forward)))
      `(let ((,arrvar ,array))
	(do ((,idx
	      ,@(if (eql :forward direction)
		    `(0 (1+ ,idx))
		    `((1- (slot-value ,arrvar 'length)) (1- ,idx)))))
	    (,(if (eql :forward direction)
		  `(>= ,idx (slot-value ,arrvar 'length))
		  `(< ,idx 0)))
	  (let ((,var (aref ,arrvar ,idx)))
	    ,@body))))))

(defun parse-function-body (body)
;  (format t "parsing function body ~A~%" body)
  (let* ((documentation
	  (when (stringp (first body))
	    (first body)))
	 (body-forms (if documentation (rest body) body)))
    (values
     body-forms
     documentation)))

#+nil
(defun parse-extended-function (lambda-list body &optional name)
  "Returns the effective body for a function with the given lambda-list and body."
  (declare (ignore name))
  (labels ((default-part (arg) (if (listp arg) (second arg)))
	   (name-part (arg) (if (listp arg) (first arg) arg)))
    (multiple-value-bind (requireds optionals rest? rest keys? keys)
	(paren-psos::parse-lambda-list lambda-list)
;      (format t "~A .." rest)
      (let* ((options-var 'options)
	     (defaulting-args ;an alist of arg -> default val
		 (remove-if
		  #'null (mapcar #'(lambda (arg) (when (default-part arg)
						   (cons (name-part arg) (default-part arg))))
				 (append requireds optionals keys))))
	     (arg-names (mapcar #'name-part
				(append requireds optionals)))
	     (effective-args (append arg-names
				     (if keys? (list options-var))))
	     (body-paren-forms (parse-function-body body)) ;remove documentation
	     (body-with-defaulters
	      (append (mapcar #'(lambda (default-pair)
				  `(defaultf ,(car default-pair) ,(cdr default-pair)))
			      defaulting-args)
		      body-paren-forms))
	     (effective-body
	      (if rest?
		  (append (list `(defvar ,rest ((slot-value (to-array arguments) 'slice)
						,(length effective-args))))
			  body-with-defaulters)
		  body-with-defaulters))
	     (effective-body
	      (if keys?
		  (list `(with-slots ,(mapcar #'name-part keys) ,options-var
			  ,@effective-body))
		  effective-body)))
	(values effective-args effective-body)))))

;old:  `(js:defvar ,formal-name (lambda ,arguments ,@body)))

