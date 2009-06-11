(in-package org.iodb.paren-psos)

(defvar *include-documentation* nil)

(defun parse-direct-slot (slot-name &rest options)
  "Parses a direct-slot form and returns a psos-direct-slot-definition.
This doesn't handle multiple readers and writers."
  (let (readers writers documentation initargs initform initformp)
    (loop :for (option-key value) :on options :by #'cddr
	  :do
	  (case option-key
	    (:reader (push value readers))
	    (:writer (push value writers))
	    (:documentation (setf documentation value))
	    (:accessor (push value readers) (push value writers))
	    (:initarg (push value initargs))
	    (:initform (setf initform value
			     initformp t))))
;    (format t "initargs : ~A~%" initargs)
    (make-instance 'psos-direct-slot-definition
		   :readers readers
		   :writers writers
		   :initargs initargs
		   :initform initform
		   :name slot-name
		   :documentation documentation)))

(defun parse-class-options (options)
  options)

(ps:defpsmacro defclass (class-name &optional superclasses direct-slots &rest options)
  (expand-psos-definition
   (let ((psos-class
	  (make-instance 'psos-class-definition
			 :name class-name
			 :options (parse-class-options options)
			 :direct-superclasses superclasses
			 :direct-slot-definitions
			 (mapcar #'(lambda (dslot)
				     (apply #'parse-direct-slot dslot))
				 direct-slots))))
     (mapc #'(lambda (dslot) (setf (psos-slot-class dslot)
				   psos-class))
	   (classdef-direct-slot-definitions psos-class))
     psos-class)))

(ps:defpsmacro defgeneric (formal-name &body options)
  (declare (ignore options))
  (let ((name-as-string (string-downcase (string formal-name))))
    `(defvar ,formal-name (ensure-generic ,formal-name
			   (create :name ,name-as-string)))))

(defun parse-defjsmethod-args (args)
  (let* ((name (first args))
	 (qualifiers? (keywordp (second args)))
	 (qualifiers (if qualifiers? (list (second args)) nil))
	 (post-qualifers (if qualifiers? (cddr args) (cdr args)))
	 (lambda-list (first post-qualifers))
	 (body (rest post-qualifers)))
    (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux? aux
				    more? more-context more-count beyond-requireds? key-object)
	(parenscript::parse-lambda-list-like-thing lambda-list)
      (declare (ignore allow? aux? aux more? more-context more-count beyond-requireds?))

      (labels ((specializer-part (arg-form)
		 (if (listp arg-form) (second arg-form) nil))
	       (name-part (arg-form)
		 (if (listp arg-form) (first arg-form) arg-form)))
	(let ((specializers (mapcar #'specializer-part requireds))
	      (normal-lambda-list
	       (append (mapcar #'name-part requireds)
		       (when optionals
			 (apply #'list '&optional optionals))
		       (when rest?
			 (list '&rest rest))
		       (when (or keys? key-object)
			 (apply #'list '&key keys))
		       (when key-object
			 (list '&key-object key-object)))))
	  (values name qualifiers specializers
		  normal-lambda-list body))))))

(ps:defpsmacro defmethod (&rest args)
  (multiple-value-bind (formal-name method-qualifiers specializers argument-list body)
      (parse-defjsmethod-args args)
    ;(format t "~%DEFMETHOD ARGS: ~A ~% and resultant argument list: ~A ~%" args argument-list)
    (let* ((name-as-string (string-downcase (string formal-name)))
	   (result
	    `(progn
	       (defvar ,formal-name (ensure-generic ,formal-name (create :name ,name-as-string)))
	       (ensure-method ,formal-name (array ,@specializers)
			      (lambda ,argument-list
				,@body)
			      ,@(if (first method-qualifiers)
				    (list (ps:symbol-to-js-string (first method-qualifiers))))))))
      ;(format t "Result of transform: ~S~%" result)
      result)))

(ps:defpsmacro call-next-method (&rest args)
  `((slot-value this 'call-following-method)
    ,@args))