(in-package org.iodb.paren-psos)
;;; This File Includes Utilities For Integrating Clos Classes And PSOS classes
;;; CLOS and PSOS are not identical systems, and PSOS is not refined, so beware

;; to be specific, CLOS expresses more in first-order objects than does PSOS
;; generic functions, methods, and method combinations will probably be supported
;; as first-order objects in the PSOS runtime in the future because they are already
;; expressed in the runtime

(defvar *include-documentation* nil)

(defclass psos-class-definition ()
  ((name
    :accessor classdef-name
    :initarg :name
    :documentation "Name of class definition as a symbol")
   (direct-superclasses
    :accessor classdef-direct-superclasses
    :initarg :direct-superclasses
    :documentation "A list of psos-class designators, either symbols or class definitions.")
   (direct-slot-definitions
    :accessor classdef-direct-slot-definitions
    :initarg :direct-slot-definitions)
   (options
    :accessor classdef-options
    :initarg :options
    :documentation "options attached to the class definition")
   (documentation
    :accessor classdef-documentation
    :initarg :documentation
    :initform nil))
  (:documentation "A class definition in the parenscript object system."))

(defclass psos-direct-slot-definition ()
  ((name
    :accessor psos-slot-name
    :initarg :name
    :documentation "Name of class slot definition as a symbol")
   (initform
    :initform nil
    :initarg :initform
    :accessor psos-slot-initform
    :documentation "Parenscript form evaluated to initialize the slot value")
   (initargs
    :initform nil
    :initarg :initargs
    :accessor psos-slot-initargs
    :documentation "Initialization argument forms.")
   (class
    :accessor psos-slot-class
    :initarg :class
    :documentation "class definition associated with this slot.")
   (readers
    :accessor psos-slot-readers
    :initarg :readers :initform nil
    :documentation "readers asssociated with this slot.")
   (writers
    :accessor psos-slot-writers
    :initarg :writers :initform nil
    :documentation "writers asssociated with this slot.")
   (documentation
    :accessor psos-slot-documentation
    :initarg :documentation
    :initform nil))
  (:documentation "A class definition in the parenscript object system."))

(defgeneric expand-psos-definition (psos-definition &key &allow-other-keys)
  (:documentation "This is poorly named."))

#+nil
(defgeneric lisp-to-psos-slot-definition (lisp-slot class)
  (:documentation "Converts a lisp slot definition to a parenscript one."))

#+nil
(defmethod lisp-to-psos-slot-definition ((lisp-slot closer-mop:effective-slot-definition) class)
  nil)

#+nil
(defmethod lisp-to-psos-slot-definition ((lisp-slot closer-mop:direct-slot-definition) class)
  (make-instance 'psos-direct-slot-definition
		 :readers (closer-mop:slot-definition-initargs
			   :initargs (closer-mop:slot-definition-initargs lisp-slot)
			   :name (closer-mop:slot-definition-name lisp-slot))))
  

(defmethod expand-psos-definition ((class-definition psos-class-definition)
                                   &key extra-superclasses &allow-other-keys)
  "Expands a class definition to a parenscript form."
  (with-accessors ((class-name classdef-name) 
		   (slot-definitions classdef-direct-slot-definitions)
		   (options classdef-options)
		   (documentation classdef-documentation))
    class-definition
    (let ((superclasses (append (classdef-direct-superclasses class-definition)  extra-superclasses)))
      `(progn
         ;; was defvar but changed to setf so it affects the global situation
         (defvar ,class-name (ensure-class
                              ,class-name
                              :name ,(string-downcase (string class-name))
                              ,@(when superclasses
                                  (list :direct-superclasses `(array ,@superclasses)))
                              :slot-definitions nil ; placeholder for true slot stuff
                              :initarg-map
                              (create
                               ,@(mapcan 
                                  #'(lambda (slot-def)
                                      (mapcar 'parenscript:symbol-to-js-string
                                              (mapcan #'(lambda (init-arg)
                                                          (list init-arg (psos-slot-name slot-def)))
                                                      (psos-slot-initargs slot-def))))
                                  slot-definitions))
                              :initform-fn
                              ,(when (remove nil slot-definitions :key 'psos-slot-initform)
                                 (let ((object-var (gensym "self_obj")))
                                   `(lambda (,object-var)
                                      ,@(mapcar #'(lambda (slot-def)
                                                    `(defaultf (slot-value ,object-var ',(psos-slot-name slot-def))
                                                         ,(psos-slot-initform slot-def)))
                                                slot-definitions))))
                              ,@(apply #'append options)))
         (setf (slot-value *classes* ',class-name) ,class-name)
         ,@(mapcar #'(lambda (slot-def)
                       (expand-psos-slot-definition class-name slot-def))
                   slot-definitions)))))
  

(defun js-format-symbol (format-string &rest format-args)
  (intern (string-upcase
	   (apply #'format nil format-string format-args))))

;(defun expand-psos-slot-definition (slot-

(defgeneric expand-psos-slot-definition (class-name slot-def)
  (:method (class-name (slot-definition psos-direct-slot-definition))
    (let ((accessors-output-convention :lisp))
      (with-accessors ((slot-name psos-slot-name) (slot-class psos-slot-class)
		       (readers psos-slot-readers) (writers psos-slot-writers)
		       (documentation psos-slot-documentation))
	slot-definition
	(case accessors-output-convention
	  ((or :clos :lisp) ;this is not implemented because setf is strange in parenscript
	   `(progn
	     ,@(mapcar #'(lambda (reader)
			   `(defmethod ,reader ((obj ,class-name))
			     (return (slot-value obj ',slot-name))))
		       readers)
	     ,@(mapcar #'(lambda (writer)
			 `(defsetf ,writer (object) (new-value)
			   (let ((slot-name ',slot-name))
			     `(setf (slot-value ,object ',slot-name) ,new-value))))
		       writers))))))))
;	  (:java
;	   (let ((slot-form
;		  `(slot-value obj
;		    (quote ,(js-format-symbol "~A" slot-name)))))
;	     `(progn
;	       (defmethod ,(js-format-symbol "get-~A" slot-name)
;		 ((obj ,(classdef-name slot-class)))
;	       (return ,slot-form))
;	     (defmethod ,(js-format-symbol "set-~A" slot-name)
;		 ((obj ,(classdef-name slot-class)) value)
;	       (return (setf ,slot-form value)))) ))))))


(defclass psos-class-import ()
  ((imported-class :accessor imported-class :initarg :imported-class)
   (alias :accessor psos-import-alias :initarg :alias
	  :documentation "Symbol of the clos class mapped to the lisp imported-class"))
   (:documentation "A class import made by a some parenscript. NOT USED RIGHT NOW"))

(defgeneric import-lisp-class (lisp-class &key alias)
  (:documentation "Returns a psos-class-definition corresponding to the given lisp class."))
(defgeneric import-lisp-slot-definition (slot-definition)
  (:documentation "Returns a psos-direct-slot-definition corresponding to the given lisp slot definition."))

(defmethod import-lisp-class ((lisp-class standard-class) &key alias)
  (let ((class-definition
	 (make-instance 'psos-class-definition
			:name (or alias (class-name lisp-class))
			:options nil
			:direct-superclasses (mapcar #'(lambda (class) (class-name class))
						     (closer-mop:class-direct-superclasses lisp-class))
			:direct-slot-definitions (mapcar #'import-lisp-slot-definition
							 (closer-mop:class-direct-slots lisp-class))
			:documentation (documentation lisp-class t))))
    (mapc #'(lambda (dslot) (setf (psos-slot-class dslot)
				  class-definition))
	  (classdef-direct-slot-definitions class-definition))
    class-definition))

(defmethod import-lisp-slot-definition ((slot-definition closer-mop:standard-direct-slot-definition))
  (make-instance 'psos-direct-slot-definition
		  :name (closer-mop:slot-definition-name slot-definition)
		  :initargs (closer-mop:slot-definition-initargs slot-definition)
		  :class nil
		  :readers (closer-mop:slot-definition-readers slot-definition)
		  :writers nil
		  :documentation (documentation slot-definition t)))

(ps:defpsmacro import-class (class-name &key alias extra-superclasses alloc-fn init-fn construct-fn)
  `(progn
     ,(expand-psos-definition
       (import-lisp-class (find-class class-name) :alias alias)
       :extra-superclasses extra-superclasses)
     (rjtype ,(rjson:rjson-type-of-class (find-class class-name))
             :construct-fn ,construct-fn
	     :alloc-fn ,(or alloc-fn `(rjson-alloc-fn ,class-name))
	     :init-fn ,(or init-fn `(rjson-init-fn ,class-name)))))

     