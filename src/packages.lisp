(in-package :cl-user)

(defpackage #:org.iodb.paren-psos
  (:nicknames #:paren-psos #:psos)
  (:documentation "A CLOS-like object system for ParenScript.")
  (:use :common-lisp :parenscript :paren-util :rjson)
  (:export
   #:encode-rjson-string
   ;; hackish
   #:pslog

   ;; parenscript exports
   #:defgeneric
   #:defmethod
   #:defclass
   #:standard-object
   #:standard-class
   #:standard-generic-function
   #:standard-method
   #:make-instance
   #:allocate-instance
   #:initialize-instance
   #:class-name
   #:class-direct-superclasses
   #:class-direct-subclasses
   #:class-precedence-list
   #:class-direct-methods
   #:class-default-direct-superclasses
   #:class-initarg-map
   #:generic-name
   #:class-of
   #:import-class

   #:invoke-restart
   #:restart-case
   #:restart-bind
   #:handler-case
   #:handler-bind
   #:signal))


(in-package org.iodb.paren-psos)

;(defpackage #:org.iodb.paren-psos.js-package
;  (:nicknames #:paren-psos.js-package)
;  (:use :parenscript))
  

