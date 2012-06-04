;;; -*- Lisp -*- mode
(defpackage #:org.iodb.paren-psos-system
  (:use #:cl #:asdf))

(in-package :org.iodb.paren-psos-system)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :paren-psos
  :description "ParenScript Object System - A CLOS-like object system for ParenScript."
  :version "0.2.1"
  :author "Red Daly <reddaly at gmail>"
  :license "MIT License"
  :components ((:module "src"
                        :components 
			((:file "packages")
			 ;; (:file "rjson" :depends-on ("packages"))
			 (:file "lisp-integration" :depends-on ("packages"))
			 ;; (:file "rjson-clos" :depends-on ("lisp-integration"))
			 ;; (:file "parse-lambda-list" :depends-on ("packages"))
			 (:file "util-macrology" :depends-on ("lisp-integration"))
			 (:file "psos-macrology" :depends-on ("util-macrology" "lisp-integration"))
			 (:file "conditions-macrology" :depends-on ("util-macrology" "lisp-integration"))
			 
			 (:module "paren"
				  :components
				  ((:parenscript-file "package")
				   (:parenscript-file "psos" :depends-on ("package"))
                                   (:parenscript-file "paren-conditions" :depends-on ("psos")))))))
	       
  :depends-on ("parenscript" "closer-mop" "paren-util" "rjson"))


(defsystem :paren-psos-test
  :description "Lisp and Parenscript tests for the Parenscript Object System."
  :version "0.2.0"
  :author "Red Daly"
  :license "MIT License"
  :components ((:module "test"
			:components
			((:file "test-package")
             (:file "test-conditions")
                         (:module "paren"
				  :components ((:parenscript-file "psos-test"))))))
  :depends-on ("parenscript" "paren-psos" "cl-spidermonkey" "hu.dwim.stefil"))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :paren-psos))))
  (asdf:operate 'asdf:load-op :paren-psos-test)
  (funcall (intern (symbol-name '#:paren-psos-tests)
                   (find-package '#:psos-tests))))

#+nil
(defsystem :paren-psos-test
  :description "Lisp and Parenscript tests for the Parenscript Object System."
  :version "0.2.0"
  :author "Red Daly"
  :license "GPL version 2"
  :components ((:module "test"
			:components
			((:module "paren-test"
				  :components ((:parenscript-file "psos-test"))))))
  :depends-on ("parenscript" "paren-test" "paren-psos"))
