;;; -*- Lisp -*- mode
(defpackage #:org.iodb.paren-psos-system
  (:use #:cl #:asdf))

(in-package :org.iodb.paren-psos-system)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :paren-psos
  :description "ParenScript Object System - A CLOS-like object system for ParenScript."
  :version "0.2.1"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:module "src"
                        :components 
			((:file "packages")
			 ;; (:file "rjson" :depends-on ("packages"))
			 (:file "lisp-integration" :depends-on ("packages"))
			 (:file "net-transmit" :depends-on ("lisp-integration"))
			 ;; (:file "parse-lambda-list" :depends-on ("packages"))
			 (:file "util-macrology" :depends-on ("lisp-integration"))
			 (:file "psos-macrology" :depends-on ("util-macrology" "lisp-integration"))
			 
			 (:module "paren"
				  :components
				  ((:parenscript-file "package")
				   (:parenscript-file "psos" :depends-on ("package")))))))
	       
  :depends-on ("parenscript" "closer-mop" "paren-util" "rjson"))

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
