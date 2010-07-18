(cl:in-package :cl-user)

(defpackage :paren-psos-tests
    (:nicknames :psos-tests)
  (:use :cl :anaphora :hu.dwim.stefil :paren-psos :cl-spidermonkey :parenscript :paren-util)
  (:export #:paren-psos-tests))

(in-package :paren-psos-tests)

(defun system-js ()
  (let ((src (with-output-to-string (s)
               (let ((parenscript.asdf::*omit-implicit-load-op-dependency* t))
                 (paren-files:compile-script-system :paren-psos :output-stream s)))))
    (with-open-file (s "/tmp/paren-psos.js" :direction :output :if-exists :supersede)
      (write-string src s))
    src))

(defsuite paren-psos-tests nil 
  (with-js-context (context)
    (compile-and-evaluate-js "var window = this;" :filename "fakewindow.js")
    (compile-and-evaluate-js (system-js) :filename "/tmp/paren-psos.js")
    (run-child-tests)))

(in-suite paren-psos-tests)

(deftest test-js-context ()
  (is (eql t (compile-and-evaluate-js "true;"))))

(defmacro ps-forms-equal-lisp-forms (&body forms)
  "Forms is a list of (lisp-form ps-form)."
  `(progn
     ,@(mapcar (lambda (form)
                 `(is (equal ,(first form) (evaluate-js (ps:ps ,@(rest form))))))
               forms)))

(deftest test-standard-metaobjects ()
  (ps-forms-equal-lisp-forms
    (t (and (=== standard-object jsns::-Object) t))

    ("standard-generic-function" (slot-value psos::*classes* 'standard-generic-function 'class-name))

    ("standard-class" (slot-value standard-class 'class-name))

    (nil (not (slot-value psos::*classes* 'standard-class)))

    (nil (not (slot-value psos::*classes* 'standard-generic-function)))))

(deftest test-init-order ()
  (ps-forms-equal-lisp-forms
    (t (progn
         (defvar *init-order* (array))
         (defclass thing () ())
         (defclass alive (thing) ())
         (defclass person (alive) ())
         (defclass intellectual (person) ())
         (defclass elitist (person) ())
         (defclass smart-elitist (elitist intellectual) ())
         
         (defun push-init (thing)
           (methcall 'jsns::push *init-order* thing))
         
         (defmethod initialize-instance ((x thing))  (push-init "THING")  (call-next-method))
         (defmethod initialize-instance ((x alive))  (push-init "ALIVE")  (call-next-method))
         (defmethod initialize-instance ((x person)) (push-init "PERSON") (call-next-method))
         (defmethod initialize-instance ((x elitist)) (push-init "ELITIST") (call-next-method))
         (=== thing (find-class 'thing))))

    ("PERSON,ALIVE,THING" (progn
                            (make-instance 'person)
                            (methcall 'jsns::to-string *init-order*)))

    ("ELITIST,PERSON,ALIVE,THING"
     (progn
       (setf *init-order* (array))
       (make-instance 'elitist)
       (methcall 'jsns::to-string *init-order*)))))


(deftest test-class-precedence-order ()
  (ps-forms-equal-lisp-forms
    (t (and  (psos::is-subclass-of thing person) t))

    (t (and  (psos::is-subclass-of thing alive) t))

    (t (and  (psos::is-subclass-of alive person) t))

    (t (and  (psos::is-subclass-of thing smart-elitist)
             (psos::is-subclass-of elitist smart-elitist)
             (psos::is-subclass-of intellectual smart-elitist)
             t))))




