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
    (compile-and-evaluate-js "var window = this;")
    (compile-and-evaluate-js (system-js))
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

(deftest test-slot-inheritance ()
  (ps-forms-equal-lisp-forms
    (3 (progn
         (defclass doggy () 
           ((x :initarg :x :initform 3 :accessor doggy-x)))
         
         (defclass poodle (doggy) 
           ((x :initarg :x :initform 4 :accessor doggy-x)))

         (doggy-x (make-instance 'doggy))))
    (4 (progn
         (doggy-x (make-instance 'poodle))))))

(deftest test-initialize-instance-initargs ()
  (ps-forms-equal-lisp-forms
    (5 (progn
         (doggy-x (make-instance 'doggy :x 5))))))

(deftest test-return-value-is-primary ()
  (ps-forms-equal-lisp-forms
    (1 (progn
         (defgeneric my-method2 ())
         (defmethod my-method2 ((x doggy))
                    (return 1))
         (defmethod my-method2  :after ((x doggy))
                    (return 2))
         (my-method2 (make-instance 'doggy :x 1))))))

(deftest test-redefined-method-replace-old-ones ()
  (ps-forms-equal-lisp-forms
    (3 (progn
         (defgeneric my-method (x))
         (defmethod my-method (x)
           (return 3))
         (my-method 8)))
    (4 (progn
         (defmethod my-method (x)
           (return 4))
         (my-method 8)))
    (5 (progn
         (defmethod my-method ((x doggy))
           (return 5))
         (my-method (make-instance 'doggy))))
    (6 (progn
         (defmethod my-method ((x doggy))
           (return 6))
         (my-method (make-instance 'doggy))))
    (6 (progn
         (defmethod my-method :after ((x doggy))
           (return 7))
         (my-method (make-instance 'doggy))))))

(deftest test-reinitialize-instance ()
  "Ensures that initialize-instance only calls initforms when run for
the first time.  We should really use reinitialize-instance for this
behavior but this is the current hack."
  (ps-forms-equal-lisp-forms
    (3 (progn
         (defclass yarn ()
           ((length :initform 3 :initarg :length :accessor yarn-length)))
         (yarn-length (make-instance 'yarn))))
    (4 (progn
         (let ((y (make-instance 'yarn)))
           (setf (yarn-length y) 4)
           (yarn-length y))))
    (5 (progn
         (let ((y (make-instance 'yarn)))
           (setf (yarn-length y) 5)
           (initialize-instance y)
           (yarn-length y))))))
